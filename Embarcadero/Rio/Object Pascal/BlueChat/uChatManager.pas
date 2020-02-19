//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit uChatManager;

interface

uses
  System.Classes, System.Bluetooth;

type
  TTextEvent = procedure (const Sender: TObject; const AText: string; const aDeviceName: string) of object;

  TReadThread = class(TThread)
  private
    FOnTextReceived: TTextEvent;
    FName: string;
    FManager: TBluetoothManager;
    FServerGUID: TGUID;
    procedure SetOnTextReceived(const Value: TTextEvent);
  public
    constructor Create(const Manager: TBluetoothManager; const Name: string; const AGUID: TGUID); overload;
    destructor Destroy; override;
    procedure Execute; override;
    property OnTextReceived: TTextEvent read FOnTextReceived write SetOnTextReceived;
  end;

  TWriteThread = class(TThread)
  private
    FOnTextSent: TTextEvent;
    FName: string;
    FLock: TObject;
    FStringsToWrite: TStringList;
    FDevice: TBluetoothDevice;
    FGUID: TGUID;
    procedure SetOnTextSent(const Value: TTextEvent);
  public
    constructor Create(const Device: TBluetoothDevice; const Name: string; const AGUID: TGUID); overload;
    destructor Destroy; override;
    procedure SendText(const AText: string);
    procedure Execute; override;
    property OnTextSent: TTextEvent read FOnTextSent write SetOnTextSent;
  end;

  TChatManager = class
  private
    FManager: TBluetoothManager;
    FDiscoveredDevices: TBluetoothDeviceList;
    FPairedDevices: TBluetoothDeviceList;
    FReadThread: TReadThread;
    FWriteThread: TWriteThread;
    FOnTextSent: TTextEvent;
    FOnTextReceived: TTextEvent;
    FOnDiscoveryEnd: TDiscoveryEndEvent;
    FSelectedDevice: Integer;
    FCurrentDevice: TBluetoothDevice;
    FServerGUID: TGUID;
    FClientGUID: TGUID;
    procedure DiscoveryEnd(const Sender: TObject; const ADeviceList: TBluetoothDeviceList);
    procedure SetOnTextReceived(const Value: TTextEvent);
    procedure SetOnTextSent(const Value: TTextEvent);
    procedure SetSelectedDevice(const Value: Integer);
    procedure SetCurrentDevice(const Value: TBluetoothDevice);
    function GetKnownDevices: TBluetoothDeviceList;
    procedure TeminateThreads;
    procedure InitializeThreads;
    procedure RecreateThreads;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SendText(const AText: string);
    procedure PairTo(const ADevice: string);
    procedure DiscoverDevices;
    function HasBluetoothDevice: boolean;
    property SelectedDevice: Integer read FSelectedDevice write SetSelectedDevice;
    property CurrentDevice: TBluetoothDevice read FCurrentDevice write SetCurrentDevice;
    property OnTextReceived: TTextEvent read FOnTextReceived write SetOnTextReceived;
    property OnTextSent: TTextEvent read FOnTextSent write SetOnTextSent;
    property OnDiscoveryEnd: TDiscoveryEndEvent read FOnDiscoveryEnd write FOnDiscoveryEnd;
    property KnownDevices: TBluetoothDeviceList read GetKnownDevices;
  end;

implementation

uses
  System.SysUtils, FMX.Dialogs;


{ TWriteThread }

constructor TWriteThread.Create(const Device: TBluetoothDevice; const Name: string; const AGUID: TGUID);
begin
  FDevice := Device;
  FGUID := AGUID;
  FName := Name;
  FLock := TObject.Create;
  FStringsToWrite := TStringList.Create;
  Create;
end;

destructor TWriteThread.Destroy;
begin
  FStringsToWrite.Free;
  FLock.Free;
  FDevice := nil;
  inherited;
end;

procedure TWriteThread.Execute;
var
  I: Integer;
  LFailed: Boolean;
  LBytesToSend: TBytes;
  LClientSocket: TBluetoothSocket;
begin
  inherited;
  While not Terminated do
  begin
    if FStringsToWrite.Count > 0 then
    begin
      FDevice.GetServices;
      LClientSocket := FDevice.CreateClientSocket(FGUID, False);
      try
        LClientSocket.Connect;
//        if LClientSocket.Connected then
        begin
          TMonitor.Enter(FLock);
          LFailed := False;
          try
            for I := 0 to FStringsToWrite.Count - 1 do
            begin
              try
                LBytesToSend := TEncoding.UTF8.GetBytes(FStringsToWrite[I]);
                LClientSocket.SendData(LBytesToSend);
                if Assigned(FOnTextSent) then
                  FOnTextSent(Self, FStringsToWrite[I], FName);
              except
                LFailed := True;
                Break;
              end;
            end;
            if not LFailed then
              FStringsToWrite.Clear
            else if Assigned(FOnTextSent) then
              FOnTextSent(Self, FStringsToWrite[I] + ' "failed to send"', FName);
          finally
            TMonitor.Exit(FLock);
          end;
        end;
      finally
        LClientSocket.Close;
        FreeAndNil(LClientSocket);
      end;
    end;
    Sleep(500);
  end;
end;

procedure TWriteThread.SendText(const AText: string);
begin
  TMonitor.Enter(FLock);
  try
    FStringsToWrite.Add(AText);
  finally
    TMonitor.Exit(FLock);
  end;
end;

procedure TWriteThread.SetOnTextSent(const Value: TTextEvent);
begin
  FOnTextSent := Value;
end;

{ TReadThread }

constructor TReadThread.Create(const Manager: TBluetoothManager; const Name: string; const AGUID: TGUID);
begin
  FServerGUID := AGUID;
  FManager := Manager;
  FName := Name;
  Create;
end;

destructor TReadThread.Destroy;
begin
  FManager := nil;
  inherited;
end;

procedure TReadThread.Execute;
const
  TIMEOUT = 100;
var
  LBuffer: TBytes;
  LReadSocket: TBluetoothSocket;
  LServerSocket: TBluetoothServerSocket;
begin
  inherited;
  Setlength(LBuffer,0);
  LServerSocket := FManager.CurrentAdapter.CreateServerSocket('ChatReadSocket ' + FManager.CurrentAdapter.AdapterName, FServerGUID, False);
  LReadSocket := nil;
  while not Terminated do
  begin
    try
      while not Terminated and (LReadSocket = nil) do
      begin
        LReadSocket := LServerSocket.Accept(TIMEOUT);
        sleep(TIMEOUT);
      end;
      if (LReadSocket <> nil) and (LReadSocket.Connected) then
      begin
        While not Terminated and LReadSocket.Connected do // and LReadSocket.Connected  do
        begin
          LBuffer := LReadSocket.ReadData;
          if (Length(LBuffer) > 0) and Assigned(FOnTextReceived) then
          begin
            FOnTextReceived(Self, TEncoding.UTF8.GetString(Lbuffer), FName);
            Setlength(LBuffer,0);
          end;
          Sleep(TIMEOUT);
        end;
      end;
      FreeAndNil(LReadSocket);
    except
      FreeAndNil(LReadSocket);
    end;
  end;
end;

procedure TReadThread.SetOnTextReceived(const Value: TTextEvent);
begin
  FOnTextReceived := Value;
end;

{ TChatManager }

constructor TChatManager.Create;
begin
  FManager := TBluetoothManager.Current;
  FManager.OnDiscoveryEnd := DiscoveryEnd;
  SelectedDevice := -1;
end;

destructor TChatManager.Destroy;
begin
  TeminateThreads;
  inherited;
end;

procedure TChatManager.DiscoverDevices;
begin
  FManager.OnDiscoveryEnd := DiscoveryEnd;
  FManager.StartDiscovery(5000);
end;

procedure TChatManager.DiscoveryEnd(const Sender: TObject; const ADeviceList: TBluetoothDeviceList);
begin
  if Assigned(FOnDiscoveryEnd) then
    FOnDiscoveryEnd(Sender, ADeviceList);
end;

function TChatManager.GetKnownDevices: TBluetoothDeviceList;
begin
  if  FManager.CurrentAdapter <> nil then
    Result := FManager.CurrentAdapter.PairedDevices
  else
    Result := nil;
end;

function TChatManager.HasBluetoothDevice: boolean;
begin
  Result := FManager.CurrentAdapter <> nil;
end;

procedure TChatManager.SendText(const AText: string);
begin
  if FWriteThread <> nil then
  begin
    if FWriteThread.Terminated then
      RecreateThreads;
    FWriteThread.SendText(AText)
  end
  else
    raise Exception.Create('You are not connected to a device');
end;

procedure TChatManager.RecreateThreads;
begin
  TeminateThreads;
  InitializeThreads;
end;

procedure TChatManager.InitializeThreads;

  function CreateGUIDFromName(const AName: string): TGUID;
  var
    LStringGUID: string;
    LGUIDLength: Integer;
    I: Integer;
  begin
    LGUIDLength := TGUID.Empty.ToString.Length - 2;
    if AName.Length > LGUIDLength then
      LStringGUID := AName.Substring(0, LGUIDLength)
    else
      LStringGUID := AName + string.Create('F', LGUIDLength - AName.Length);

    LStringGUID := UpperCase(LStringGUID);
    for I := Low(LStringGUID) to High(LStringGUID) do
    begin
      if not (LStringGUID[I] in ['0'..'9', 'A'..'F']) then
      begin
        if LStringGUID[I] < 'A' then
          LStringGUID := LStringGUID.Replace(LStringGUID[I], 'A')
        else
          LStringGUID := LStringGUID.Replace(LStringGUID[I], 'F')
      end;
    end;
    LStringGUID[low(string) + 8] := '-';
    LStringGUID[low(string) + 13] := '-';
    LStringGUID[low(string) + 18] := '-';
    LStringGUID[low(string) + 23] := '-';
    Result := TGUID.Create('{' + LStringGUID + '}');
  end;
begin
  FServerGUID := CreateGUIDFromName(FManager.CurrentAdapter.AdapterName);
  FClientGUID := CreateGUIDFromName(FCurrentDevice.DeviceName);

  FReadThread := TReadThread.Create(FManager, FCurrentDevice.DeviceName, FServerGUID);
  FReadThread.OnTextReceived := FOnTextReceived;

  FWriteThread := TWriteThread.Create(FCurrentDevice, ' me ', FClientGUID);
  FWriteThread.OnTextSent := FOnTextSent;
end;

procedure TChatManager.PairTo(const ADevice: string);
var
  I: Integer;
  LDevice: TBluetoothDevice;
begin
  for I := 0 to FManager.LastDiscoveredDevices.Count - 1 do
    if FManager.LastDiscoveredDevices[I].DeviceName = ADevice then
    begin
      LDevice := FManager.LastDiscoveredDevices[I];
      Break;
    end;
  if LDevice <> nil then
    if not LDevice.IsPaired then
      FManager.CurrentAdapter.Pair(LDevice)
end;

procedure TChatManager.TeminateThreads;
begin
  if FReadThread <> nil then
  begin
    FReadThread.Terminate;
    FReadThread.Free;
  end;
  if FWriteThread <> nil then
  begin
    FWriteThread.Terminate;
    FWriteThread.Free;
  end;
end;

procedure TChatManager.SetCurrentDevice(const Value: TBluetoothDevice);
begin
  FCurrentDevice := Value;
  FCurrentDevice.GetServices;
  RecreateThreads;
end;

procedure TChatManager.SetOnTextReceived(const Value: TTextEvent);
begin
  FOnTextReceived := Value;
  if FReadThread <> nil then
    FReadThread.OnTextReceived := Value;
end;

procedure TChatManager.SetOnTextSent(const Value: TTextEvent);
begin
  FOnTextSent := Value;
  if FWriteThread <> nil then
    FWriteThread.OnTextSent := Value;
end;

procedure TChatManager.SetSelectedDevice(const Value: Integer);
begin
  if (Value >= 0) and (FManager.CurrentAdapter.PairedDevices.Count > Value) then
  begin
    FSelectedDevice := Value;
    if Value >= 0 then
      CurrentDevice := TBluetoothDevice(FManager.CurrentAdapter.PairedDevices.Items[Value]);
  end;
end;

end.
