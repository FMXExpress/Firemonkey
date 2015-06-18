unit uChatManager;

interface

uses
  System.Classes, System.Bluetooth;

type
  TTextEvent = procedure (const Sender: TObject; const AText: string; const aDeviceName: string) of object;

  TReadThread = class(TThread)
  private
    FSocket: TBluetoothServerSocket;
    FOnTextReceived: TTextEvent;
    FName: string;
    procedure SetOnTextReceived(const Value: TTextEvent);
  public
    constructor Create(const Socket: TBluetoothServerSocket; const Name: string); overload;
    procedure Execute; override;
    property OnTextReceived: TTextEvent read FOnTextReceived write SetOnTextReceived;
  end;

  TWriteThread = class(TThread)
  private
    FSocket: TBluetoothSocket;
    FOnTextSent: TTextEvent;
    FName: string;
    FLock: TObject;
    FStringsToWrite: TStringList;
    procedure SetOnTextSent(const Value: TTextEvent);
  public
    constructor Create(const Socket: TBluetoothSocket; const Name: string); overload;
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

const
  BTChat_UUID: TGUID = '{98E630C2-E1E1-40BE-A31F-BB0F0EEB00B8}';

{ TWriteThread }

constructor TWriteThread.Create(const Socket: TBluetoothSocket; const Name: string);
begin
  FSocket := Socket;
  FName := Name;
  FLock := TObject.Create;
  FStringsToWrite := TStringList.Create;
  Create;
end;

destructor TWriteThread.Destroy;
begin
  FStringsToWrite.Free;
  FLock.Free;
  inherited;
end;

procedure TWriteThread.Execute;
var
  I: Integer;
  LFailed: Boolean;
begin
  inherited;
  While not Terminated do
  begin
    if FStringsToWrite.Count > 0 then
    begin
      if not FSocket.Connected then
        FSocket.Connect;
      TMonitor.Enter(FLock);
      LFailed := False;
      try
        for I := 0 to FStringsToWrite.Count - 1 do
        begin
          try
            FSocket.SendData(TEncoding.UTF8.GetBytes(FStringsToWrite[I]));
            if Assigned(FOnTextSent) then
              FOnTextSent(Self, FStringsToWrite[I], FName);
          except
            LFailed := True;
            Break;
          end;
        end;
        if not LFailed then
          FStringsToWrite.Clear;
      finally
        TMonitor.Exit(FLock);
        if LFailed then
          Terminate;
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

constructor TReadThread.Create(const Socket: TBluetoothServerSocket; const Name: string);
begin
  FSocket := Socket;
  FName := Name;
  Create;
end;

procedure TReadThread.Execute;
var
  LBuffer: TBytes;
  LReadSocket: TBluetoothSocket;
  LReset: Boolean;
begin
  inherited;
  Setlength(LBuffer,0);
  while not Terminated do
  begin
    LReadSocket := FSocket.Accept(3000);
    if LReadSocket <> nil then
    begin
      LReset := False;
      While (not Terminated) and LReadSocket.Connected and (not LReset) do
      begin
        try
          LBuffer := LReadSocket.ReadData;
          if (Length(LBuffer) > 0) and Assigned(FOnTextReceived) then
          begin
            FOnTextReceived(Self, TEncoding.UTF8.GetString(Lbuffer), Fname);
            Setlength(LBuffer,0);
          end;
          Sleep(500);
        except
          LReset := True;
        end;
      end;
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
var
  LSocket: TBluetoothServerSocket;
  LClientSocket: TBluetoothSocket;
begin
  LSocket := FManager.CurrentAdapter.CreateServerSocket('ChatReadSocket', BTChat_UUID, True);
  LClientSocket := FCurrentDevice.CreateClientSocket(BTChat_UUID, True);
  FReadThread := TReadThread.Create(LSocket, FCurrentDevice.DeviceName);
  FReadThread.OnTextReceived := FOnTextReceived;
  FWriteThread := TWriteThread.Create(LClientSocket, ' me ');
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
