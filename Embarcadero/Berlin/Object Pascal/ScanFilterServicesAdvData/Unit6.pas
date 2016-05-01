//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit Unit6;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Bluetooth, FMX.Layouts, FMX.ListBox, FMX.StdCtrls, System.StrUtils,
  System.Bluetooth.Components, FMX.Controls.Presentation, System.Generics.Collections;

const
  ScanningTime = 5000;
  // 16bits services standard
  // https://developer.bluetooth.org/gatt/services/Pages/ServicesHome.aspx
  HeartRate = $180D;
  HealthThermometer = $1809;
  DeviceInformation = $180A;
  // 128bits services standard --> {xxxxxxxx-0000-1000-8000-00805F9B34FB}
  // 32bits services standar --> 0000xxxx
  // 16bits services standard --> https://developer.bluetooth.org/gatt/services/Pages/ServicesHome.aspx
  //Guid128bStandard = '00001000800000805F9B34FB';
   Guid128bStandard = '{0000xxxx-0000-1000-8000-00805F9B34FB}';

type
  TForm6 = class(TForm)
    Panel1: TPanel;
    BluetoothLE1: TBluetoothLE;
    Button1: TButton;
    Button2: TButton;
    ListBox1: TListBox;
    CheckBox1: TCheckBox;
    procedure BluetoothLE1EndDiscoverDevices(const Sender: TObject;
      const ADeviceList: TBluetoothLEDeviceList);
    procedure BluetoothLE1DiscoverLEDevice(const Sender: TObject;
      const ADevice: TBluetoothLEDevice; Rssi: Integer;
      const ScanResponse: TScanResponse);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    DevicesAdvDataFiltered: TBluetoothLEDeviceList;
    DevicesAdvDataNoFiltered: TBluetoothLEDeviceList;
    FFilterUUIDList: TList<Word>;
    FForceConnectDevices: Boolean;
    StringRawGuid128bStandard: string;
    function AddDeviceToList(const ADevice: TBluetoothLEDevice; const ADeviceList: TBluetoothLEDeviceList): TBluetoothLEDevice;
    procedure PrintLine();
  public
    { Public declarations }
    property ForceConnectDevices: Boolean read FForceConnectDevices write FForceConnectDevices;
  end;

var
  Form6: TForm6;

type
  TServicesLengthType = (S16B = 2, S32B = 4, S128B = 16);

implementation

{$R *.fmx}

procedure BinToHex(Buffer: PByte; Text: PWideChar; BufSize: Integer);
const
  Convert: array[0..15] of WideChar = '0123456789ABCDEF';
var
  I: Integer;
begin
  {$IF defined(ANDROID)}
    Inc(Text, (BufSize - 1)*2);
    for I := 0 to BufSize - 1 do
    begin
      Text[0] := Convert[Buffer[I] shr 4];
      Text[1] := Convert[Buffer[I] and $F];
      Dec(Text, 2);
    end;
  {$ELSE}
    for I := 0 to BufSize - 1 do
    begin
      Text[0] := Convert[Buffer[I] shr 4];
      Text[1] := Convert[Buffer[I] and $F];
      Inc(Text, 2);
    end;
  {$ENDIF}
end;

function TForm6.AddDeviceToList(const ADevice: TBluetoothLEDevice;
      const ADeviceList: TBluetoothLEDeviceList): TBluetoothLEDevice;

  function GetDeviceInList(const AIdentifier: string; const ADeviceList: TBluetoothLEDeviceList): TBluetoothLEDevice;
  var
    LDevice: TBluetoothLEDevice;
  begin
    Result := nil;
    for LDevice in ADeviceList do
    begin
      if LDevice.Identifier = AIdentifier then
      begin
        Result := LDevice;
        Break;
      end;
    end;
  end;

begin
  Result := GetDeviceInList(ADevice.Identifier, ADeviceList);
  if Result = nil then
  begin
    ADeviceList.Add(ADevice);
    Result := ADevice;
  end;
end;

procedure TForm6.PrintLine;
var
  Name: string;
  NumberOfDevices: Integer;
  I: Integer;
begin
NumberOfDevices := Listbox1.Count;
  for I  := 0 to DevicesAdvDataFiltered.Count - 1 do
  begin
    Name := DevicesAdvDataFiltered[I].DeviceName;
    if Name = '' then
      Name := 'Unknown device';
    Name := ' - '+ Name + ' - ' + DevicesAdvDataFiltered[I].Identifier;
    if NumberOfDevices = I then
      Listbox1.Items.Add((NumberOfDevices + 1).ToString+Name)
    else
      Listbox1.Items[I] := (I + 1).ToString+Name;
  end;
end;

procedure TForm6.BluetoothLE1DiscoverLEDevice(const Sender: TObject;
  const ADevice: TBluetoothLEDevice; Rssi: Integer;
  const ScanResponse: TScanResponse);

  function ChekBLEServices(const AData: TBytes; AServicesLengthType: TServicesLengthType): Boolean;
  var
    GUID: string;
    LDataLength: Integer;
    I, B: Integer;
    LDeviation: Integer;
    Service: Word;
  begin
    case Integer(AServicesLengthType) of
    2: LDeviation := 0;
    4: LDeviation := 2;
    16: begin // we just can have one Service in format 128 bits
          Setlength(GUID, 32);
          BinToHex(PByte(AData), PChar(GUID),16);          
          if not GUID.Contains(StringRawGuid128bStandard) then
             Exit(False);
          GUID := GUID.Substring(4,4);   
          B := 0;
          while B < FFilterUUIDList.Count do        
          begin
            if GUID = FFilterUUIDList[B].ToHexString then
            begin
              AddDeviceToList(ADevice, DevicesAdvDataFiltered);
              Exit(True);
            end;
            Inc(B);
          end;
          Exit(False); 
        end;
    end;

    LDataLength := Length(AData);
    I := 0;
    while I < LDataLength  do
    begin
     {$IF defined(ANDROID)} //0D18 - 0D180000
        WordRec(Service).Hi := AData[I + LDeviation + 1];
        WordRec(Service).Lo := AData[I + LDeviation];
     {$ELSE} //180D
        WordRec(Service).Hi := AData[I + LDeviation];
        WordRec(Service).Lo := AData[I + LDeviation + 1];
      {$ENDIF}
      B := 0;
      while B < FFilterUUIDList.Count do        
      begin
        if Service = FFilterUUIDList[B] then
        begin
          AddDeviceToList(ADevice, DevicesAdvDataFiltered);
          Exit(True);
        end;
        Inc(B);
      end;
      
      Inc(I, Integer(AServicesLengthType)); //2, 4, 16
    end;
  end;

  function AdvDataServiceFilter(): Boolean;
  begin
    Result := False;
    if ScanResponse.ContainsKey(TScanResponseKey.IncompleteList16SCUUID) then
    begin
      Result := True;
      if ChekBLEServices(ScanResponse.Items[TScanResponseKey.IncompleteList16SCUUID], TServicesLengthType.S16B) then
        Exit(True);
    end;

    if ScanResponse.ContainsKey(TScanResponseKey.CompleteList16SCUUID) then
    begin
      Result := True;
      if ChekBLEServices(ScanResponse.Items[TScanResponseKey.CompleteList16SCUUID], TServicesLengthType.S16B) then
        Exit(True);
    end;

    if ScanResponse.ContainsKey(TScanResponseKey.IncompleteList32SCUUID) then
    begin
      Result := True;
      if ChekBLEServices(ScanResponse.Items[TScanResponseKey.IncompleteList32SCUUID], TServicesLengthType.S32B) then
        Exit(True);
    end;

    if ScanResponse.ContainsKey(TScanResponseKey.CompleteList32SCUUID) then
    begin
      Result := True;
      if ChekBLEServices(ScanResponse.Items[TScanResponseKey.CompleteList32SCUUID], TServicesLengthType.S32B) then
        Exit(True);
    end;

    if ScanResponse.ContainsKey(TScanResponseKey.IncompleteList128SCUUID) then
    begin
      Result := True;
      if ChekBLEServices(ScanResponse.Items[TScanResponseKey.IncompleteList128SCUUID], TServicesLengthType.S128B) then
        Exit(True);
    end;

    if ScanResponse.ContainsKey(TScanResponseKey.CompleteList128SCUUID) then
    begin
      Result := True;
      if ChekBLEServices(ScanResponse.Items[TScanResponseKey.CompleteList128SCUUID], TServicesLengthType.S128B) then
        Exit(True);
    end;
  end;

begin
  if not AdvDataServiceFilter then
  begin
    if ForceConnectDevices then
    begin
      if DevicesAdvDataNoFiltered = nil then
        DevicesAdvDataNoFiltered := TBluetoothLEDeviceList.create(False);
      AddDeviceToList(ADevice, DevicesAdvDataNoFiltered);
    end;
  end;

  PrintLine;
end;

procedure TForm6.BluetoothLE1EndDiscoverDevices(const Sender: TObject;
  const ADeviceList: TBluetoothLEDeviceList);

  function MatchAnyService(const ADevice: TBluetoothLEDevice): Boolean;
  var
    I: Integer;
    GUID: string;
  begin
    Result := False;
    Guid := Guid128bStandard;
    if ADevice.DiscoverServices then
      for I := 0 to FFilterUUIDList.Count - 1 do
      begin
        GUID := FFilterUUIDList[I].ToHexString;
        GUID := Guid128bStandard.Substring(0, 5) + GUID + Guid128bStandard.Substring(9, Guid128bStandard.Length - 8);
        if ADevice.GetService(TGUID.Create(GUID)) <> nil then
          Exit(True);
      end;
  end;

  function FilterDiscoveredDevices: Boolean;
  var
    I: Integer;
  begin
    Result := True;
    if (FFilterUUIDList <> nil) and (FFilterUUIDList.Count > 0) then
      for I := 0 to DevicesAdvDataNoFiltered.Count - 1 do
        if MatchAnyService(DevicesAdvDataNoFiltered[I]) then
        begin
          AddDeviceToList(DevicesAdvDataNoFiltered[I], DevicesAdvDataFiltered);
          PrintLine;
        end;
  end;
begin
  if ForceConnectDevices and (DevicesAdvDataNoFiltered <> nil) then
  begin
    Listbox1.Items.Add('  -- Connecting to non advertising devices --');
    FilterDiscoveredDevices;
  end;
  Listbox1.Items.Add('  -- END --');
end;


procedure TForm6.Button1Click(Sender: TObject);
begin
  if DevicesAdvDataFiltered = nil then
    DevicesAdvDataFiltered := TBluetoothLEDeviceList.Create(False);
  DevicesAdvDataFiltered.Clear;
  if DevicesAdvDataNoFiltered <> nil then
    DevicesAdvDataNoFiltered.Clear;

  ForceConnectDevices := CheckBox1.IsChecked;
  Listbox1.Clear;
  BluetoothLE1.DiscoverDevices(ScanningTime);
end;

procedure TForm6.Button2Click(Sender: TObject);
begin
  BluetoothLE1.CancelDiscovery;
end;

procedure TForm6.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if DevicesAdvDataFiltered <> nil then
  begin
    DevicesAdvDataFiltered.Clear;
    DevicesAdvDataFiltered := nil;
  end;
  if DevicesAdvDataNoFiltered <> nil then
  begin
    DevicesAdvDataNoFiltered.Clear;
    DevicesAdvDataNoFiltered := nil;
  end;
end;

procedure TForm6.FormShow(Sender: TObject);
begin
  StringRawGuid128bStandard := Guid128bStandard.Substring(9, Guid128bStandard.Length - 10).Replace('-','');

  if FFilterUUIDList = nil then
    FFilterUUIDList := TList<Word>.create;

  FFilterUUIDList.Add(HeartRate);
  FFilterUUIDList.Add(HealthThermometer);
  FFilterUUIDList.Add(DeviceInformation);
end;

end.
