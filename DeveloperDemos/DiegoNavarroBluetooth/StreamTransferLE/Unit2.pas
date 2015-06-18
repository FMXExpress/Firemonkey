unit Unit2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Bluetooth, System.Bluetooth.Components,
  FMX.Objects, FMX.ListView.Types, FMX.ListView, FMX.StdCtrls, FMX.TabControl, BluetoothStream, FMX.Layouts, FMX.Memo,
  System.Actions, FMX.ActnList, FMX.StdActns, FMX.MediaLibrary.Actions;

type
  TForm2 = class(TForm)
    BluetoothLE1: TBluetoothLE;
    Image1: TImage;
    StyleBook1: TStyleBook;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    ListView1: TListView;
    Image3: TImage;
    Button1: TButton;
    Timer1: TTimer;
    ReceiveProgressLabel: TLabel;
    SecondsLabel: TLabel;
    ReceiveProgressBar: TProgressBar;
    SendProgressBar: TProgressBar;
    SendProgressLabel: TLabel;
    ActionList1: TActionList;
    TakePhotoFromLibraryAction1: TTakePhotoFromLibraryAction;
    Button2: TButton;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure BluetoothLE1EndDiscoverDevices(const Sender: TObject; const ADeviceList: TBluetoothLEDeviceList);
    procedure BluetoothLE1CharacteristicRead(const Sender: TObject; const ACharacteristic: TBluetoothGattCharacteristic;
      AGattStatus: TBluetoothGattStatus);
    procedure BluetoothLE1CharacteristicReadRequest(const Sender: TObject;
      const ACharacteristic: TBluetoothGattCharacteristic; var AGattStatus: TBluetoothGattStatus);
    procedure Timer1Timer(Sender: TObject);
    procedure BluetoothLE1CharacteristicWriteRequest(const Sender: TObject;
      const ACharacteristic: TBluetoothGattCharacteristic; var AGattStatus: TBluetoothGattStatus;
      const AValue: TArray<System.Byte>);
    procedure BluetoothLE1CharacteristicWrite(const Sender: TObject;
      const ACharacteristic: TBluetoothGattCharacteristic; AGattStatus: TBluetoothGattStatus);
    procedure TakePhotoFromLibraryAction1DidFinishTaking(Image: TBitmap);
    procedure ListView1ItemClick(const Sender: TObject; const AItem: TListViewItem);
  private
    { Private declarations }
    procedure OnSendProgress(Sender: TObject; Progress: Integer; BytesProcessed: Int64);
    procedure OnReceiveProgress(Sender: TObject; Progress: Integer; BytesProcessed: Int64);
    procedure ClientRequestTransfer;
    procedure ClienteStartTransfer;
    procedure ServerPrepareTransfer;
  public
    { Public declarations }
  end;

const
  ServiceUUIDPattern: string = '56829860-6B1E-4830-8073-9C77889E'; // XXXX
  CharacteristicStreamUUID: TBluetoothUUID = '{90EB5B5C-7673-4856-8894-A71D43CA8A4C}';
  CharacteristicControlUUID: TBluetoothUUID = '{CF985997-5C38-48AD-BFF4-2D10C4AA5A30}';

var
  Form2: TForm2;
  GattServer: TBluetoothGattServer;
  ServerService: TBluetoothGattService;
  ServerStreamCharacteristic: TBluetoothGattCharacteristic;
  ServerControlCharacteristic: TBluetoothGattCharacteristic;
  ClientService: TBluetoothGattService;
  ClientStreamCharacteristic: TBluetoothGattCharacteristic;
  ClientControlCharacteristic: TBluetoothGattCharacteristic;
  ClientDeviceList: TBluetoothLEDeviceList;
  ClientDevice: TBluetoothLEDevice;
  StreamReceiver: TBluetoothStreamReceiver;
  StreamSender: TBluetoothStreamSender;

implementation

{$R *.fmx}

uses System.IOUtils;

function GenerateServiceUUID: TBluetoothUUID;
const
  Hex: array[0..15] of Char = '0123456789ABCDEF';
var
  LStr: string;
begin
  LStr := '';
  repeat
    LStr := LStr + Hex[Random(Length(Hex))];
  until (Length(LStr) = 4);
  Result := TBluetoothUUID.Create('{' + ServiceUUIDPattern + LStr + '}');
end;

function GetStreamService(ADevice: TBluetoothLEDevice): TBluetoothGattService;
var
  LService: TBluetoothGattService;
  Str: string;
begin
  for LService in ADevice.Services do
  begin
    Str := LService.UUID.ToString;
    if Str.StartsWith('{' + ServiceUUIDPattern) then
      Exit(LService);
  end;
  Result := nil;
end;

function BytesToString(const B: TBytes): string;
var
  I: Integer;
begin
  if Length(B) > 0 then
  begin
    Result := Format('%0.2X', [B[0]]);
    for I := 1 to High(B) do
      Result := Result + Format(' %0.2X', [B[I]]);
  end
  else
    Result := '';
end;

procedure TForm2.BluetoothLE1CharacteristicRead(const Sender: TObject;
  const ACharacteristic: TBluetoothGattCharacteristic; AGattStatus: TBluetoothGattStatus);
var
  LStream: TStream;
  LBytes: TBytes;
begin
  LBytes := ACharacteristic.Value;
  StreamReceiver.ProcessPacket(LBytes);

  if StreamReceiver.Finished then
  begin
    LStream := StreamReceiver.Stream;
    LStream.Position := 0;
    Image3.Bitmap.LoadFromStream(LStream)
  end
  else
    BluetoothLE1.ReadCharacteristic(ClientDevice, ClientStreamCharacteristic);
end;

procedure TForm2.BluetoothLE1CharacteristicReadRequest(const Sender: TObject;
  const ACharacteristic: TBluetoothGattCharacteristic; var AGattStatus: TBluetoothGattStatus);
var
  LStream: TBytesStream;
  LBytes: TBytes;
begin
  if ACharacteristic.UUID = ServerStreamCharacteristic.UUID then
    ACharacteristic.Value := StreamSender.CreatePacket;
end;

procedure TForm2.BluetoothLE1CharacteristicWrite(const Sender: TObject;
  const ACharacteristic: TBluetoothGattCharacteristic; AGattStatus: TBluetoothGattStatus);
begin
  ClienteStartTransfer;
end;

procedure TForm2.BluetoothLE1CharacteristicWriteRequest(const Sender: TObject;
  const ACharacteristic: TBluetoothGattCharacteristic; var AGattStatus: TBluetoothGattStatus;
  const AValue: TArray<System.Byte>);
begin
  if ACharacteristic.UUID = ServerControlCharacteristic.UUID then
    ServerPrepareTransfer;
end;

procedure TForm2.BluetoothLE1EndDiscoverDevices(const Sender: TObject; const ADeviceList: TBluetoothLEDeviceList);
var
  LDevice: TBluetoothLEDevice;
  LItem: TListViewItem;
  LService: TBluetoothGattService;
begin
  if ClientDeviceList <> nil then
    ClientDeviceList.Free;

  // Filter by services
  ClientDeviceList := TBluetoothLEDeviceList.Create;
  for LDevice in ADeviceList do
  begin
    if LDevice.DiscoverServices then
      if GetStreamService(LDevice) <> nil then
      begin
        ClientDeviceList.Add(LDevice);
        Break;
      end;
  end;

  // Update list
  ListView1.BeginUpdate;
  ListView1.ClearItems;
  for LDevice in ClientDeviceList do
  begin
    LItem := ListView1.Items.Add;
    LItem.Text := LDevice.DeviceName;
  end;
  ListView1.EndUpdate;
  Button1.Enabled := True;
end;

procedure TForm2.Button1Click(Sender: TObject);
var
  B: TBluetoothConnectionState;
begin
  BluetoothLE1.DiscoverDevices(1000);
  Button1.Enabled := False;
end;

procedure TForm2.ClienteStartTransfer;
var
  LStream: TStream;
begin
  LStream := TBytesStream.Create;
  StreamReceiver := TBluetoothStreamReceiver.Create(LStream);
  StreamReceiver.OnBluetoothStreamProgressEvent := OnReceiveProgress;
  BluetoothLE1.ReadCharacteristic(ClientDevice, ClientStreamCharacteristic);  // Start read
end;

procedure TForm2.ClientRequestTransfer;
var
  LBytes: TBytes;
begin
  // Tell the server to reset the transfer
  SetLength(LBytes, 1);
  LBytes[0] := 0;
  ClientControlCharacteristic.Value := LBytes;
  BluetoothLE1.WriteCharacteristic(ClientDevice, ClientControlCharacteristic);
end;

procedure TForm2.FormShow(Sender: TObject);
var
  LBytes: TBytes;
begin
  if BluetoothLE1.SupportsGattServer then
  begin
    GattServer := BluetoothLE1.GetGattServer;
    ServerService := GattServer.CreateService(GenerateServiceUUID, TBluetoothServiceType.Primary);
    ServerService.AdvertiseData := 'Stream Service';
    ServerStreamCharacteristic := GattServer.CreateCharacteristic(ServerService, CharacteristicStreamUUID,[TBluetoothProperty.Read],
      'Stream file');
    ServerControlCharacteristic := GattServer.CreateCharacteristic(ServerService, CharacteristicControlUUID,[TBluetoothProperty.Write],
      'Stream control');
    GattServer.AddService(ServerService);
  end
end;

procedure TForm2.ListView1ItemClick(const Sender: TObject; const AItem: TListViewItem);
var
  I, J: Integer;
  LBytes: TBytes;
begin
  ClientDevice := ClientDeviceList[AItem.Index];
  ClientService := GetStreamService(ClientDevice);
  BluetoothLE1.GetCharacteristics(ClientService);
  ClientStreamCharacteristic := BluetoothLE1.GetCharacteristic(ClientService, CharacteristicStreamUUID);
  ClientControlCharacteristic := BluetoothLE1.GetCharacteristic(ClientService, CharacteristicControlUUID);
  ClientRequestTransfer;
end;

procedure TForm2.OnReceiveProgress(Sender: TObject; Progress: Integer; BytesProcessed: Int64);
begin
  ReceiveProgressBar.Value := Progress;
  ReceiveProgressLabel.Text := BytesProcessed.ToString + ' bytes';
  Timer1.Enabled := True;
end;

procedure TForm2.OnSendProgress(Sender: TObject; Progress: Integer; BytesProcessed: Int64);
begin
  SendProgressBar.Value := Progress;
  SendProgressLabel.Text := BytesProcessed.ToString + ' bytes';
end;

procedure TForm2.ServerPrepareTransfer;
var
  LStream: TStream;
begin
  LStream := TBytesStream.Create;
  Image1.Bitmap.SaveToStream(LStream);
  StreamSender := TBluetoothStreamSender.Create(LStream);
  StreamSender.OnBluetoothStreamProgressEvent := OnSendProgress;
end;

procedure TForm2.TakePhotoFromLibraryAction1DidFinishTaking(Image: TBitmap);
begin
  Image1.Bitmap.Assign(Image);
end;

procedure TForm2.Timer1Timer(Sender: TObject);
begin
  SecondsLabel.Text := (StrToInt(SecondsLabel.Text) + 1).toString;
end;

end.
