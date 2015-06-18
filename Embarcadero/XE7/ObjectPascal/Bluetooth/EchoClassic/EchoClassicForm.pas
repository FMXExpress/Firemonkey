unit EchoClassicForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Memo, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Edit, FMX.ListBox,
  System.Bluetooth;

type
  TTEchoClassicForm = class(TForm)
    CBDevices: TComboBox;
    KnownDevices: TButton;
    Listen: TButton;
    Seconds: TEdit;
    Edit1: TEdit;
    Send: TButton;
    Secure: TCheckBox;
    Memo1: TMemo;
    procedure KnownDevicesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListenClick(Sender: TObject);
    procedure SendClick(Sender: TObject);
  private
    { Private declarations }
    procedure DiscoveryEnd(const Sender: TObject; const ADeviceList: TBluetoothDeviceList);
    function FindBTDevice(Device: string): TBluetoothDevice;
  public
    { Public declarations }
  end;

var
  TEchoClassicForm: TTEchoClassicForm;
  Manager: TBluetoothManager;
  Adapter: TBluetoothAdapter;
  PairedDevices: TBluetoothDeviceList;
  DiscoveredDevices: TBluetoothDeviceList;
  TestServiceClass_UUID:  TGUID;

implementation

{$R *.fmx}

procedure TTEchoClassicForm.DiscoveryEnd(const Sender: TObject;
  const ADeviceList: TBluetoothDeviceList);
var
  I: Integer;
  LDevice: TBluetoothCustomDevice;
begin
  DiscoveredDevices := ADeviceList;
  Memo1.Lines.Add('Devices discovered: ');
  for I := 0 to ADeviceList.Count - 1 do
  begin
    LDevice := ADeviceList.Items[I];
    Memo1.Lines.Add(LDevice.Address + ': ' + LDevice.DeviceName);
  end;
end;

function TTEchoClassicForm.FindBTDevice(Device: string): TBluetoothDevice;
var
  I: integer;
  LDevice: TBluetoothDevice;
	KnownDevices: TBluetoothDeviceList;
begin
  KnownDevices := Manager.GetPairedDevices(Manager.CurrentAdapter);
  for I := 0 to KnownDevices.Count - 1 do
  begin
    LDevice := KnownDevices.Items[I];
    if Device = LDevice.DeviceName then Exit(LDevice);
  end;

  KnownDevices := Manager.LastDiscoveredDevices;
  for I := 0 to KnownDevices.Count - 1  do
  begin
    LDevice := KnownDevices.Items[I];
    if Device = LDevice.DeviceName then Exit(LDevice);
  end;

  Result := nil;
end;

procedure TTEchoClassicForm.FormCreate(Sender: TObject);
begin
  Manager := TBluetoothManager.Current;
  Manager.OnDiscoveryEnd := DiscoveryEnd;
  TestServiceClass_UUID := StringToGUID('{B62C4E8D-62CC-404b-BBBF-BF3E3BBB1374}');
end;

procedure TTEchoClassicForm.KnownDevicesClick(Sender: TObject);
var
  I: integer;
begin
  PairedDevices := Manager.CurrentAdapter.PairedDevices;
  CBDevices.Items.Clear;

  for I := 0 to PairedDevices.Count - 1 do
    CBDevices.Items.Add(PairedDevices.Items[I].DeviceName);
end;

procedure TTEchoClassicForm.ListenClick(Sender: TObject);
var
  s: TBluetoothServerSocket;
	client: TBluetoothSocket;
	Buff: TBytes;
begin
  client := nil;
  s := Manager.CurrentAdapter.CreateServerSocket('Test windows socket', TestServiceClass_UUID, Secure.IsChecked);

  try
    Memo1.Lines.Add('Accept. Waiting connection.');
    client := s.Accept(StrToInt(Seconds.Text)*1000);
    Memo1.Lines.Add('Accept. reading');
    Buff := client.ReadData;
    Memo1.Lines.Add('Received: ' + TEncoding.UTF8.GetString(Buff));
  except
    on E: Exception do
       Memo1.Lines.Add('Exception ' + E.Message );
  end;
  s.Free;
  client.Free;
end;

procedure TTEchoClassicForm.SendClick(Sender: TObject);
var
  I : Integer;
  cad : String;
 	Buff: TBytes;
  LDevice: TBluetoothDevice;
	LSockect: TBluetoothSocket;
begin
  Buff := TEncoding.UTF8.GetBytes(Edit1.Text);
  cad := '';

  for I := 0 to Length(Buff) - 1  do
  begin
    cad := cad + Format('%0.2X(%d) ', [Buff[I], Buff[I]]);
                         //hex dec
  end;

  Memo1.Lines.Add('Sending(' + IntToStr(Length(Buff)) + '): ');
	Memo1.Lines.Add(cad);

  LDevice := FindBTDevice(CBDevices.Selected.Text);
	if (LDevice <> nil) then
	begin
		LSockect := LDevice.CreateClientSocket(TestServiceClass_UUID, Secure.IsChecked);
    try
      Memo1.Lines.Add('Connecting:');
      Application.ProcessMessages;
      LSockect.Connect;
      Memo1.Lines.Add('Connected. waiting before send');
      Application.ProcessMessages;
      Memo1.Lines.Add('Sending Data');
      Application.ProcessMessages;
      LSockect.SendData(Buff);
      LSockect.Close;
      Memo1.Lines.Add('Data Sent');
      Application.ProcessMessages;
    except
      on E: Exception do
         Memo1.Lines.Add('Exception ' + E.Message );
    end;
    LSockect.Free;
	end;
end;

end.
