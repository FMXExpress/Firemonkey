unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Bluetooth,
  FMX.Layouts, FMX.ListBox, FMX.StdCtrls, FMX.Memo, FMX.Controls.Presentation,
  FMX.Edit, FMX.TabControl;

type

  TServerConnectionTH = class(TThread)
  private
    { Private declarations }
    FServerSocket: TBluetoothServerSocket;
    FSocket: TBluetoothSocket;
    FData: TBytes;
  protected
    procedure Execute; override;
  public
    { Public declarations }
    constructor Create(ACreateSuspended: Boolean);
    destructor Destroy; override;
  end;

  TForm1 = class(TForm)
    ButtonDiscover: TButton;
    ButtonPair: TButton;
    ButtonUnPair: TButton;
    ButtonPairedDevices: TButton;
    DisplayR: TMemo;
    Edit1: TEdit;
    Button2: TButton;
    FreeSocket: TButton;
    Labeldiscoverable: TLabel;
    ComboBoxDevices: TComboBox;
    ComboBoxPaired: TComboBox;
    Panel1: TPanel;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    LabelNameSarver: TLabel;
    ButtonServices: TButton;
    ComboBoxServices: TComboBox;
    PanelClient: TPanel;
    LabelClient: TLabel;
    ButtonConnectToRFCOMM: TButton;
    PanelServer: TPanel;
    ButtonCloseReadingSocket: TButton;
    ButtonOpenReadingSocket: TButton;
    LabelServer: TLabel;
    procedure ButtonDiscoverClick(Sender: TObject);
    procedure ButtonPairClick(Sender: TObject);
    procedure ButtonUnPairClick(Sender: TObject);
    procedure ButtonPairedDeviceClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonOpenReadingSocketClick(Sender: TObject);
    procedure ButtonConnectToRFCOMMClick(Sender: TObject);
    procedure ButtonCloseReadingSocketClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FreeSocketClick(Sender: TObject);
    function ManagerConnected:Boolean;
    function GetServiceName(GUID: string): string;
    procedure ComboBoxPairedChange(Sender: TObject);
    procedure ButtonServicesClick(Sender: TObject);
  private
    { Private declarations }
    FBluetoothManager: TBluetoothManager;
    FDiscoverDevices: TBluetoothDeviceList;
    FPairedDevices: TBluetoothDeviceList;
    FAdapter: TBluetoothAdapter;
    FData: TBytes;
    FSocket: TBluetoothSocket;
    ItemIndex: Integer;
    ServerConnectionTH: TServerConnectionTH;
    procedure DevicesDiscoveryEnd(const Sender: TObject; const ADevices: TBluetoothDeviceList);
    procedure PairedDevices;
    procedure SendData;
  public
    { Public declarations }
  end;

Const
  ServiceName = 'Basic Text Server';
  ServiceGUI = '{B62C4E8D-62CC-404B-BBBF-BF3E3BBB1378}';
var
  Form1: TForm1;

implementation

{$R *.fmx}
{$R *.NmXhdpiPh.fmx ANDROID}
{$R *.LgXhdpiPh.fmx ANDROID}
{$R *.SmXhdpiPh.fmx ANDROID}
{$R *.Macintosh.fmx MACOS}
{$R *.iPhone4in.fmx IOS}
{$R *.Windows.fmx MSWINDOWS}

procedure TForm1.ButtonPairClick(Sender: TObject);
begin
  if ManagerConnected then
    if ComboboxDevices.ItemIndex > -1 then
      FAdapter.Pair(FDiscoverDevices[ComboboxDevices.ItemIndex])
    else
      ShowMessage('No device selected');
end;

procedure TForm1.ButtonUnPairClick(Sender: TObject);
begin
  if ManagerConnected then
    if ComboboxPaired.ItemIndex > -1 then
      FAdapter.UnPair(FPairedDevices[ComboboxPaired.ItemIndex])
    else
      ShowMessage('No Paired device selected');
end;

procedure TForm1.ComboBoxPairedChange(Sender: TObject);
begin
  LabelNameSarver.Text := ComboBoxPaired.Items[ComboBoxPaired.ItemIndex];
end;

procedure TForm1.PairedDevices;
var
  I: Integer;
begin
  ComboboxPaired.Clear;
  if ManagerConnected then
  begin
  FPairedDevices := FBluetoothManager.GetPairedDevices;
  if FPairedDevices.Count > 0 then
    for I:= 0 to FPairedDevices.Count - 1 do
      ComboboxPaired.Items.Add(FPairedDevices[I].DeviceName)
  else
    ComboboxPaired.Items.Add('No Paired Devices');
  end;
end;

procedure TForm1.ButtonPairedDeviceClick(Sender: TObject);
begin
  PairedDevices;
  ComboboxPaired.DropDown;
end;

procedure TForm1.ButtonServicesClick(Sender: TObject);
var
  LServices: TBluetoothServiceList;
  LDevice: TBluetoothDevice;
  I: Integer;
begin
  ComboBoxServices.Clear;
  if ManagerConnected then
    if ComboboxPaired.ItemIndex > -1 then
    begin
      LDevice := FPairedDevices[ComboboxPaired.ItemIndex] as TBluetoothDevice;
      LServices := LDevice.GetServices;
      for I := 0 to LServices.Count - 1 do
        ComboBoxServices.Items.Add(LServices[I].Name + ' --> ' + GUIDToString(LServices[I].UUID));
      ComboBoxServices.ItemIndex := 0;
      ComboBoxServices.DropDown;
    end
    else
      ShowMessage('No paired device selected');
end;

procedure TForm1.FreeSocketClick(Sender: TObject);
begin
  FreeAndNil(FSocket);
  DisplayR.Lines.Add('Client socket set free');
  DisplayR.GoToLineEnd;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  DisplayR.ReadOnly := False;
  DisplayR.SelectAll;
  DisplayR.DeleteSelection;
  DisplayR.ReadOnly := True;
end;

function TForm1.GetServiceName(GUID: string): string;
var
  LServices: TBluetoothServiceList;
  LDevice: TBluetoothDevice;
  I: Integer;
begin
  LDevice := FPairedDevices[ComboboxPaired.ItemIndex] as TBluetoothDevice;
  LServices := LDevice.GetServices;
  for I := 0 to LServices.Count - 1 do
  begin
    if StringToGUID(GUID) = LServices[I].UUID then
    begin
      Result := LServices[I].Name;
      break;
    end;
  end;
end;

procedure TForm1.ButtonConnectToRFCOMMClick(Sender: TObject);
begin
  if ManagerConnected then
    try
      SendData;
    except
      on E : Exception do
      begin
        DisplayR.Lines.Add(E.Message);
        DisplayR.GoToTextEnd;
        FreeAndNil(FSocket);
      end;
    end;
end;

function TForm1.ManagerConnected:Boolean;
begin
  if FBluetoothManager.ConnectionState = TBluetoothConnectionState.Connected then
  begin
    Labeldiscoverable.Text := 'Device discoverable as "'+FBluetoothManager.CurrentAdapter.AdapterName+'"';
    Result := True;
  end
  else
  begin
    Result := False;
    DisplayR.Lines.Add('No Bluetooth device Found');
    DisplayR.GoToTextEnd;
  end
end;

procedure TForm1.SendData;
var
  ToSend: TBytes;
  LDevice: TBluetoothDevice;
begin
  if (FSocket = nil) or (ItemIndex <> ComboboxPaired.ItemIndex) then
  begin
    if ComboboxPaired.ItemIndex > -1 then
    begin
      LDevice := FPairedDevices[ComboboxPaired.ItemIndex] as TBluetoothDevice;
      DisplayR.Lines.Add(GetServiceName(ServiceGUI));
      DisplayR.GoToTextEnd;
      FSocket := LDevice.CreateClientSocket(StringToGUID(ServiceGUI), False);
      if FSocket <> nil then
      begin
        ItemIndex := ComboboxPaired.ItemIndex;
        FSocket.Connect;
        ToSend := TEncoding.UTF8.GetBytes(Edit1.Text);
        FSocket.SendData(ToSend);
        DisplayR.Lines.Add('Text Sent');
        DisplayR.GoToTextEnd;
      end
      else
        ShowMessage('Out of time -15s-');
    end
    else
      ShowMessage('No paired device selected');
  end
  else
  begin
    ToSend := TEncoding.UTF8.GetBytes(Edit1.Text);
    FSocket.SendData(ToSend);
    DisplayR.Lines.Add('Text Sent');
    DisplayR.GoToTextEnd;
  end;
end;

procedure TForm1.ButtonDiscoverClick(Sender: TObject);
begin
  ComboboxDevices.Clear;
  if ManagerConnected then
  begin
    FAdapter := FBluetoothManager.CurrentAdapter;
    FBluetoothManager.StartDiscovery(10000);
    FBluetoothManager.OnDiscoveryEnd := DevicesDiscoveryEnd;
  end;
end;

procedure TForm1.DevicesDiscoveryEnd(const Sender: TObject; const ADevices: TBluetoothDeviceList);
var
  I: Integer;
begin
  FDiscoverDevices := ADevices;
  for I := 0 to ADevices.Count - 1 do
    ComboboxDevices.Items.Add(ADevices[I].DeviceName + '  -> ' + ADevices[I].Address);
  ComboboxDevices.ItemIndex := 0;
end;

procedure TForm1.ButtonOpenReadingSocketClick(Sender: TObject);
begin
  if (ServerConnectionTH = nil) and ManagerConnected then
  begin
    try
      FAdapter := FBluetoothManager.CurrentAdapter;
      ServerConnectionTH := TServerConnectionTH.Create(True);
      ServerConnectionTH.FServerSocket := FAdapter.CreateServerSocket(ServiceName, StringToGUID(ServiceGUI), False);
      ServerConnectionTH.Start;
      DisplayR.Lines.Add(' - Service created: "'+ServiceName+'"');
      DisplayR.GoToTextEnd;
    except
      on E : Exception do
      begin
        DisplayR.Lines.Add(E.Message);
        DisplayR.GoToTextEnd;
      end;
    end;
  end;
end;

procedure TForm1.ButtonCloseReadingSocketClick(Sender: TObject);
begin
  if ServerConnectionTH <> nil then
  begin
    ServerConnectionTH.Terminate;
    ServerConnectionTH.WaitFor;
    FreeAndNil(ServerConnectionTH);
    DisplayR.Lines.Add(' - Service removed -');
    DisplayR.GoToTextEnd;
  end
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  try
    LabelServer.Text := ServiceName;
    LabelClient.Text := 'Client of '+ServiceName;
    FBluetoothManager := TBluetoothManager.Current;
    FAdapter := FBluetoothManager.CurrentAdapter;
    if ManagerConnected then
    begin
      PairedDevices;
      ComboboxPaired.ItemIndex := 0;
    end;
  except
    on E : Exception do
    begin
      ShowMessage(E.Message);
    end;
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if ServerConnectionTH <> nil then
  begin
    ServerConnectionTH.Terminate;
    ServerConnectionTH.WaitFor;
    FreeAndNil(ServerConnectionTH);
  end
end;

{TServerConnection}

constructor TServerConnectionTH.Create(ACreateSuspended: Boolean);
begin
  inherited;
end;

destructor TServerConnectionTH.Destroy;
begin
  FSocket.Free;
  FServerSocket.Free;
  inherited;
end;

procedure TServerConnectionTH.execute;
var
  ASocket: TBluetoothSocket;
  Msg: string;
begin
  while not Terminated do
    try
      ASocket := nil;
      while not Terminated and (ASocket = nil) do
        ASocket := FServerSocket.Accept(100);
      if(ASocket <> nil) then
      begin
        FSocket := ASocket;
        while not Terminated do
        begin
          FData := ASocket.ReadData;
          if length(FData) > 0 then
            Synchronize(procedure
              begin
                Form1.DisplayR.Lines.Add(TEncoding.UTF8.GetString(FData));
                Form1.DisplayR.GoToTextEnd;
              end);
          sleep(100);
        end;
      end;
    except
      on E : Exception do
      begin
        Msg := E.Message;
        Synchronize(procedure
          begin
            Form1.DisplayR.Lines.Add('Server Socket closed: ' + Msg);
            Form1.DisplayR.GoToTextEnd;
          end);
      end;
    end;
end;

end.
