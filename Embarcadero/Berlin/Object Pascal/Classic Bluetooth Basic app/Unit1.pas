//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Bluetooth,
  FMX.Layouts, FMX.ListBox, FMX.StdCtrls, FMX.Memo, FMX.Controls.Presentation,
  FMX.Edit, FMX.TabControl, FMX.ScrollBox, FMX.ListView.Types, FMX.ListView, FMX.ListView.Appearances, System.ImageList,
  FMX.ImgList;

type

  TServerConnectionTH = class(TThread)
  private
    { Private declarations }
    FServerSocket: TBluetoothServerSocket;
    FSocket: TBluetoothSocket;
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
    PanelClient: TPanel;
    LabelClient: TLabel;
    ButtonConnectToRFCOMM: TButton;
    PanelServer: TPanel;
    ButtonOpenReadingSocket: TButton;
    LabelServer: TLabel;
    PnlDiscover: TPanel;
    PnlPairedDevs: TPanel;
    Panel2: TPanel;
    AniIndicator1: TAniIndicator;
    StyleBook1: TStyleBook;
    ListView1: TListView;
    ImageList1: TImageList;
    AniIndicator2: TAniIndicator;
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
    FSocket: TBluetoothSocket;
    ItemIndex: Integer;
    ServerConnectionTH: TServerConnectionTH;
    procedure DevicesDiscoveryEnd(const Sender: TObject; const ADevices: TBluetoothDeviceList);
    procedure PairedDevices;
    procedure SendData;
    function GetServiceImageIndex(const AServiceUUID: TGUID): Integer;
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
  LListItem : TListViewItem;
begin
  ListView1.Items.Clear;
  if ManagerConnected then
    if ComboboxPaired.ItemIndex > -1 then
    begin
      LDevice := FPairedDevices[ComboboxPaired.ItemIndex] as TBluetoothDevice;
      AniIndicator2.Visible := True;
      LServices := LDevice.GetServices;
      AniIndicator2.Visible := False;
      for I := 0 to LServices.Count - 1 do
      begin
        LListItem := ListView1.Items.Add;
        LListItem.ImageIndex := GetServiceImageIndex(LServices[I].UUID);
        LListItem.Text :=  LServices[I].Name;
        if LListItem.Text = '' then
          LListItem.Text := '<Unknown>';
        LListItem.Detail :=  GUIDToString(LServices[I].UUID);
      end;
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
  AniIndicator1.Visible := True;
  ButtonDiscover.Text := 'Discovering...';
  ButtonDiscover.Enabled := False;
  ComboboxDevices.Clear;
  if ManagerConnected then
  begin
    FAdapter := FBluetoothManager.CurrentAdapter;
    FBluetoothManager.StartDiscovery(10000);
    FBluetoothManager.OnDiscoveryEnd := DevicesDiscoveryEnd;
  end;
end;

procedure TForm1.DevicesDiscoveryEnd(const Sender: TObject; const ADevices: TBluetoothDeviceList);
begin
  TThread.Synchronize(nil, procedure
  var
    I: Integer;
  begin
    ButtonDiscover.Text := 'Discover devices';
    ButtonDiscover.Enabled := True;
    AniIndicator1.Visible := False;
    FDiscoverDevices := ADevices;
    for I := 0 to ADevices.Count - 1 do
      ComboboxDevices.Items.Add(ADevices[I].DeviceName + '  -> ' + ADevices[I].Address);
    ComboboxDevices.ItemIndex := 0;
  end);
end;

procedure TForm1.ButtonOpenReadingSocketClick(Sender: TObject);
begin
  if ButtonOpenReadingSocket.IsPressed then
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
        ButtonOpenReadingSocket.Text := 'Stop Text Service';
      except
        on E : Exception do
        begin
          DisplayR.Lines.Add(E.Message);
          DisplayR.GoToTextEnd;
          ButtonOpenReadingSocket.IsPressed := False;
        end;
      end;
    end;
  end
  else
    if ServerConnectionTH <> nil then
    begin
      ServerConnectionTH.Terminate;
      ServerConnectionTH.WaitFor;
      FreeAndNil(ServerConnectionTH);
      DisplayR.Lines.Add(' - Service removed -');
      DisplayR.GoToTextEnd;
      ButtonOpenReadingSocket.Text := 'Start Text Service';
    end
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

procedure TServerConnectionTH.Execute;
var
  Msg: string;
  LData: TBytes;
begin
  while not Terminated do
    try
      FSocket := nil;
      while not Terminated and (FSocket = nil) do
        FSocket := FServerSocket.Accept(100);
      if(FSocket <> nil) then
      begin      
        while not Terminated do
        begin
          LData := FSocket.ReceiveData;
          if Length(LData) > 0 then
            Synchronize(procedure
              begin
                Form1.DisplayR.Lines.Add(TEncoding.UTF8.GetString(LData));
                Form1.DisplayR.GoToTextEnd;
              end);
          Sleep(100);
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

function TForm1.GetServiceImageIndex(const AServiceUUID: TGUID): Integer;
type
   TServiceImg = record
    Name: string;
    UUID: TBluetoothUUID;
    ImageIndex: Integer;
  end;
  TServiceImages = array [0..27] of TServiceImg;

const
  ServiceImages: TServiceImages = (
    (Name: 'LAN Access Using PPP'; UUID:'{00001102-0000-1000-8000-00805F9B34FB}'; ImageIndex: 7),
    (Name: 'DialupNetworking'; UUID:'{00001103-0000-1000-8000-00805F9B34FB}'; ImageIndex: 7),
    (Name: 'OBEXObjectPush'; UUID:'{00001105-0000-1000-8000-00805F9B34FB}'; ImageIndex: 8),
    (Name: 'OBEXFileTransfer'; UUID:'{00001106-0000-1000-8000-00805F9B34FB}'; ImageIndex: 8),
    (Name: 'Cordless Telephony'; UUID:'{00001109-0000-1000-8000-00805F9B34FB}'; ImageIndex: 5),
    (Name: 'Audio Source'; UUID:'{0000110A-0000-1000-8000-00805F9B34FB}'; ImageIndex: 1),
    (Name: 'Audio Sink'; UUID:'{0000110B-0000-1000-8000-00805F9B34FB}'; ImageIndex: 1),
    (Name: 'AV Remote Control Target'; UUID:'{0000110C-0000-1000-8000-00805F9B34FB}'; ImageIndex: 2),
    (Name: 'Advanced Audio Distribution'; UUID:'{0000110D-0000-1000-8000-00805F9B34FB}'; ImageIndex: 1),
    (Name: 'AV Remote Control'; UUID:'{0000110E-0000-1000-8000-00805F9B34FB}'; ImageIndex: 2),
    (Name: 'Headset Audio Gateway'; UUID:'{00001112-0000-1000-8000-00805F9B34FB}'; ImageIndex: 6),
    (Name: 'WAP'; UUID:'{00001113-0000-1000-8000-00805F9B34FB}'; ImageIndex: 7),
    (Name: 'WAP Client'; UUID:'{00001114-0000-1000-8000-00805F9B34FB}'; ImageIndex: 7),
    (Name: 'Personal Area Network User (PANU)'; UUID:'{00001115-0000-1000-8000-00805F9B34FB}'; ImageIndex: 9),
    (Name: 'Network Access Point (NAP)'; UUID:'{00001116-0000-1000-8000-00805F9B34FB}'; ImageIndex: 7),
    (Name: 'Group Ad-hoc Network (GN)'; UUID:'{00001117-0000-1000-8000-00805F9B34FB}'; ImageIndex: 7),
    (Name: 'Handsfree'; UUID:'{0000111E-0000-1000-8000-00805F9B34FB}'; ImageIndex: 5),
    (Name: 'Handsfree Audio Gateway'; UUID:'{0000111F-0000-1000-8000-00805F9B34FB}'; ImageIndex: 5),
    (Name: 'SIM Access'; UUID:'{0000112D-0000-1000-8000-00805F9B34FB}'; ImageIndex: 10),
    (Name: 'Phonebook Access - PCE'; UUID:'{0000112E-0000-1000-8000-00805F9B34FB}'; ImageIndex: 0),
    (Name: 'Phonebook Access - PSE'; UUID:'{0000112F-0000-1000-8000-00805F9B34FB}'; ImageIndex: 0),
    (Name: 'Phonebook Access'; UUID:'{00001130-0000-1000-8000-00805F9B34FB}'; ImageIndex: 0),
    (Name: 'Headset headset'; UUID:'{00001131-0000-1000-8000-00805F9B34FB}'; ImageIndex: 6),
    (Name: 'Message Access Server'; UUID:'{00001132-0000-1000-8000-00805F9B34FB}'; ImageIndex: 4),
    (Name: 'Message Notification Server'; UUID:'{00001133-0000-1000-8000-00805F9B34FB}'; ImageIndex: 4),
    (Name: 'Message Access Profile'; UUID:'{00001134-0000-1000-8000-00805F9B34FB}'; ImageIndex: 4),
    (Name: 'Generic Networking'; UUID:'{00001201-0000-1000-8000-00805F9B34FB}'; ImageIndex: 7),
    (Name: 'Generic Audio'; UUID:'{00001203-0000-1000-8000-00805F9B34FB}'; ImageIndex: 1)
  );
var
  I: Integer;
begin
  Result := 3;
  for I := Low(ServiceImages) to High(ServiceImages) do
    if ServiceImages[I].UUID = AServiceUUID then
      Exit(ServiceImages[I].ImageIndex);
end;

end.
