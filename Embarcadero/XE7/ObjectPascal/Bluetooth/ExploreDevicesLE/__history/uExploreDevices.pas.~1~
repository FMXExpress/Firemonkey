unit uExploreDevices;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Edit,
  FMX.ListBox, FMX.Layouts, System.Rtti, FMX.Grid, System.Bluetooth, FMX.EditBox, FMX.SpinBox,
  system.NetEncoding, FMX.TreeView;

type
  TFrDeviceExplorer = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Label2: TLabel;
    CornerButton3: TCornerButton;
    Label4: TLabel;
    Panel4: TPanel;
    Button1: TButton;
    Button2: TButton;
    CbDevices: TComboBox;
    Panel5: TPanel;
    Label1: TLabel;
    EdCurrentDevice: TEdit;
    PbFindDevices: TProgressBar;
    tmAnimateFindDevices: TTimer;
    ScrollBox1: TScrollBox;
    PbServices: TProgressBar;
    Panel6: TPanel;
    Panel7: TPanel;
    lbCurrentService: TLabel;
    Panel8: TPanel;
    ScrollBox2: TScrollBox;
    Panel9: TPanel;
    Label6: TLabel;
    CbRead: TCheckBox;
    cbWriteNoResponse: TCheckBox;
    cbBroadcast: TCheckBox;
    CbWrite: TCheckBox;
    cbSignedWrite: TCheckBox;
    cbExtendedProp: TCheckBox;
    cbNotify: TCheckBox;
    cbIndicate: TCheckBox;
    Label5: TLabel;
    Label8: TLabel;
    btnRefresh: TSpeedButton;
    btnSuscribe: TButton;
    btnWrite: TButton;
    EdCharacWrite: TEdit;
    CbWriteTypes: TComboBox;
    EdCharacName: TEdit;
    EdCharacUID: TEdit;
    Label7: TLabel;
    TvCharacteristics: TTreeView;
    tmAnimateFindServices: TTimer;
    btnGetValues: TButton;
    LbCurrentValue: TListBox;
    Panel3: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure tmAnimateFindDevicesTimer(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CornerButton3Click(Sender: TObject);
    procedure tmAnimateFindServicesTimer(Sender: TObject);
    procedure btnWriteClick(Sender: TObject);
    procedure btnSuscribeClick(Sender: TObject);
    procedure btnGetValuesClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TvCharacteristicsClick(Sender: TObject);
  private
    FBluetoothManagerLE: TBluetoothLEManager;
    CurrentService: Integer;
    CurrentCharacteristic: Integer;
    procedure CleanDeviceInformation;
    procedure DevicesDiscoveryLEEnd(const Sender: TObject; const ADevices: TBluetoothLEDeviceList);
    procedure ServicesDiscovered(const Sender: TObject; const AServiceList: TBluetoothGattServiceList);
    procedure GetCurrentDevice(var ADevice: TBluetoothLEDevice);
    procedure DidCharacteristicRead(const Sender: TObject; const ACharacteristic: TBluetoothGattCharacteristic;
      AGattStatus: TBluetoothGattStatus);
    procedure RefreshCurrentCharacteristic;
    procedure EmptyCharacteristic;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrDeviceExplorer: TFrDeviceExplorer;

implementation

{$R *.fmx}

procedure TFrDeviceExplorer.Button2Click(Sender: TObject);
begin
  if CbDevices.Selected <> nil then
  begin
    CleanDeviceInformation;
    EdCurrentDevice.Text := CbDevices.Selected.Text;
  end;
end;

procedure TFrDeviceExplorer.DidCharacteristicRead(const Sender: TObject; const ACharacteristic: TBluetoothGattCharacteristic;
  AGattStatus: TBluetoothGattStatus);
begin
  if GUIDToString(ACharacteristic.UUID) = EdCharacUID.Text then
    RefreshCurrentCharacteristic;
end;

procedure TFrDeviceExplorer.FormCreate(Sender: TObject);
begin
  CurrentService := 0;
  CurrentCharacteristic := 0;
end;

procedure TFrDeviceExplorer.CleanDeviceInformation;
begin
  TvCharacteristics.Clear;
  EmptyCharacteristic;
  CurrentService := 0;
  CurrentCharacteristic := 0;
end;

procedure TFrDeviceExplorer.CornerButton3Click(Sender: TObject);
var
  ADevice: TBluetoothLEDevice;
begin
  GetCurrentDevice(ADevice);
  if ADevice <> nil then
  begin
    PBServices.Value := 0;
    ADevice.OnServicesDiscovered := ServicesDiscovered;
    tmAnimateFindServices.Enabled := ADevice.DiscoverServices;
  end
  else
    ShowMessage(EdCurrentDevice.Text + ' is not available');
end;

procedure TFrDeviceExplorer.ServicesDiscovered(const Sender: TObject; const AServiceList: TBluetoothGattServiceList);
var
  I: Integer;
  CharList: TBluetoothGattCharacteristicList;
  AChar: TBluetoothGattCharacteristic;
  J: Integer;
  CurrentRow: Integer;
  Options: string;
  ServiceItem, Characteristic, CharProps: TTreeViewItem;
begin
  TvCharacteristics.Clear;
  for I := 0 to AServiceList.Count - 1 do
  begin
    ServiceItem := TTreeViewItem.Create(nil);
    ServiceItem.Parent := TvCharacteristics;
    ServiceItem.Tag := I;
    ServiceItem.IsExpanded := True;
    ServiceItem.Text := AServiceList[I].UUIDName;

    CharList := AServiceList[I].Characteristics;
    for J := 0 to CharList.Count - 1 do
    begin
      AChar := CharList[J];
      TThread.Synchronize(nil, procedure begin
        Options := '';
        if TBluetoothProperty.Broadcast in AChar.Properties then Options := Options + 'Broadcast ';
        if TBluetoothProperty.ExtendedProps in AChar.Properties then Options := Options + 'ExtendedProps ';
        if TBluetoothProperty.Notify in AChar.Properties then Options := Options + 'Notify ';
        if TBluetoothProperty.Indicate in AChar.Properties then Options := Options + 'Indicate ';
        if TBluetoothProperty.Read in AChar.Properties then Options := Options + 'Read ';
        if TBluetoothProperty.Write in AChar.Properties then Options := Options + 'Write ';
        if TBluetoothProperty.WriteNoResponse in AChar.Properties then Options := Options + 'WriteNoResponse ';
        if TBluetoothProperty.SignedWrite in AChar.Properties then Options := Options + 'SignedWrite ';
        Characteristic := TTreeViewItem.Create(nil);
        Characteristic.Parent := ServiceItem;
        Characteristic.IsExpanded := False;
        Characteristic.Text := AChar.UUIDName;
        Characteristic.Tag := J;
        CharProps := TTreeViewItem.Create(nil);
        CharProps.Tag := -1;
        CharProps.Parent := Characteristic;
        CharProps.IsExpanded := True;
        CharProps.Text := GUIDToString(AChar.UUID);
        CharProps := TTreeViewItem.Create(nil);
        CharProps.Tag := -1;
        CharProps.Parent := Characteristic;
        CharProps.IsExpanded := True;
        CharProps.Text := Options;
      end);
      Application.ProcessMessages;
    end;
  end;
  tmAnimateFindServices.Enabled := False;
  PbServices.Value := 100;
end;

procedure TFrDeviceExplorer.GetCurrentDevice(var ADevice: TBluetoothLEDevice);
var
  I: Integer;
begin
  for I := 0 to FBluetoothManagerLE.LastDiscoveredDevices.Count - 1 do
  begin
    if FBluetoothManagerLE.LastDiscoveredDevices[I].DeviceName = EdCurrentDevice.Text then
      ADevice := FBluetoothManagerLE.LastDiscoveredDevices[I];
  end;
end;

procedure TFrDeviceExplorer.DevicesDiscoveryLEEnd(const Sender: TObject; const ADevices: TBluetoothLEDeviceList);
var
  I: Integer;
begin
  CbDevices.Items.Clear;
  for I := 0 to ADevices.Count - 1 do
    CbDevices.Items.Add(ADevices[I].DeviceName);
  tmAnimateFindDevices.Enabled := False;
  PbFindDevices.Value := 100;
end;

procedure TFrDeviceExplorer.tmAnimateFindDevicesTimer(Sender: TObject);
begin
  PbFindDevices.Value := PbFindDevices.Value + 10;
end;

procedure TFrDeviceExplorer.tmAnimateFindServicesTimer(Sender: TObject);
begin
  if PbServices.Value = 99 then
    PbServices.Value := 0
  else
    PbServices.Value := PbServices.Value + 1;
end;

procedure TFrDeviceExplorer.TvCharacteristicsClick(Sender: TObject);
begin

  if (TvCharacteristics.Selected <> nil) and (TvCharacteristics.Selected.Tag <> -1) and (TvCharacteristics.Selected.ParentItem <> nil) then
  begin
    CurrentService := TvCharacteristics.Selected.ParentItem.Tag;
    CurrentCharacteristic := TvCharacteristics.Selected.Tag;
    RefreshCurrentCharacteristic;
  end;
end;

procedure TFrDeviceExplorer.RefreshCurrentCharacteristic;
var
  ADevice: TBluetoothLEDevice;
  AService: TBluetoothGattService;
  AChar: TBluetoothGattCharacteristic;
  I: Integer;
begin
  GetCurrentDevice(ADevice);
  if ADevice <> nil then
  begin
    AService := ADevice.Services[CurrentService];
    AChar:= AService.Characteristics[CurrentCharacteristic];
    lbCurrentService.Text := AService.UUIDName;
    EdCharacName.Text := Achar.UUIDName;
    EdCharacUID.Text := GUIDToString(AChar.UUID);
    cbBroadcast.IsChecked := TBluetoothProperty.Broadcast in AChar.Properties;
    cbExtendedProp.IsChecked := TBluetoothProperty.ExtendedProps in AChar.Properties;
    cbNotify.IsChecked := TBluetoothProperty.Notify in AChar.Properties;
    cbIndicate.IsChecked := TBluetoothProperty.Indicate in AChar.Properties;
    CbRead.IsChecked := TBluetoothProperty.Read in AChar.Properties;
    CbWrite.IsChecked := TBluetoothProperty.Write in AChar.Properties;
    cbWriteNoResponse.IsChecked := TBluetoothProperty.WriteNoResponse in AChar.Properties;
    cbSignedWrite.IsChecked := TBluetoothProperty.SignedWrite in AChar.Properties;

    if CbRead.IsChecked then
    begin
      LbCurrentValue.Items.Clear;
      try
        LbCurrentValue.Items.Add('String(UTF8): ' +  AChar.GetValueAsString);
      except
      end;
      try
        LbCurrentValue.Items.Add('String(NOUTF8): ' +  AChar.GetValueAsString(0,False));
      except
      end;
      try
        LbCurrentValue.Items.Add('Int8: ' +  IntTostr(AChar.GetValueAsInt8));
      except
      end;
      try
        LbCurrentValue.Items.Add('Int16: ' +  IntTostr(AChar.GetValueAsInt16));
      except
      end;
      try
        LbCurrentValue.Items.Add('Int32: ' +  IntTostr(AChar.GetValueAsInt32));
      except
      end;
      try
        LbCurrentValue.Items.Add('Int64: ' +  IntTostr(AChar.GetValueAsInt64));
      except
      end;
      try
        LbCurrentValue.Items.Add('UInt8: ' +  IntTostr(AChar.GetValueAsUInt8));
      except
      end;
      try
        LbCurrentValue.Items.Add('UInt16: ' +  IntTostr(AChar.GetValueAsUInt16));
      except
      end;
      try
        LbCurrentValue.Items.Add('UInt32: ' +  IntTostr(AChar.GetValueAsUInt32));
      except
      end;
      try
        LbCurrentValue.Items.Add('UInt64: ' +  IntTostr(AChar.GetValueAsUInt64));
      except
      end;
      try
        LbCurrentValue.Items.Add('Double: ' +  FloatToStr(AChar.GetValueAsDouble));
      except
      end;
      try
        LbCurrentValue.Items.Add('Single: ' +  FloatToStr(AChar.GetValueAsSingle));
      except
      end;
    end;
  end;
end;

procedure TFrDeviceExplorer.EmptyCharacteristic;
begin
  lbCurrentService.Text := '';
  EdCharacName.Text := '';
  EdCharacUID.Text := '';
  cbBroadcast.IsChecked := False;
  cbExtendedProp.IsChecked := False;
  cbNotify.IsChecked := False;
  cbIndicate.IsChecked := False;
  CbRead.IsChecked := False;
  CbWrite.IsChecked := False;
  cbWriteNoResponse.IsChecked := False;
  cbSignedWrite.IsChecked := False;
  LbCurrentValue.Items.Clear;
end;


procedure TFrDeviceExplorer.btnGetValuesClick(Sender: TObject);
var
  ADevice: TBluetoothLEDevice;
  AService: TBluetoothGattService;
  CharList: TBluetoothGattCharacteristicList;
  AChar: TBluetoothGattCharacteristic;
  J: Integer;
  I: Integer;
begin
  if TvCharacteristics.Count > 0 then
  begin
    GetCurrentDevice(ADevice);
    if ADevice <> nil then
    begin
      ADevice.OnCharacteristicRead := DidCharacteristicRead;
      for I := 0 to ADevice.Services.Count -1 do
      begin
        AChar := nil;
        AService := ADevice.Services[I];
        CharList := AService.Characteristics;
        for J := 0 to CharList.Count - 1 do
        begin
          AChar := CharList.Items[J];
          if (TBluetoothProperty.Read in AChar.Properties) then
             ADevice.ReadCharacteristic(AChar);
        end;
      end;
    end
    else
      ShowMessage(EdCurrentDevice.Text + ' is not available');
  end;
end;

procedure TFrDeviceExplorer.btnRefreshClick(Sender: TObject);
var
  ADevice: TBluetoothLEDevice;
  AService: TBluetoothGattService;
  AChar: TBluetoothGattCharacteristic;
begin
  GetCurrentDevice(ADevice);
  if ADevice <> nil then
  begin
    ADevice.OnCharacteristicRead := DidCharacteristicRead;
    AService := ADevice.Services[CurrentService];
    AChar := AService.Characteristics[CurrentCharacteristic];
    if (TBluetoothProperty.Read in AChar.Properties) then
       ADevice.ReadCharacteristic(AChar);
  end
  else
    ShowMessage(EdCurrentDevice.Text + ' is not available');
end;

procedure TFrDeviceExplorer.btnSuscribeClick(Sender: TObject);
var
  ADevice: TBluetoothLEDevice;
  AService: TBluetoothGattService;
  CharList: TBluetoothGattCharacteristicList;
  AChar: TBluetoothGattCharacteristic;
  J: Integer;
begin
  GetCurrentDevice(ADevice);
  if ADevice <> nil then
  begin
    AChar := nil;
    AService := ADevice.Services[CurrentService];
    AChar := AService.Characteristics[CurrentCharacteristic];
    if (TBluetoothProperty.Notify in AChar.Properties) then
      ADevice.SetCharacteristicNotification(AChar, True)
    else
      Showmessage('This characteristic doesn''t allow notifications');
  end
  else
    ShowMessage(EdCurrentDevice.Text + ' is not available');
end;

procedure TFrDeviceExplorer.btnWriteClick(Sender: TObject);
var
  ADevice: TBluetoothLEDevice;
  AService: TBluetoothGattService;
  AChar: TBluetoothGattCharacteristic;
  J: Integer;
begin
  GetCurrentDevice(ADevice);
  if ADevice <> nil then
  begin
    AChar := nil;
    AService := ADevice.Services[CurrentService];
    AChar := AService.characteristics[CurrentCharacteristic];
    if (TBluetoothProperty.Write in AChar.Properties) or (TBluetoothProperty.WriteNoResponse in AChar.Properties)
       or  (TBluetoothProperty.SignedWrite in AChar.Properties) then
    begin
      if CbWriteTypes.ItemIndex = 0 then
        AChar.SetValueAsString(EdCharacWrite.Text);
      if CbWriteTypes.ItemIndex = 1 then
        AChar.SetValueAsString(EdCharacWrite.Text,False);

      if CbWriteTypes.ItemIndex = 2 then
        AChar.SetValueAsUInt8(UInt8(StrToInt(EdCharacWrite.Text)));
      if CbWriteTypes.ItemIndex = 3 then
        AChar.SetValueAsUInt16(UInt16(StrToInt(EdCharacWrite.Text)));
      if CbWriteTypes.ItemIndex = 4 then
        AChar.SetValueAsUInt32(UInt32(StrToInt(EdCharacWrite.Text)));
      if CbWriteTypes.ItemIndex = 5 then
        AChar.SetValueAsUInt64(UInt64(StrToInt(EdCharacWrite.Text)));

      if CbWriteTypes.ItemIndex = 6 then
        AChar.SetValueAsInt8(Int8(StrToInt(EdCharacWrite.Text)));
      if CbWriteTypes.ItemIndex = 7 then
        AChar.SetValueAsInt16(Int16(StrToInt(EdCharacWrite.Text)));
      if CbWriteTypes.ItemIndex = 8 then
        AChar.SetValueAsInt32(Int32(StrToInt(EdCharacWrite.Text)));
      if CbWriteTypes.ItemIndex = 9 then
        AChar.SetValueAsInt64(Int64(StrToInt(EdCharacWrite.Text)));

      if CbWriteTypes.ItemIndex = 10 then
        AChar.SetValueAsDouble(StrToFloat(EdCharacWrite.Text));
      if CbWriteTypes.ItemIndex = 11 then
        AChar.SetValueAsSingle(StrToFloat(EdCharacWrite.Text));

      ADevice.WriteCharacteristic(AChar);
    end
    else
      Showmessage('This characteristic doesn''t allow Write');
  end
  else
    ShowMessage(EdCurrentDevice.Text + ' is not available');
end;

procedure TFrDeviceExplorer.Button1Click(Sender: TObject);
begin
  PbFindDevices.Max := 100;
  PbFindDevices.Value := 0;
  tmAnimateFindDevices.Enabled := True;
  FBluetoothManagerLE := TBluetoothLEManager.Current;
  FBluetoothManagerLE.OnDiscoveryEnd := DevicesDiscoveryLEEnd;
  FBluetoothManagerLE.StartDiscovery(2000);
end;

end.
