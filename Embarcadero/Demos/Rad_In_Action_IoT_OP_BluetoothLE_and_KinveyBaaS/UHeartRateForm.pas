unit UHeartRateForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Ani, FMX.StdCtrls, System.Bluetooth, FMX.Layouts,
  FMX.Memo, FMX.Controls.Presentation, FMX.Edit, FMX.Objects, IPPeerClient, IPPeerServer,
  System.Tether.Manager, System.Bluetooth.Components, FMX.ListBox,
  REST.Backend.ServiceTypes, REST.Backend.MetaTypes, System.JSON,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  REST.OpenSSL, REST.Backend.KinveyProvider, Data.Bind.Components,
  Data.Bind.ObjectScope, REST.Backend.BindSource,
  REST.Backend.ServiceComponents, REST.Response.Adapter, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, REST.Backend.Providers,
  REST.Backend.KinveyServices, System.Rtti, System.Bindings.Outputs,
  Fmx.Bind.Editors, Data.Bind.EngExt, Fmx.Bind.DBEngExt, Data.Bind.DBScope,
  FMXTee.Engine, FMXTee.Procs, FMXTee.Chart, FMX.TabControl, FMXTee.Series, System.DateUtils,
 FMX.ListView.Types, FMX.ListView, FMX.EditBox, FMX.NumberBox,
  REST.Backend.PushTypes, System.Threading;

type

  TSensorContactStatus = (NonSupported, NonDetected, Detected);

  THRMFlags = record
    HRValue16bits: boolean;
    SensorContactStatus: TSensorContactStatus;
    EnergyExpended: boolean;
    RRInterval: boolean;
  end;

  TfrmHeartMonitor = class(TForm)
    BluetoothLE1: TBluetoothLE;
    pnlLog: TPanel;
    pnlMain: TPanel;
    btnScan: TButton;
    lblBPM: TLabel;
    imgHeart: TImage;
    lblBodyLocation: TLabel;
    lblContactStatus: TLabel;
    btnMonitoring: TButton;
    ToolBar1: TToolBar;
    Label1: TLabel;
    SpeedButton2: TSpeedButton;
    TabControl1: TTabControl;
    Measure: TTabItem;
    History: TTabItem;
    Memo1: TMemo;
    Image1: TImage;
    lblDevice: TEdit;
    HeartrateChart: TChart;
    FDMemTable1: TFDMemTable;
    RESTResponseDataSetAdapter1: TRESTResponseDataSetAdapter;
    FDMemTable1bpmlog: TWideStringField;
    FDMemTable1time: TStringField;
    ToolBar2: TToolBar;
    Label2: TLabel;
    SpeedButton1: TSpeedButton;
    BluetoothListDetails: TListBox;
    ListBoxGroupHeader1: TListBoxGroupHeader;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    ListBoxGroupHeader2: TListBoxGroupHeader;
    Settings: TTabItem;
    BindingsList1: TBindingsList;
    BindSourceDB1: TBindSourceDB;
    LinkFillControlToField1: TLinkFillControlToField;
    Image2: TImage;
    ToolBar3: TToolBar;
    Label3: TLabel;
    ListView1: TListView;
    ListImage: TImageControl;
    ImageControl1: TImageControl;
    BackendPush1: TBackendPush;
    BackendQuery1: TBackendQuery;
    BackendStorage1: TBackendStorage;
    Series1: TFastLineSeries;
    FDMemTable1username: TStringField;
    FDMemTable1threshold: TIntegerField;
    Timer1: TTimer;
    Label4: TLabel;
    ListView2: TListView;
    Memo2: TMemo;
    LinkListControlToField1: TLinkListControlToField;
    procedure btnScanClick(Sender: TObject);
    procedure btnMonitoringClick(Sender: TObject);
    procedure btConnectClick(Sender: TObject);
    procedure BluetoothLE1EndDiscoverDevices(const Sender: TObject; const ADeviceList: TBluetoothLEDeviceList);
    procedure BluetoothLE1DescriptorRead(const Sender: TObject; const ADescriptor: TBluetoothGattDescriptor;
      AGattStatus: TBluetoothGattStatus);
    procedure BluetoothLE1CharacteristicRead(const Sender: TObject; const ACharacteristic: TBluetoothGattCharacteristic;
      AGattStatus: TBluetoothGattStatus);
    procedure FormCreate(Sender: TObject);
    procedure HistoryClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure LinkListControlToField1FilledListItem(Sender: TObject;
      const AEditor: IBindListEditorItem);
  private
    { Private declarations }
    FBLEDevice: TBluetoothLEDevice;

    FHRGattService: TBluetoothGattService;
    FHRMeasurementGattCharact: TBluetoothGattCharacteristic;
    FBodySensorLocationGattCharact: TBluetoothGattCharacteristic;
   
    procedure GetServiceAndCharacteristics;

    procedure ManageCharacteristicData(const ACharacteristic: TBluetoothGattCharacteristic);
    procedure DisplayHeartRateMeasurementData(Data: TBytes);
    procedure DisplayBodySensorLocationData(Index: Byte);

    function GetFlags(Data: Byte): THRMFlags;
    procedure EnableHRMMonitoring(Enabled: boolean);
    procedure ReadBodySensorLocation;

    procedure ClearData;
    procedure DoScan;
    procedure SaveinBackground;
    procedure UpdateGraph;

  public
    { Public declarations }
  end;


const
  HRDeviceName = 'Cardiosport HRM';
  ServiceUUID = '';
  CharactUUID = '';

  // From the Bluetooth website under Home > GATT Specifications > Services > ServiceViewer
  // This service exposes heart rate and other data from a Heart Rate Sensor
  HRSERVICE: TBluetoothUUID = '{0000180D-0000-1000-8000-00805F9B34FB}';

  // From the Bluetooth website under Home > GATT Specifications > Characteristics > CharacteristicViewer
  HRMEASUREMENT_CHARACTERISTIC: TBluetoothUUID  = '{00002A37-0000-1000-8000-00805F9B34FB}';
  BODY_SENSOR_LOCATION_CHARACTERISTIC: TBluetoothUUID  = '{00002A38-0000-1000-8000-00805F9B34FB}';

  //Body Sensor Location Characteristic value used to index the array to display the string
  BodySensorLocations : array[0..6] of string = ('Other', 'Chest', 'Wrist', 'Finger', 'Hand', 'Ear Lobe', 'Foot');

  HR_VALUE_FORMAT_MASK = $1;
  SENSOR_CONTACT_STATUS_MASK = $6;
  ENERGY_EXPANDED_STATUS_MASK = $8;
  RR_INTERVAL_MASK = $10;



var
  frmHeartMonitor: TfrmHeartMonitor;

implementation

{$R *.fmx}

uses Login;

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

function TfrmHeartMonitor.GetFlags(Data: Byte): THRMFlags;
var
  LValue: Byte;
begin
  Result.HRValue16bits := (Data and HR_VALUE_FORMAT_MASK) = 1;
  LValue := (Data and SENSOR_CONTACT_STATUS_MASK) shr 1;
  case LValue of
    2: Result.SensorContactStatus := NonDetected;
    3: Result.SensorContactStatus := Detected;
    else
      Result.SensorContactStatus := NonSupported;
  end;
  Result.EnergyExpended := ((Data and ENERGY_EXPANDED_STATUS_MASK) shr 3) = 1;
  Result.RRInterval := ((Data and RR_INTERVAL_MASK) shr 4) = 1;
end;

procedure TfrmHeartMonitor.EnableHRMMonitoring(Enabled: boolean);
begin
  if FHRMeasurementGattCharact <> nil then
  begin
{$IFDEF MSWINDOWS}
{
    LDescriptor := FHRMeasurementGattCharact.Descriptors[0];
    LDescriptor.Notification := Enabled;
    FBluetoothLE.WriteDescriptor(FBLEDevice, LDescriptor);
    }
{$ENDIF}
    if Enabled then
    begin
      BluetoothLE1.SubscribeToCharacteristic(FBLEDevice, FHRMeasurementGattCharact);
      btnMonitoring.Text := 'Stop'
    end
    else
    begin
      BluetoothLE1.UnSubscribeToCharacteristic(FBLEDevice, FHRMeasurementGattCharact);
      btnMonitoring.Text := 'Start';
      ClearData;
    end;

    btnMonitoring.Enabled := True;
  end
  else begin
    Memo1.Lines.Add('HRM Characteristic not found');
    lblBPM.Font.Size := 13;
    lblBPM.Text := 'HRM Characteristic not found';
    btnMonitoring.Enabled := False;
  end;
end;

procedure TfrmHeartMonitor.FormCreate(Sender: TObject);
begin
  BackendQuery1.Execute;
  TabControl1.ActiveTab := Measure;
end;



//finds and lists available services and characteristics, and searching for specific ones, i.e. heart rate and position
procedure TfrmHeartMonitor.GetServiceAndCharacteristics;
var
  I, J, K: Integer;
begin
  for I := 0 to FBLEDevice.Services.Count - 1 do
  begin
    Memo1.Lines.Add(FBLEDevice.Services[I].UUIDName + ' : ' + FBLEDevice.Services[I].UUID.ToString);
    for J := 0 to FBLEDevice.Services[I].Characteristics.Count - 1 do begin
      Memo1.Lines.Add('--> ' + FBLEDevice.Services[I].Characteristics[J].UUIDName + ' : ' +
                      FBLEDevice.Services[I].Characteristics[J].UUID.ToString);
      for K := 0 to FBLEDevice.Services[I].Characteristics[J].Descriptors.Count - 1 do begin
        Memo1.Lines.Add('----> ' + FBLEDevice.Services[I].Characteristics[J].Descriptors[K].UUIDName + ' : ' +
                      FBLEDevice.Services[I].Characteristics[J].Descriptors[K].UUID.ToString);
      end;
    end;
  end;

  FHRGattService := nil;
  FHRMeasurementGattCharact := nil;
  FBodySensorLocationGattCharact := nil;

  FHRGattService := BluetoothLE1.GetService(FBLEDevice, HRSERVICE);
  if FHRGattService <> nil then
  begin
    Memo1.Lines.Add('Service found');
    FHRMeasurementGattCharact := BluetoothLE1.GetCharacteristic(FHRGattService, HRMEASUREMENT_CHARACTERISTIC);
    FBodySensorLocationGattCharact := BluetoothLE1.GetCharacteristic(FHRGattService, BODY_SENSOR_LOCATION_CHARACTERISTIC);
  end
  else
  begin
    Memo1.Lines.Add('Service not found');
    lblBPM.Font.Size := 14;
    lblBPM.Text := 'Service not found';
  end;

  EnableHRMMonitoring(True);
  ReadBodySensorLocation;
end;



procedure TfrmHeartMonitor.HistoryClick(Sender: TObject);
begin
         UpdateGraph;
end;



// update graph method
procedure TfrmHeartMonitor.UpdateGraph;
var
  i : integer;
begin
     BackendQuery1.Execute;
      Series1.Clear;
      FDMemTable1.First;
          for I := 0 to FDMemTable1.RecordCount -1 do begin
          Series1.AddY(FDMemTable1.FieldByName('bpmlog').Value);
          FDMemTable1.Next;
end;
end;



//adds heatbeat icon to each recorded heartrate entry on History tab
procedure TfrmHeartMonitor.LinkListControlToField1FilledListItem(
  Sender: TObject; const AEditor: IBindListEditorItem);
var
  LItem: TListViewItem;
begin
  LItem := AEditor.CurrentObject As TListViewItem;
  LItem.BitmapRef := ImageControl1.Bitmap;
end;




procedure TfrmHeartMonitor.ManageCharacteristicData(const ACharacteristic: TBluetoothGattCharacteristic);
begin
  if ACharacteristic.UUID = HRMEASUREMENT_CHARACTERISTIC then begin
    DisplayHeartRateMeasurementData(ACharacteristic.Value)
  end;

  if ACharacteristic.UUID = BODY_SENSOR_LOCATION_CHARACTERISTIC then begin
    DisplayBodySensorLocationData(ACharacteristic.Value[0]);
  end;
end;

procedure TfrmHeartMonitor.ReadBodySensorLocation;
begin
  if FBodySensorLocationGattCharact<>nil then
    BluetoothLE1.ReadCharacteristic(FBLEDevice, FBodySensorLocationGattCharact)
  else begin
    Memo1.Lines.Add('FBodySensorLocationGattCharacteristic not found!!!');
    lblBodyLocation.Text := 'Sensor location characteristic not found';
  end;
end;


//calls timer when user is on History tab and updates data shown on graph every second
procedure TfrmHeartMonitor.Timer1Timer(Sender: TObject);
begin
  if TabControl1.ActiveTab = History  then
  begin
  UpdateGraph;
  Timer1.Enabled := False;
end;
end;


procedure TfrmHeartMonitor.BluetoothLE1CharacteristicRead(const Sender: TObject;
  const ACharacteristic: TBluetoothGattCharacteristic; AGattStatus: TBluetoothGattStatus);
var
  LSValue: string;
begin
  if AGattStatus <> TBluetoothGattStatus.Success then
    Memo1.Lines.Add('Error reading Characteristic ' + ACharacteristic.UUIDName + ': ' + Ord(AGattStatus).ToString)
  else
  begin
    LSValue := BytesToString(ACharacteristic.Value);
    Memo1.Lines.Add(ACharacteristic.UUIDName + ' Value: ' + LSValue);
    ManageCharacteristicData(ACharacteristic);
  end;
end;

procedure TfrmHeartMonitor.BluetoothLE1DescriptorRead(const Sender: TObject;
  const ADescriptor: TBluetoothGattDescriptor; AGattStatus: TBluetoothGattStatus);
var
  LSValue: string;
begin
  if AGattStatus <> TBluetoothGattStatus.Success then
    Memo1.Lines.Add('Error reading Characteristic ' + ADescriptor.UUIDName + ': ' + Ord(AGattStatus).ToString)
  else
  begin
    LSValue := BytesToString(ADescriptor.GetValue);
    Memo1.Lines.Add(ADescriptor.UUIDName + ' Value: ' + LSValue);
  end;
end;

procedure TfrmHeartMonitor.BluetoothLE1EndDiscoverDevices(const Sender: TObject;
  const ADeviceList: TBluetoothLEDeviceList);
var
  I: Integer;
begin
  // log
  Memo1.Lines.Add(ADeviceList.Count.ToString +  ' devices discovered:');
  for I := 0 to ADeviceList.Count - 1 do Memo1.Lines.Add(ADeviceList[I].DeviceName);

  if BluetoothLE1.DiscoveredDevices.Count > 0 then
  begin
    FBLEDevice := BluetoothLE1.DiscoveredDevices.First;
    lblDevice.Text := HRDeviceName;
    if BluetoothLE1.GetServices(FBLEDevice).Count = 0 then
    begin
      Memo1.Lines.Add('No services found!');
      lblBPM.Font.Size := 14;
      lblBPM.Text := 'No services found!';
    end
    else
      GetServiceAndCharacteristics;
  end
  else
    lblDevice.Text := 'Device not found';
end;

procedure TfrmHeartMonitor.btConnectClick(Sender: TObject);
begin
  GetServiceAndCharacteristics;
end;

procedure TfrmHeartMonitor.btnMonitoringClick(Sender: TObject);
begin
  if btnMonitoring.Text.StartsWith('Stop') then
    EnableHRMMonitoring(False)
  else
    EnableHRMMonitoring(True)
end;

procedure TfrmHeartMonitor.btnScanClick(Sender: TObject);
begin
  DoScan;
end;

procedure TfrmHeartMonitor.ClearData;
begin
  lblBPM.Font.Size := 14;
  lblBPM.Text := 'Scanning ...';
  imgHeart.Visible := False;
end;

procedure TfrmHeartMonitor.DisplayBodySensorLocationData(Index: Byte);
begin
  if Index > 6 then
    lblBodyLocation.Text := ''
  else
    lblBodyLocation.Text := 'Sensor location: ' + BodySensorLocations[Index];
end;

procedure TfrmHeartMonitor.DisplayHeartRateMeasurementData(Data: TBytes);
var
  Flags: THRMFlags;
  LBPM: Integer;
  RecordedTime: string;
begin
  Flags := GetFlags(Data[0]);
  if Flags.HRValue16bits then
    LBPM := Data[1] + (Data[2] * 16)
  else
    LBPM := Data[1];
  case Flags.SensorContactStatus of
    NonSupported: lblContactStatus.Text := '';
    NonDetected: lblContactStatus.Text := 'Sensor contact not detected';
    Detected: lblContactStatus.Text := 'Sensor contact detected';
  end;

  if Flags.SensorContactStatus = NonDetected then
    ClearData
  else
  begin
  // displays current heartrate on label, and adds heartrate data to listview
    lblBPM.Font.Size := 14;
    lblBPM.Text := LBPM.ToString;
    RecordedTime:= FormatDateTime('  mm/dd/yyyy hh:nn', Now);
    ListView2.Visible := True;
    ListView2.Items.Add.Text := lblBPM.Text + RecordedTime;
    imgHeart.Visible := not imgHeart.Visible;
    SaveinBackground;  //calls SaveinBackground method to store new heartrate data

    //sends a push notification if heartrate exceeds threshold set on the login form
    BackendPush1.GCM.Message := 'Patient ID: ' +  Loginform.Username.Text + ', Logged Heartrate ' + lblBPM.Text;
    BackendPush1.GCM.Title := 'High Heartrate Notification';
  if StrToInt(lblBPM.Text) > loginForm.BPMNotify.Value then BackendPush1.Push;
  end;
end;

//use of threading to executing storing continuous heartrate data in the cloud while using app
procedure TfrmHeartMonitor.SaveinBackground;
var
  API: TBackendStorageAPI;
  LJSON : TJSONObject;
  ACreatedObject: TBackendEntityValue;
  RecordedTime: string;
begin
    RecordedTime:= FormatDateTime('  mm/dd/yyyy hh:nn', Now);
    API := BackendStorage1.CreateStorageAPI;
    LJSON := TJSONObject.Create;
    LJSON.AddPair('bpmlog', lblBPM.Text);
    LJSON.AddPair('time', RecordedTime);
    LJSON.AddPair('username', Loginform.Username.Text);
    Timer1.Enabled := True;
    TTask.Run(
   procedure
   begin
        API.CreateObject('heartratecloud', LJSON, ACreatedObject);
        API.Free;
    end)
end;

// scans for available heartrate monitors to connect to
procedure TfrmHeartMonitor.DoScan;
begin
  ClearData;
  lblDevice.Text := '';
  lblBodyLocation.Text := '';
  lblContactStatus.Text := '';
  BluetoothLE1.DiscoverDevices(2500, [HRSERVICE]);
end;

end.
