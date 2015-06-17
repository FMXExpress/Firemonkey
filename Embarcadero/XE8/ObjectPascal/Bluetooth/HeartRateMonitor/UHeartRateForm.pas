unit UHeartRateForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Ani, FMX.StdCtrls, System.Bluetooth, FMX.Layouts,
  FMX.Memo, FMX.Controls.Presentation, FMX.Edit, FMX.Objects, IPPeerClient, IPPeerServer,
  System.Tether.Manager, System.Bluetooth.Components, FMX.ListBox;

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
    LogList: TListBox;
    ListBoxGroupHeader1: TListBoxGroupHeader;
    ListBoxItem1: TListBoxItem;
    Memo1: TMemo;
    pnlMain: TPanel;
    MainList: TListBox;
    DeviceScan: TListBoxItem;
    lblDevice: TLabel;
    btnScan: TButton;
    BPM: TListBoxItem;
    lblBPM: TLabel;
    Image: TListBoxItem;
    imgHeart: TImage;
    Location: TListBoxItem;
    lblBodyLocation: TLabel;
    Status: TListBoxItem;
    lblContactStatus: TLabel;
    Monitoring: TListBoxItem;
    btnMonitorize: TButton;
    ToolBar1: TToolBar;
    Label1: TLabel;
    procedure btnScanClick(Sender: TObject);
    procedure btnMonitorizeClick(Sender: TObject);
    procedure btConnectClick(Sender: TObject);
    procedure BluetoothLE1EndDiscoverDevices(const Sender: TObject; const ADeviceList: TBluetoothLEDeviceList);
    procedure BluetoothLE1DescriptorRead(const Sender: TObject; const ADescriptor: TBluetoothGattDescriptor;
      AGattStatus: TBluetoothGattStatus);
    procedure BluetoothLE1CharacteristicRead(const Sender: TObject; const ACharacteristic: TBluetoothGattCharacteristic;
      AGattStatus: TBluetoothGattStatus);
  private
    { Private declarations }
    //FBluetoothLE: TBluetoothLE;
    //FBLEManager: TBluetoothLEManager;
    FBLEDevice: TBluetoothLEDevice;

    FHRGattService: TBluetoothGattService;
    FHRMeasurementGattCharact: TBluetoothGattCharacteristic;
    FBodySensorLocationGattCharact: TBluetoothGattCharacteristic;
   
    procedure GetServiceAndCharacteristics;

    procedure ManageCharacteristicData(const ACharacteristic: TBluetoothGattCharacteristic);
    procedure DisplayHeartRateMeasurementData(Data: TBytes);
    procedure DisplayBodySensorLocationData(Index: Byte);

    function GetFlags(Data: Byte): THRMFlags;
    procedure EnableHRMMonitorize(Enabled: boolean);
    procedure ReadBodySensorLocation;

    procedure ClearData;
    procedure DoScan;

  public
    { Public declarations }
  end;




const
  HRDeviceName = 'Cardiosport HRM';
  ServiceUUID = '';
  CharactUUID = '';

  HRSERVICE: TBluetoothUUID = '{0000180D-0000-1000-8000-00805F9B34FB}';
  HRMEASUREMENT_CHARACTERISTIC: TBluetoothUUID  = '{00002A37-0000-1000-8000-00805F9B34FB}';
  BODY_SENSOR_LOCATION_CHARACTERISTIC: TBluetoothUUID  = '{00002A38-0000-1000-8000-00805F9B34FB}';

  BodySensorLocations : array[0..6] of string = ('Other', 'Chest', 'Wrist', 'Finger', 'Hand', 'Ear Lobe', 'Foot');

  HR_VALUE_FORMAT_MASK = $1;
  SENSOR_CONTACT_STATUS_MASK = $6;
  ENERGY_EXPANDED_STATUS_MASK = $8;
  RR_INTERVAL_MASK = $10;



var
  frmHeartMonitor: TfrmHeartMonitor;

implementation

{$R *.fmx}

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

procedure TfrmHeartMonitor.EnableHRMMonitorize(Enabled: boolean);
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
      btnMonitorize.Text := 'Stop monitoring'
    end
    else
    begin
      BluetoothLE1.UnSubscribeToCharacteristic(FBLEDevice, FHRMeasurementGattCharact);
      btnMonitorize.Text := 'Start monitoring';
      ClearData;
    end;

    btnMonitorize.Enabled := True;
  end
  else begin
    Memo1.Lines.Add('HRM Characteristic not found');
    lblBPM.Font.Size := 13;
    lblBPM.Text := 'HRM Characteristic not found';
    btnMonitorize.Enabled := False;
  end;
end;

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
    lblBPM.Font.Size := 26;
    lblBPM.Text := 'Service not found';
  end;

  EnableHRMMonitorize(True);
  ReadBodySensorLocation;
end;

procedure TfrmHeartMonitor.ManageCharacteristicData(const ACharacteristic: TBluetoothGattCharacteristic);
begin
  if ACharacteristic.UUID = HRMEASUREMENT_CHARACTERISTIC then begin
    DisplayHeartRateMeasurementData(ACharacteristic.Value);
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
    Memo1.Lines.Add('FBodySensorLocationGattCharact not found!!!');
    lblBodyLocation.Text := 'Sensor location charact not found';
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
      lblBPM.Font.Size := 26;
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

procedure TfrmHeartMonitor.btnMonitorizeClick(Sender: TObject);
begin
  if btnMonitorize.Text.StartsWith('Stop') then
    EnableHRMMonitorize(False)
  else
    EnableHRMMonitorize(True)
end;

procedure TfrmHeartMonitor.btnScanClick(Sender: TObject);
begin
  DoScan;
end;

procedure TfrmHeartMonitor.ClearData;
begin
  lblBPM.Font.Size := 26;
  lblBPM.Text := '? bpm';
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
begin
  Flags := GetFlags(Data[0]);
  if Flags.HRValue16bits then
    LBPM := Data[1] + (Data[2] * 16)
  else
    LBPM := Data[1];

  case Flags.SensorContactStatus of
    NonSupported: lblContactStatus.Text := '';
    NonDetected: lblContactStatus.Text := 'Sensor contact non detected';
    Detected: lblContactStatus.Text := 'Sensor contact detected';
  end;

  if Flags.SensorContactStatus = NonDetected then
    ClearData
  else
  begin
    lblBPM.Font.Size := 26;
    lblBPM.Text := LBPM.ToString + ' bpm';
    imgHeart.Visible := not imgHeart.Visible;
  end;
end;

procedure TfrmHeartMonitor.DoScan;
begin
  ClearData;
  lblDevice.Text := '';
  lblBodyLocation.Text := '';
  lblContactStatus.Text := '';
  BluetoothLE1.DiscoverDevices(2500, [HRSERVICE]);
end;

end.
