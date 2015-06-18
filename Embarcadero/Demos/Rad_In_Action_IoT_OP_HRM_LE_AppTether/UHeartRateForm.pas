unit UHeartRateForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Ani,
  FMX.StdCtrls, System.Bluetooth, FMX.Layouts,
  FMX.Memo, FMX.Controls.Presentation, FMX.Edit, FMX.Objects, IPPeerClient,
  IPPeerServer,
  System.Tether.Manager, System.Bluetooth.Components, FMX.ListBox,
  System.Tether.AppProfile, System.SyncObjs;

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
    pnlAT: TPanel;
    ATList: TListBox;
    ListBoxGroupHeader2: TListBoxGroupHeader;
    ListBoxItem2: TListBoxItem;
    Memo2: TMemo;
    TetheringManager1: TTetheringManager;
    TetherProfile: TTetheringAppProfile;
    lbBPM: TListBox;
    btnSendBPM: TButton;
    tmSendBPM: TTimer;
    lblUsername: TLabel;
    edtUsername: TEdit;
    procedure btnScanClick(Sender: TObject);
    procedure btnMonitorizeClick(Sender: TObject);
    procedure btConnectClick(Sender: TObject);
    procedure BluetoothLE1EndDiscoverDevices(const Sender: TObject;
      const ADeviceList: TBluetoothLEDeviceList);
    procedure BluetoothLE1DescriptorRead(const Sender: TObject;
      const ADescriptor: TBluetoothGattDescriptor;
      AGattStatus: TBluetoothGattStatus);
    procedure BluetoothLE1CharacteristicRead(const Sender: TObject;
      const ACharacteristic: TBluetoothGattCharacteristic;
      AGattStatus: TBluetoothGattStatus);
    procedure FormCreate(Sender: TObject);
    procedure TetheringManager1RequestManagerPassword(const Sender: TObject;
      const ARemoteIdentifier: string; var Password: string);
    procedure TetheringManager1PairedToRemote(const Sender: TObject;
      const AManagerInfo: TTetheringManagerInfo);
    procedure TetheringManager1PairedFromLocal(const Sender: TObject;
      const AManagerInfo: TTetheringManagerInfo);
    procedure TetheringManager1EndManagersDiscovery(const Sender: TObject;
      const ARemoteManagers: TTetheringManagerInfoList);
    procedure TetheringManager1EndProfilesDiscovery(const Sender: TObject;
      const ARemoteProfiles: TTetheringProfileInfoList);
    procedure TetheringManager1NewManager(const Sender: TObject;
      const AManagerInfo: TTetheringManagerInfo);
    procedure TetheringManager1UnPairManager(const Sender: TObject;
      const AManagerInfo: TTetheringManagerInfo);
    procedure TetheringManager1RemoteManagerShutdown(const Sender: TObject;
      const AManagerIdentifier: string);
    procedure TetherProfileResourceUpdated(const Sender: TObject;
      const AResource: TRemoteResource);
    procedure TetherProfileResourceReceived(const Sender: TObject;
      const AResource: TRemoteResource);
    procedure FormDestroy(Sender: TObject);
    procedure TetherProfileDisconnect(const Sender: TObject;
      const AProfileInfo: TTetheringProfileInfo);

    procedure RefreshBPMList;
    procedure btnSendBPMClick(Sender: TObject);
    procedure tmSendBPMTimer(Sender: TObject);

  private
    { Private declarations }
    FBLEDevice: TBluetoothLEDevice;

    FHRGattService: TBluetoothGattService;
    FHRMeasurementGattCharact: TBluetoothGattCharacteristic;
    FBodySensorLocationGattCharact: TBluetoothGattCharacteristic;

    // App Tethering Shared Resource
    FMyBPM: String;

    FLock: TCriticalSection;
    FRemoteProfiles: TTetheringProfileInfoList;
    procedure Lock;
    procedure Unlock;

    procedure GetServiceAndCharacteristics;

    procedure ManageCharacteristicData(const ACharacteristic
      : TBluetoothGattCharacteristic);
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
  HRMEASUREMENT_CHARACTERISTIC
    : TBluetoothUUID = '{00002A37-0000-1000-8000-00805F9B34FB}';
  BODY_SENSOR_LOCATION_CHARACTERISTIC
    : TBluetoothUUID = '{00002A38-0000-1000-8000-00805F9B34FB}';

  BodySensorLocations: array [0 .. 6] of string = ('Other', 'Chest', 'Wrist',
    'Finger', 'Hand', 'Ear Lobe', 'Foot');

  HR_VALUE_FORMAT_MASK = $1;
  SENSOR_CONTACT_STATUS_MASK = $6;
  ENERGY_EXPANDED_STATUS_MASK = $8;
  RR_INTERVAL_MASK = $10;

var
  frmHeartMonitor: TfrmHeartMonitor;

implementation

{$R *.fmx}

procedure TfrmHeartMonitor.Lock;
begin
  FLock.Acquire;
end;

procedure TfrmHeartMonitor.Unlock;
begin
  FLock.Release;
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

function TfrmHeartMonitor.GetFlags(Data: Byte): THRMFlags;
var
  LValue: Byte;
begin
  Result.HRValue16bits := (Data and HR_VALUE_FORMAT_MASK) = 1;
  LValue := (Data and SENSOR_CONTACT_STATUS_MASK) shr 1;
  case LValue of
    2:
      Result.SensorContactStatus := NonDetected;
    3:
      Result.SensorContactStatus := Detected;
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
    if Enabled then
    begin
      BluetoothLE1.SubscribeToCharacteristic(FBLEDevice,
        FHRMeasurementGattCharact);
      btnMonitorize.Text := 'Stop monitoring'
    end
    else
    begin
      BluetoothLE1.UnSubscribeToCharacteristic(FBLEDevice,
        FHRMeasurementGattCharact);
      btnMonitorize.Text := 'Start monitoring';
      ClearData;
    end;

    btnMonitorize.Enabled := True;
  end
  else
  begin
    Memo1.Lines.Add('HRM Characteristic not found');
    lblBPM.Font.Size := 13;
    lblBPM.Text := 'HRM Characteristic not found';
    btnMonitorize.Enabled := False;
  end;
end;

procedure TfrmHeartMonitor.FormCreate(Sender: TObject);
begin
  // Assign a default value to the "MyBPM" Resource
  TetherProfile.Resources.FindByName('MyBPM').Value := 'Waiting for BPM...';

  // Create the Remote Profile Lock
  FLock := TCriticalSection.Create;

  // Create the Remote Profiles List
  FRemoteProfiles := TTetheringProfileInfoList.Create;

  // Log the unique LOCAL identifier so we can distinguish between instances
  // This is a unique GUID generated when our application is executed
  Memo2.Lines.Add('Local Identifier: ' + TetheringManager1.Identifier);
  // Now let's look for Remote Mangers with which to pair...
  Memo2.Lines.Add('Scanning for Remote Managers with which to pair...');
  TetheringManager1.DiscoverManagers;
end;

procedure TfrmHeartMonitor.FormDestroy(Sender: TObject);
begin
  // Destroy the Remote Profiles List and Lock
  FRemoteProfiles.Free;
  FLock.Free;

end;

procedure TfrmHeartMonitor.GetServiceAndCharacteristics;
var
  I, J, K: Integer;
begin
  for I := 0 to FBLEDevice.Services.Count - 1 do
  begin
    Memo1.Lines.Add(FBLEDevice.Services[I].UUIDName + ' : ' +
      FBLEDevice.Services[I].UUID.ToString);
    for J := 0 to FBLEDevice.Services[I].Characteristics.Count - 1 do
    begin
      Memo1.Lines.Add('--> ' + FBLEDevice.Services[I].Characteristics[J]
        .UUIDName + ' : ' + FBLEDevice.Services[I].Characteristics[J]
        .UUID.ToString);
      for K := 0 to FBLEDevice.Services[I].Characteristics[J]
        .Descriptors.Count - 1 do
      begin
        Memo1.Lines.Add('----> ' + FBLEDevice.Services[I].Characteristics[J]
          .Descriptors[K].UUIDName + ' : ' + FBLEDevice.Services[I]
          .Characteristics[J].Descriptors[K].UUID.ToString);
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
    FHRMeasurementGattCharact := BluetoothLE1.GetCharacteristic(FHRGattService,
      HRMEASUREMENT_CHARACTERISTIC);
    FBodySensorLocationGattCharact := BluetoothLE1.GetCharacteristic
      (FHRGattService, BODY_SENSOR_LOCATION_CHARACTERISTIC);
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

procedure TfrmHeartMonitor.ManageCharacteristicData(const ACharacteristic
  : TBluetoothGattCharacteristic);
begin
  if ACharacteristic.UUID = HRMEASUREMENT_CHARACTERISTIC then
  begin
    DisplayHeartRateMeasurementData(ACharacteristic.Value);
  end;

  if ACharacteristic.UUID = BODY_SENSOR_LOCATION_CHARACTERISTIC then
  begin
    DisplayBodySensorLocationData(ACharacteristic.Value[0]);
  end;
end;

procedure TfrmHeartMonitor.ReadBodySensorLocation;
begin
  if FBodySensorLocationGattCharact <> nil then
    BluetoothLE1.ReadCharacteristic(FBLEDevice, FBodySensorLocationGattCharact)
  else
  begin
    Memo1.Lines.Add('FBodySensorLocationGattCharact not found!!!');
    lblBodyLocation.Text := 'Sensor location charact not found';
  end;
end;

procedure TfrmHeartMonitor.TetheringManager1EndManagersDiscovery
  (const Sender: TObject; const ARemoteManagers: TTetheringManagerInfoList);
var
  I: Integer;
begin
  // Output the number of Remote Managers into the Memo.
  // Handle pairing with Remote Managers
  Memo2.Lines.Add(Format('Manager Discovery Complete, found %d Remote Managers',
    [ARemoteManagers.Count]));
  // Iterate through all of the discovered Remote Managers,
  // Check if the Text property matches that of the HRM Manager App.
  // If it matches, then we can pair with it;
  for I := 0 to ARemoteManagers.Count - 1 do
  begin
    // Log information about the Remote Manager
    Memo2.Lines.Add(Format('Discovered Remote Manager %d - %s' + #13#10 + #9 +
      'Manager Name: %s' + #13#10 + #9 + 'Manager Text: %s' + #13#10 + #9 +
      'Connection String: %s', [I, ARemoteManagers[I].ManagerIdentifier,
      ARemoteManagers[I].ManagerName, ARemoteManagers[I].ManagerText,
      ARemoteManagers[I].ConnectionString]));

    // Check if the Remote Manager's "Text" matches the Local Manager's
    if (ARemoteManagers[I].ManagerText = TetheringManager1.Text) then
    begin
      // Log that we're attempting to pair with this Remote Manager
      Memo2.Lines.Add
        ('Remote Manager matches Local Manger, attempting to pair...');
      // Pair with the Remote Manager
      TetheringManager1.PairManager(ARemoteManagers[I]);
    end
    else
    begin
      // Log that this Remote Manager is of no interest
      Memo2.Lines.Add
        ('Remote Manager does not match Local Manager, ignoring it...');
    end;
  end;
  // Once the Local Manager pairs with a Remote Manager,
  // the Profile Discovery process automatically takes place on both sides.

end;

procedure TfrmHeartMonitor.TetheringManager1EndProfilesDiscovery
  (const Sender: TObject; const ARemoteProfiles: TTetheringProfileInfoList);
var
  LRemoteProfile: TTetheringProfileInfo;
begin
  Memo2.Lines.Add(Format('Profile Discovery Complete, Found %d Remote Profiles',
    [ARemoteProfiles.Count]));
  // Lock the container
  Lock;
  try
    // Iterate all discovered profiles
    for LRemoteProfile in ARemoteProfiles do
    begin
      // If the profile isn't already in the list...
      if (not FRemoteProfiles.Contains(LRemoteProfile)) then
      begin
        // If we can connect to the Remote Profile
        if TetherProfile.Connect(LRemoteProfile) then
        begin
          // We need to Subscribe to each Remote Profile‘s BPM resource.
          // This will enable the OnResourceUpdated event to update the BPM list.
          // Each time a Remote Profile changes the value of its BPM resource,
          // our Local Profile‘s OnResourceUpdated event is automatically fired,
          // thus refreshing our BPM list.
          TetherProfile.SubscribeToRemoteItem(LRemoteProfile,
            TetherProfile.GetRemoteResourceValue(LRemoteProfile, 'MyBPM'));
          // Add it into our lockable container
          FRemoteProfiles.Add(LRemoteProfile);
          // Log the Remote Profile
          Memo2.Lines.Add(Format('Added Remote Profile %s to the Lockable List',
            [LRemoteProfile.ProfileIdentifier]));
        end;
      end;
    end;
    // Re-sort the lockable container
    FRemoteProfiles.Sort;
  finally
    // Unlock the container
    Unlock;
  end;
end;

procedure TfrmHeartMonitor.TetheringManager1NewManager(const Sender: TObject;
  const AManagerInfo: TTetheringManagerInfo);
begin
  // Log that we've paired to a Remote Manager and provide details
  Memo2.Lines.Add(Format('New Remote Manager %s' + #13#10 + #9 +
    'Manager Name: %s' + #13#10 + #9 + 'Manager Text: %s' + #13#10 + #9 +
    'Connection String: %s', [AManagerInfo.ManagerIdentifier,
    AManagerInfo.ManagerName, AManagerInfo.ManagerText,
    AManagerInfo.ConnectionString]));

end;

procedure TfrmHeartMonitor.TetheringManager1PairedFromLocal
  (const Sender: TObject; const AManagerInfo: TTetheringManagerInfo);
begin
  // Log that we've paired to a Remote Manager and provide details
  Memo2.Lines.Add(Format('A Remote Manager %s has paired with us' + #13#10 + #9
    + 'Manager Name: %s' + #13#10 + #9 + 'Manager Text: %s' + #13#10 + #9 +
    'Connection String: %s', [AManagerInfo.ManagerIdentifier,
    AManagerInfo.ManagerName, AManagerInfo.ManagerText,
    AManagerInfo.ConnectionString]));

end;

procedure TfrmHeartMonitor.TetheringManager1PairedToRemote
  (const Sender: TObject; const AManagerInfo: TTetheringManagerInfo);
begin
  // Log that we've paired to a Remote Manager and provide details
  Memo2.Lines.Add(Format('We have paired with a Remote Manager %s' + #13#10 + #9
    + 'Manager Name: %s' + #13#10 + #9 + 'Manager Text: %s' + #13#10 + #9 +
    'Connection String: %s', [AManagerInfo.ManagerIdentifier,
    AManagerInfo.ManagerName, AManagerInfo.ManagerText,
    AManagerInfo.ConnectionString]));

end;

procedure TfrmHeartMonitor.TetheringManager1RemoteManagerShutdown
  (const Sender: TObject; const AManagerIdentifier: string);
begin
  // Logs the identity of a Remote Manager that has shut down.
  Memo2.Lines.Add(Format('Remote Manager %s Shutdown', [AManagerIdentifier]));
end;

procedure TfrmHeartMonitor.TetheringManager1RequestManagerPassword
  (const Sender: TObject; const ARemoteIdentifier: string;
  var Password: string);
begin
  // Log the request
  // Set Password = “1234”
  TThread.Synchronize(TThread.CurrentThread,
    procedure
    begin
      Memo1.Lines.Add(Format('Remote Manager %s requested for a password...',
        [ARemoteIdentifier]));
    end);
  Password := '1234';

end;

procedure TfrmHeartMonitor.TetheringManager1UnPairManager(const Sender: TObject;
const AManagerInfo: TTetheringManagerInfo);
begin
  // Logs information about a Remote Manager from which we have UnPaired.
  Memo2.Lines.Add(Format('UnPaired with Remote Manager %s' + #13#10 + #9 +
    'Manager Name: %s' + #13#10 + #9 + 'Manager Text: %s' + #13#10 + #9 +
    'Connection String: %s', [AManagerInfo.ManagerIdentifier,
    AManagerInfo.ManagerName, AManagerInfo.ManagerText,
    AManagerInfo.ConnectionString]));

end;

procedure TfrmHeartMonitor.TetherProfileDisconnect(const Sender: TObject;
const AProfileInfo: TTetheringProfileInfo);
var
  LIndex: Integer;
  LProfileInfo: TTetheringProfileInfo;
begin
  // Engage the Lock
  Lock;
  try
    // If the Profile is in the list...
    if FRemoteProfiles.BinarySearch(AProfileInfo, LIndex) then
    begin
      // ...remove it
      FRemoteProfiles.Delete(LIndex);
      // Log that we've removed it
      LProfileInfo := AProfileInfo;
      TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          Memo2.Lines.Add
            (Format('Removed Remote Profile %s from the lockable container',
            [LProfileInfo.ProfileIdentifier]));
        end);
    end;
  finally
    // Disengage the Lock
    Unlock;
  end;
end;

procedure TfrmHeartMonitor.TetherProfileResourceReceived(const Sender: TObject;
const AResource: TRemoteResource);
// Use this Event to send our HR BPM (Temporary Resources)
// to the App Tethered Paired Apps
begin
  Memo2.Lines.Add(Format('Temporary Resource Received: %s', [AResource.Hint]));
  // if it's a String resource...
  if (AResource.Hint = 'BPM') and
    (AResource.Value.DataType = TResourceType.String) then
  begin
    // Add the message to the display
    Memo1.Lines.Add(AResource.Value.AsString);
  end;
end;

procedure TfrmHeartMonitor.TetherProfileResourceUpdated(const Sender: TObject;
const AResource: TRemoteResource);
// Fires when the Resource (MyBPM) gets updated in the Remote Profile.
// Use this event to send the HR BPM as Persistent Resources
// to the App Tethered Paired apps.
begin
  // Log that a Remote Resource has been updated
  Memo2.Lines.Add(Format('Remote Resource Updated: %s', [AResource.Name]));
  // if this is the MyBPM resource...
  if AResource.Name = 'MyBPM' then
    // Refresh the BPM list
    RefreshBPMList;
end;

procedure TfrmHeartMonitor.tmSendBPMTimer(Sender: TObject);
var
  LRemoteProfile: TTetheringProfileInfo;
  LBPM: String;
  RecordedTime: string;
begin
  // Format the Message String
  RecordedTime := FormatDateTime('  mm/dd/yyyy hh:nn:ss', Now);
  LBPM := edtUsername.Text + ' ' + lblBPM.Text + RecordedTime;
  // Add the message to our own UI.
  Memo1.Lines.Add(LBPM);
  // Lock the Container
  Lock;
  try
    // Iterate all Remote Profiles
    for LRemoteProfile in FRemoteProfiles do
    begin
      // Send the updated BPM Resource to all the connected Tethered Apps.
      TetherProfile.SendString(LRemoteProfile, 'BPM', LBPM);
    end;
  finally
    // Unlock the Container
    Unlock;
  end;
end;

procedure TfrmHeartMonitor.BluetoothLE1CharacteristicRead(const Sender: TObject;
const ACharacteristic: TBluetoothGattCharacteristic;
AGattStatus: TBluetoothGattStatus);
var
  LSValue: string;
begin
  if AGattStatus <> TBluetoothGattStatus.Success then
    Memo1.Lines.Add('Error reading Characteristic ' + ACharacteristic.UUIDName +
      ': ' + Ord(AGattStatus).ToString)
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
    Memo1.Lines.Add('Error reading Characteristic ' + ADescriptor.UUIDName +
      ': ' + Ord(AGattStatus).ToString)
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
  Memo1.Lines.Add(ADeviceList.Count.ToString + ' devices discovered:');
  for I := 0 to ADeviceList.Count - 1 do
    Memo1.Lines.Add(ADeviceList[I].DeviceName);

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
  begin
    tmSendBPM.Enabled := False;
    EnableHRMMonitorize(False)
  end
  else
    EnableHRMMonitorize(True)
end;

procedure TfrmHeartMonitor.btnScanClick(Sender: TObject);
begin
  DoScan;
end;

procedure TfrmHeartMonitor.btnSendBPMClick(Sender: TObject);
var
  LRemoteProfile: TTetheringProfileInfo;
  LBPM: String;
  RecordedTime: string;
begin
  // Format the Message String
  LBPM := Format('%s - %s', ['Alfonso', '88']);
  RecordedTime := FormatDateTime('  mm/dd/yyyy hh:nn:ss', Now);
  LBPM := edtUsername.Text + ' ' + lblBPM.Text + RecordedTime;
  // Add the message to our OWN display
  Memo1.Lines.Add(LBPM);
  // Lock the Container
  Lock;
  try
    // Iterate all Remote Profiles
    for LRemoteProfile in FRemoteProfiles do
    begin
      // Send the Message
      TetherProfile.SendString(LRemoteProfile, 'BPM', LBPM);
    end;
  finally
    // Unlock the Container
    Unlock;
  end;
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
  RecordedTime: string;
begin
  Flags := GetFlags(Data[0]);
  if Flags.HRValue16bits then
    LBPM := Data[1] + (Data[2] * 16)
  else
    LBPM := Data[1];

  case Flags.SensorContactStatus of
    NonSupported:
      lblContactStatus.Text := '';
    NonDetected:
      lblContactStatus.Text := 'Sensor contact non detected';
    Detected:
      lblContactStatus.Text := 'Sensor contact detected';
  end;

  if Flags.SensorContactStatus = NonDetected then
    ClearData
  else
  begin
    RecordedTime := FormatDateTime('  mm/dd/yyyy hh:nn:ss', Now);
    lblBPM.Font.Size := 26;
    lblBPM.Text := LBPM.ToString + ' bpm';
    imgHeart.Visible := not imgHeart.Visible;
    Memo1.Lines.Add(edtUsername.Text + '  ' + lblBPM.Text + RecordedTime);
  end;
end;

procedure TfrmHeartMonitor.DoScan;
begin
  ClearData;
  lblDevice.Text := '';
  lblBodyLocation.Text := '';
  lblContactStatus.Text := '';
  tmSendBPM.Enabled := True;
  BluetoothLE1.DiscoverDevices(2500, [HRSERVICE]);
end;

procedure TfrmHeartMonitor.RefreshBPMList;
var
  LRemoteProfile: TTetheringProfileInfo;
  LRemoteBPM: String;
begin
  // Clear the list
  lbBPM.Clear;
  // Lock the container
  Lock;
  try
    // Iterate all Connected Remote Profiles
    for LRemoteProfile in FRemoteProfiles do
    begin
      // Retreive the BPM
      LRemoteBPM := TetherProfile.GetRemoteResourceValue(LRemoteProfile,
        'MyBPM').Value.AsString;
      // If the BPM has been specified
      if LRemoteBPM <> 'Waiting for BPM...' then
      begin
        // Add the BPM to the list
        lbBPM.Items.Add(LRemoteBPM);
      end;
    end;
    // Unlock the container
  finally
    Unlock;
  end;
end;

end.
