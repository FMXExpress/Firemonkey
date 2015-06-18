unit UProximityClient;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Math,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Layouts, FMX.Memo, System.Bluetooth,
  FMX.Controls.Presentation, FMX.Edit, FMX.ListBox;

type

  TPosition = (poNear, poFar, poSoFar, poUnknown);

  TfrmProximityForm = class(TForm)
    pnlLog: TPanel;
    Memo1: TMemo;
    pnlMain: TPanel;
    btnScan: TButton;
    lblDevice: TLabel;
    Splitter1: TSplitter;
    tmrReadRSSI: TTimer;
    lblRSSI: TLabel;
    lblDistance: TLabel;
    lblDist2: TLabel;
    lblTxPower: TLabel;
    lblLinkLossAlert: TLabel;
    lblImmediateAlert: TLabel;
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lblPosition: TLabel;
    btnConnect: TButton;
    procedure btnScanClick(Sender: TObject);
    procedure tmrReadRSSITimer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
  private
    { Private declarations }
    FBLEManager: TBluetoothLEManager;
    FBLEDevice: TBluetoothLEDevice;
    FServicesDiscovered: boolean;
    FTxPowerValue: Integer;

    FTXPowerService: TBluetoothGattService;
    FImmediateAlertService: TBluetoothGattService;
    FLinkLossService: TBluetoothGattService;

    FTXPowerLevelCharact: TBluetoothGattCharacteristic;
    FImmediateAlertLevelCharact: TBluetoothGattCharacteristic;
    FLinkLossAlertLevelCharact: TBluetoothGattCharacteristic;

    FCurrentPosition: TPosition;
    FNearCount: Integer;
    FFarCount: Integer;
    FSoFarCount: Integer;


    procedure DoDiscoveryEndEvent(const Sender: TObject; const ADeviceList: TBluetoothLEDeviceList);
    procedure DoServicesDiscovered(const Sender: TObject; const AServiceList: TBluetoothGattServiceList);
    procedure DoCharacteristicRead(const Sender: TObject; const ACharacteristic: TBluetoothGattCharacteristic;
                                  AGattStatus: TBluetoothGattStatus);
    procedure DoCharacteristicWrite(const Sender: TObject; const ACharacteristic: TBluetoothGattCharacteristic;
                                  AGattStatus: TBluetoothGattStatus);
    procedure DoReadRSSI(const Sender: TObject; ARssiValue: Integer; AGattStatus: TBluetoothGattStatus);
    procedure DoScan;
    procedure GetServiceAndCharacteristics;
    procedure EnableRSSIMonitorize(Enabled: boolean);
    procedure WriteLinkLossAlertLevel(AlertLevel: Byte);
    procedure WriteImmediateAlertLevel(AlertLevel: Byte);
    procedure CheckDistanceThreshold(PathLoss: Integer);
    procedure SetPosition(Position: TPosition);
    procedure UpdateCurrentPosition(Position: TPosition);
  public
    { Public declarations }
  end;

const
  CProximityDeviceName = 'POWERLINK';

  ServiceUUID = '';
  CharactUUID = '';

  LINK_LOSS_SERVICE: TBluetoothUUID = '{00001803-0000-1000-8000-00805F9B34FB}';
  IMMEDIATE_ALERT_SERVICE: TBluetoothUUID = '{00001802-0000-1000-8000-00805F9B34FB}';
  TX_POWER_SERVICE: TBluetoothUUID = '{00001804-0000-1000-8000-00805F9B34FB}';

  ALERT_LEVEL_CHARACTERISTIC: TBluetoothUUID  = '{00002A06-0000-1000-8000-00805F9B34FB}';
  TX_POWER_LEVEL_CHARACTERISTIC: TBluetoothUUID  = '{00002A07-0000-1000-8000-00805F9B34FB}';


var
  frmProximityForm: TfrmProximityForm;
  ProximityDeviceName: string;

implementation

{$R *.fmx}


{ TForm6 }

procedure TfrmProximityForm.btnConnectClick(Sender: TObject);
begin
  if FBLEDevice.Services.Count = 0 then
    Memo1.Lines.Add('No services found!')
  else
    GetServiceAndCharacteristics;
end;

procedure TfrmProximityForm.btnScanClick(Sender: TObject);
begin
  btnConnect.Enabled := False;
  DoScan;
end;

procedure TfrmProximityForm.Button1Click(Sender: TObject);
begin
  WriteLinkLossAlertLevel(strtoint(Edit1.Text));
end;

procedure TfrmProximityForm.Button2Click(Sender: TObject);
begin
  WriteImmediateAlertLevel(strtoint(Edit1.Text));
end;

procedure TfrmProximityForm.CheckDistanceThreshold(PathLoss: Integer);
begin
  if PathLoss < 25 then
    //WriteImmediateAlertLevel(0)
    SetPosition(poNear)
  else
    if PathLoss < 45 then
      //WriteImmediateAlertLevel(1)
      SetPosition(poFar)
    else
      //WriteImmediateAlertLevel(2);
      SetPosition(poSoFar);
end;



procedure TfrmProximityForm.DoCharacteristicRead(const Sender: TObject; const ACharacteristic: TBluetoothGattCharacteristic;
  AGattStatus: TBluetoothGattStatus);
var
  LValue: Integer;
begin
  if ACharacteristic.UUID = TX_POWER_LEVEL_CHARACTERISTIC  then begin
    if Length(ACharacteristic.Value)>0 then begin
      LValue := ACharacteristic.Value[0];
      // SINT8 : Max=20 : Min -100
      if LValue>20 then LValue := LValue - 255;
      FTxPowerValue := LValue;
    end;
    Memo1.Lines.Add('FTxPowerValue=' + FTxPowerValue.ToString );
  end;

  if ACharacteristic.UUID = ALERT_LEVEL_CHARACTERISTIC  then begin

    if (ACharacteristic.GetService.UUID = LINK_LOSS_SERVICE) and (Length(ACharacteristic.Value)>0) then begin
      LValue := ACharacteristic.Value[0];
      lblLinkLossAlert.Text := LValue.ToString;
      Memo1.Lines.Add('Link Loss Alert=' + LValue.ToString );
    end

  end;

end;

procedure TfrmProximityForm.DoCharacteristicWrite(const Sender: TObject;
  const ACharacteristic: TBluetoothGattCharacteristic; AGattStatus: TBluetoothGattStatus);
begin


end;

procedure TfrmProximityForm.DoDiscoveryEndEvent(const Sender: TObject; const ADeviceList: TBluetoothLEDeviceList);
var
  I: Integer;
  WaitTime: Integer;
begin
  // log
  Memo1.Lines.Add(ADeviceList.Count.ToString +  ' devices discovered:');
  for I := 0 to ADeviceList.Count - 1 do Memo1.Lines.Add(ADeviceList[I].DeviceName);

  FBLEDevice := nil;
  for I := 0 to FBLEManager.LastDiscoveredDevices.Count - 1 do
  begin
    FBLEDevice := FBLEManager.LastDiscoveredDevices[I];
    FBLEDevice.DiscoverServices;
    FBLEDevice.OnServicesDiscovered := DoServicesDiscovered;
    FBLEDevice.OnCharacteristicRead := DoCharacteristicRead;
    FBLEDevice.OnReadRSSI := DoReadRSSI;
    FBLEDevice.OnCharacteristicWrite := DoCharacteristicWrite;
    FServicesDiscovered := False;
    if FBLEDevice.GetService(StringToGUID('{00001804-0000-1000-8000-00805F9B34FB}')) <> nil then
    begin
      btnConnect.Enabled := True;
      break;
    end;
    FBLEDevice := nil;
  end;

  if FBLEDevice = nil then
    lblDevice.Text := 'Device not found'
  else
    lblDevice.Text := FBLEDevice.DeviceName;

end;

procedure TfrmProximityForm.DoReadRSSI(const Sender: TObject; ARssiValue: Integer; AGattStatus: TBluetoothGattStatus);
var
  LRatioDB: Integer;
  LRatioLinear, LDistance: Double;
begin

  //Discard wrong values
  if ARssiValue>0 then Exit;


  lblTxPower.Text := FTxPowerValue.ToString;
  lblRSSI.Text := ARssiValue.ToString + ' dBm';
  LRatioDB := FTxPowerValue - ARssiValue;
  LRatioLinear := Power(10, LRatioDB / 10);
  LDistance := Sqrt(LRatioLinear);

  lblDistance.Text := LDistance.ToString;
  lblDist2.Text := IntToStr(FTxPowerValue - ARssiValue);

  CheckDistanceThreshold(FTxPowerValue - ARssiValue);
end;

procedure TfrmProximityForm.DoScan;
begin
  lblDevice.Text := '';
  FBLEManager.StartDiscovery(1500);
end;

procedure TfrmProximityForm.DoServicesDiscovered(const Sender: TObject; const AServiceList: TBluetoothGattServiceList);
begin
  FServicesDiscovered := True;
  btnConnect.Enabled := True;
end;

procedure TfrmProximityForm.EnableRSSIMonitorize(Enabled: boolean);
begin
  // enable timer
  tmrReadRSSI.Enabled := True;
end;

procedure TfrmProximityForm.FormShow(Sender: TObject);
begin
  FBLEManager := TBluetoothLEManager.Current;
  FBLEManager.OnDiscoveryEnd := DoDiscoveryEndEvent;
  FCurrentPosition := poUnknown;
  ProximityDeviceName := CProximityDeviceName;
  DoScan;
end;

procedure TfrmProximityForm.GetServiceAndCharacteristics;
var
  I, J, K: Integer;
begin
  for I := 0 to FBLEDevice.Services.Count - 1 do begin
    memo1.Lines.Add(FBLEDevice.Services[I].UUIDName + ' : ' + FBLEDevice.Services[I].UUID.ToString);
    for J := 0 to FBLEDevice.Services[I].Characteristics.Count - 1 do begin
      memo1.Lines.Add('--> ' + FBLEDevice.Services[I].Characteristics[J].UUIDName + ' : ' +
        FBLEDevice.Services[I].Characteristics[J].UUID.ToString);
      for K := 0 to FBLEDevice.Services[I].Characteristics[J].Descriptors.Count - 1 do begin
        memo1.Lines.Add('----> ' + FBLEDevice.Services[I].Characteristics[J].Descriptors[K].UUIDName + ' : ' +
                      FBLEDevice.Services[I].Characteristics[J].Descriptors[K].UUID.ToString);
      end;
    end;
  end;

  FLinkLossService := nil;
  FTXPowerService := nil;
  FImmediateAlertService := nil;

  FTXPowerLevelCharact := nil;
  FImmediateAlertLevelCharact := nil;
  FLinkLossAlertLevelCharact := nil;

  FLinkLossService := FBLEDevice.GetService(LINK_LOSS_SERVICE);
  if FLinkLossService<>nil then begin
      Memo1.Lines.Add('Service found');
      FLinkLossAlertLevelCharact := FLinkLossService.GetCharacteristic(ALERT_LEVEL_CHARACTERISTIC);
      FBLEDevice.ReadCharacteristic(FLinkLossAlertLevelCharact);
  end;

  FImmediateAlertService := FBLEDevice.GetService(IMMEDIATE_ALERT_SERVICE);
  if FImmediateAlertService<>nil then begin
      Memo1.Lines.Add('Service found');
      FImmediateAlertLevelCharact := FImmediateAlertService.GetCharacteristic(ALERT_LEVEL_CHARACTERISTIC);
      FBLEDevice.ReadCharacteristic(FImmediateAlertLevelCharact);
  end;
  FTXPowerService := FBLEDevice.GetService(TX_POWER_SERVICE);
  if FTXPowerService<>nil then begin
      Memo1.Lines.Add('Service found');
      FTXPowerLevelCharact := FTXPowerService.GetCharacteristic(TX_POWER_LEVEL_CHARACTERISTIC);
      FTxPowerValue := 99; // Invalid value
      if FTXPowerLevelCharact<>nil then
          FBLEDevice.ReadCharacteristic(FTXPowerLevelCharact);
  end;

  WriteLinkLossAlertLevel(0);
  EnableRSSIMonitorize(True);

end;


procedure TfrmProximityForm.SetPosition(Position: TPosition);
begin
  case Position of
    poUnknown: ;
    poNear:
      begin
        FNearCount := Min(FNearCount + 1, 2);
        FFarCount := 0;
        FSoFarCount := 0;
        if (FNearCount >= 2) and (FCurrentPosition <> poNear) then UpdateCurrentPosition(poNear);
      end;
    poFar:
      begin
        FNearCount := 0;
        FFarCount := Min(FFarCount + 1, 2);
        FSoFarCount := 0;
        if (FFarCount >= 2) and (FCurrentPosition <> poFar) then UpdateCurrentPosition(poFar);
      end;
    poSoFar:
      begin
        FNearCount := 0;
        FFarCount := 0;
        FSoFarCount := Min(FSoFarCount + 1, 2);
        if (FSoFarCount >= 2) and (FCurrentPosition <> poSoFar) then UpdateCurrentPosition(poSoFar);
      end;
  end;

end;

procedure TfrmProximityForm.tmrReadRSSITimer(Sender: TObject);
begin
  FBLEDevice.ReadRemoteRSSI;
end;

procedure TfrmProximityForm.UpdateCurrentPosition(Position: TPosition);
var
  LPosition: string;
begin
  FCurrentPosition := Position;
  case Position of
    poUnknown: Exit;
    poNear:
      begin
        lblPosition.Text := 'Near';
      end;
    poFar:
      begin
        lblPosition.Text := 'Far';
      end;
    poSoFar:
      begin
        lblPosition.Text := 'So Far';
      end;
  end;
  WriteImmediateAlertLevel(Ord(Position));
end;

procedure TfrmProximityForm.WriteImmediateAlertLevel(AlertLevel: Byte);
var
  LData: TBytes;
begin
  SetLength(LData, 1);
  LData[0] := AlertLevel;
  if FImmediateAlertLevelCharact<>nil then begin
    FImmediateAlertLevelCharact.Value := LData;
    FBLEDevice.WriteCharacteristic(FImmediateAlertLevelCharact);
    Memo1.Lines.Add('FImmediateAlertLevelCharact service : ' + FImmediateAlertLevelCharact.GetService.UUIDName);
    Memo1.Lines.Add('Immediate Alert_Level Charact set to ' + AlertLevel.ToString  )
  end;
end;

procedure TfrmProximityForm.WriteLinkLossAlertLevel(AlertLevel: Byte);
var
  LData: TBytes;
begin
  SetLength(LData, 1);
  LData[0] := AlertLevel;
  if FLinkLossAlertLevelCharact<>nil then begin
    FLinkLossAlertLevelCharact.Value := LData;
    FBLEDevice.WriteCharacteristic(FLinkLossAlertLevelCharact);
    Memo1.Lines.Add('LinkLoss Alert_Level Charact set to ' + AlertLevel.ToString  )
  end;
end;

end.
