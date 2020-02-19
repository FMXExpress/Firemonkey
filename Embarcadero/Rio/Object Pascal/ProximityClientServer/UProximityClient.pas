//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit UProximityClient;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Math,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Layouts, FMX.Memo, System.Bluetooth,
  FMX.Controls.Presentation, FMX.Edit, FMX.ListBox, FMX.ScrollBox;

type

  TPosition = (poNear, poFar, poSoFar, poUnknown);

  TfrmProximityForm = class(TForm)
    pnlLog: TPanel;
    Memo1: TMemo;
    pnlMain: TPanel;
    lblDevice: TLabel;
    Splitter1: TSplitter;
    tmrReadRSSI: TTimer;
    lblPosition: TLabel;
    Panel1: TPanel;
    btnScan: TButton;
    Panel2: TPanel;
    Panel3: TPanel;
    lblLinkLossAlert: TLabel;
    lblImmediateAlert: TLabel;
    Label4: TLabel;
    lblTxPower: TLabel;
    Panel4: TPanel;
    Label1: TLabel;
    lblRSSI: TLabel;
    Panel5: TPanel;
    Label2: TLabel;
    lblDist2: TLabel;
    Label3: TLabel;
    lblDistance: TLabel;
    Label5: TLabel;
    procedure btnScanClick(Sender: TObject);
    procedure tmrReadRSSITimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FBLEManager: TBluetoothLEManager;
    FBLEDevice: TBluetoothLEDevice;
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
    procedure DoCharacteristicRead(const Sender: TObject; const ACharacteristic: TBluetoothGattCharacteristic;
                                  AGattStatus: TBluetoothGattStatus);
    procedure OnDeviceDisconnect(Sender: TObject);
    procedure DoReadRSSI(const Sender: TObject; ARssiValue: Integer; AGattStatus: TBluetoothGattStatus);
    procedure DoScan;
    procedure Connect;
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

  LINK_LOSS_SERVICE: TBluetoothUUID = '{00001803-0000-1000-8000-00805F9B34FB}';
  IMMEDIATE_ALERT_SERVICE: TBluetoothUUID = '{00001802-0000-1000-8000-00805F9B34FB}';
  TX_POWER_SERVICE: TBluetoothUUID = '{00001804-0000-1000-8000-00805F9B34FB}';

  ALERT_LEVEL_CHARACTERISTIC: TBluetoothUUID  = '{00002A06-0000-1000-8000-00805F9B34FB}';
  TX_POWER_LEVEL_CHARACTERISTIC: TBluetoothUUID  = '{00002A07-0000-1000-8000-00805F9B34FB}';

var
  frmProximityForm: TfrmProximityForm;

implementation

{$R *.fmx}

{ TfrmProximityForm }

procedure TfrmProximityForm.Connect;
begin
  if FBLEDevice <> nil then
  begin
    if FBLEDevice.Services.Count = 0 then
      Memo1.Lines.Add('No services found!')
    else
      GetServiceAndCharacteristics;
  end;
end;

procedure TfrmProximityForm.btnScanClick(Sender: TObject);
begin
  EnableRSSIMonitorize(False);
  DoScan;
end;

procedure TfrmProximityForm.CheckDistanceThreshold(PathLoss: Integer);
begin
  if PathLoss < 25 then
    SetPosition(poNear)
  else if PathLoss < 45 then
    SetPosition(poFar)
  else
    SetPosition(poSoFar);
end;

procedure TfrmProximityForm.DoCharacteristicRead(const Sender: TObject; const ACharacteristic: TBluetoothGattCharacteristic;
  AGattStatus: TBluetoothGattStatus);
var
  LValue: Integer;
begin
  if AGattStatus <> TBluetoothGattStatus.Success then
    Exit;

  if ACharacteristic.UUID = TX_POWER_LEVEL_CHARACTERISTIC then
  begin
    if (Length(ACharacteristic.Value) > 0) then
    begin
      LValue := ACharacteristic.Value[0];
      // SINT8 : Max=20 : Min -100
      if LValue > 20 then
        LValue := LValue - 255;
      FTxPowerValue := LValue;
      TThread.Synchronize(nil, procedure begin
        Memo1.Lines.Add('FTxPowerValue =' + FTxPowerValue.ToString);
      end);
    end;
  end;

  if ACharacteristic.UUID = ALERT_LEVEL_CHARACTERISTIC then
  begin
    if (ACharacteristic.GetService.UUID = LINK_LOSS_SERVICE) and (Length(ACharacteristic.Value) > 0) then
    begin
      LValue := ACharacteristic.Value[0];
      TThread.Synchronize(nil, procedure begin
        lblLinkLossAlert.Text := LValue.ToString;
        Memo1.Lines.Add('Link Loss Alert =' + LValue.ToString);
      end);
    end
  end;
end;

procedure TfrmProximityForm.DoDiscoveryEndEvent(const Sender: TObject; const ADeviceList: TBluetoothLEDeviceList);
var
  I: Integer;
  WaitTime: Integer;
  LBLEDevice: TBluetoothLEDevice;
begin
  TThread.Synchronize(nil,
  procedure
  var
    I: Integer;
  begin
    Memo1.Lines.Add(ADeviceList.Count.ToString +  ' devices discovered:');
    for I := 0 to ADeviceList.Count - 1 do
      Memo1.Lines.Add(ADeviceList[I].DeviceName);
  end);

  FBLEDevice := nil;
  for I := 0 to ADeviceList.Count - 1 do
  begin
    LBLEDevice := ADeviceList[I];
    LBLEDevice.DiscoverServices;
    if LBLEDevice.GetService(TX_POWER_SERVICE) <> nil then
    begin
      FBLEDevice := LBLEDevice;
      FBLEDevice.OnCharacteristicRead := DoCharacteristicRead;
      FBLEDevice.OnDisconnect := OnDeviceDisconnect;
      FBLEDevice.OnReadRSSI := DoReadRSSI;
      TThread.Synchronize(nil, Connect);
      Break;
    end;
  end;

  TThread.Synchronize(nil,
  procedure
  begin
    if FBLEDevice = nil then
      lblDevice.Text := 'Device not found'
    else
      lblDevice.Text := FBLEDevice.DeviceName;
  end);
end;

procedure TfrmProximityForm.DoReadRSSI(const Sender: TObject; ARssiValue: Integer; AGattStatus: TBluetoothGattStatus);
var
  LRatioDB: Integer;
  LRatioLinear, LDistance: Double;
begin
  //Discard wrong values
  if (AGattStatus <> TBluetoothGattStatus.Success) or (ARssiValue > 20) then
    Exit;

  LRatioDB := FTxPowerValue - ARssiValue;
  LRatioLinear := Power(10, LRatioDB / 10);
  LDistance := Sqrt(LRatioLinear);

  TThread.Synchronize(nil, procedure begin
    lblTxPower.Text := FTxPowerValue.ToString;
    lblRSSI.Text := ARssiValue.ToString + ' dBm';
    lblDistance.Text := LDistance.ToString;
    lblDist2.Text := IntToStr(LRatioDB);
  end);
  CheckDistanceThreshold(LRatioDB);
end;

procedure TfrmProximityForm.DoScan;
var
  LList: TBluetoothUUIDsList;
begin
  EnableRSSIMonitorize(False);
  lblDevice.Text := 'Scanning for devices';
  LList := TBluetoothUUIDsList.Create;
  try
    LList.Add(LINK_LOSS_SERVICE);
    LList.Add(IMMEDIATE_ALERT_SERVICE);
    LList.Add(TX_POWER_SERVICE);
    FBLEManager.StartDiscovery(1500, LList, False);
  finally
    LList.Free;
  end;
end;

procedure TfrmProximityForm.EnableRSSIMonitorize(Enabled: boolean);
begin
  // enable timer
  tmrReadRSSI.Enabled := Enabled;
end;

procedure TfrmProximityForm.FormShow(Sender: TObject);
begin
  FBLEManager := TBluetoothLEManager.Current;
  FBLEManager.OnDiscoveryEnd := DoDiscoveryEndEvent;
  FCurrentPosition := poUnknown;
  DoScan;
end;

procedure TfrmProximityForm.GetServiceAndCharacteristics;
var
  I, J, K: Integer;
begin
  for I := 0 to FBLEDevice.Services.Count - 1 do
  begin
    memo1.Lines.Add(FBLEDevice.Services[I].UUIDName + ' : ' + FBLEDevice.Services[I].UUID.ToString);
    for J := 0 to FBLEDevice.Services[I].Characteristics.Count - 1 do
    begin
      memo1.Lines.Add('--> ' + FBLEDevice.Services[I].Characteristics[J].UUIDName + ' : ' +
        FBLEDevice.Services[I].Characteristics[J].UUID.ToString);
      for K := 0 to FBLEDevice.Services[I].Characteristics[J].Descriptors.Count - 1 do
      begin
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
  if FLinkLossService <> nil then
  begin
    Memo1.Lines.Add('Service found LINK_LOSS_SERVICE ');
    FLinkLossAlertLevelCharact := FLinkLossService.GetCharacteristic(ALERT_LEVEL_CHARACTERISTIC);
    FBLEDevice.ReadCharacteristic(FLinkLossAlertLevelCharact);
  end;

  FImmediateAlertService := FBLEDevice.GetService(IMMEDIATE_ALERT_SERVICE);
  if FImmediateAlertService<>nil then
  begin
    Memo1.Lines.Add('Service found IMMEDIATE_ALERT_SERVICE');
    FImmediateAlertLevelCharact := FImmediateAlertService.GetCharacteristic(ALERT_LEVEL_CHARACTERISTIC);
  end;

  FTXPowerService := FBLEDevice.GetService(TX_POWER_SERVICE);
  if FTXPowerService <> nil then
  begin
    Memo1.Lines.Add('Service found TX_POWER_SERVICE');
    FTXPowerLevelCharact := FTXPowerService.GetCharacteristic(TX_POWER_LEVEL_CHARACTERISTIC);
    FTxPowerValue := -50; // Invalid value
    if FTXPowerLevelCharact <> nil then
      FBLEDevice.ReadCharacteristic(FTXPowerLevelCharact);
  end;
  WriteLinkLossAlertLevel(0);
  EnableRSSIMonitorize(True);
end;

procedure TfrmProximityForm.OnDeviceDisconnect(Sender: TObject);
begin
  FBLEDevice := nil;
  EnableRSSIMonitorize(False);
  DoScan; //Restore the connection
end;

procedure TfrmProximityForm.SetPosition(Position: TPosition);
begin
  case Position of
    poNear:
      begin
        FNearCount := Min(FNearCount + 1, 2);
        FFarCount := 0;
        FSoFarCount := 0;
        if (FNearCount >= 2) and (FCurrentPosition <> poNear) then
          UpdateCurrentPosition(poNear);
      end;
    poFar:
      begin
        FNearCount := 0;
        FFarCount := Min(FFarCount + 1, 2);
        FSoFarCount := 0;
        if (FFarCount >= 2) and (FCurrentPosition <> poFar) then
          UpdateCurrentPosition(poFar);
      end;
    poSoFar:
      begin
        FNearCount := 0;
        FFarCount := 0;
        FSoFarCount := Min(FSoFarCount + 1, 2);
        if (FSoFarCount >= 2) and (FCurrentPosition <> poSoFar) then
          UpdateCurrentPosition(poSoFar);
      end;
  end;
end;

procedure TfrmProximityForm.tmrReadRSSITimer(Sender: TObject);
begin
  if FBLEDevice <> nil then
    FBLEDevice.ReadRemoteRSSI;
end;

procedure TfrmProximityForm.UpdateCurrentPosition(Position: TPosition);
var
  LPosition: string;
begin
  FCurrentPosition := Position;
  TThread.Synchronize(nil, procedure begin
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
  end);
  WriteImmediateAlertLevel(Ord(Position));
end;

procedure TfrmProximityForm.WriteImmediateAlertLevel(AlertLevel: Byte);
var
  LData: TBytes;
begin
  if FImmediateAlertLevelCharact <> nil then
  begin
    TMonitor.Enter(FImmediateAlertLevelCharact);
    try
      SetLength(LData, 1);
      LData[0] := AlertLevel;
      FImmediateAlertLevelCharact.Value := LData;
      FBLEDevice.WriteCharacteristic(FImmediateAlertLevelCharact);
      TThread.Synchronize(nil, procedure begin
        Memo1.Lines.Add('FImmediateAlertLevelCharact service : ' + FImmediateAlertLevelCharact.GetService.UUIDName);
        Memo1.Lines.Add('Immediate Alert_Level Charact set to ' + AlertLevel.ToString  )
      end);
    finally
      TMonitor.Exit(FImmediateAlertLevelCharact);
    end;
  end;
end;

procedure TfrmProximityForm.WriteLinkLossAlertLevel(AlertLevel: Byte);
var
  LData: TBytes;
begin
  if FLinkLossAlertLevelCharact <> nil then
  begin
    SetLength(LData, 1);
    LData[0] := AlertLevel;
    FLinkLossAlertLevelCharact.Value := LData;
    FBLEDevice.WriteCharacteristic(FLinkLossAlertLevelCharact);
    Memo1.Lines.Add('LinkLoss Alert_Level Charact set to ' + AlertLevel.ToString);
  end;
end;

end.
