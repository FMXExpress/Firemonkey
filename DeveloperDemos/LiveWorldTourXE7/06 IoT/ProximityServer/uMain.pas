unit uMain;

interface

uses
System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.Bluetooth, FMX.Layouts, FMX.ListBox,
  System.Mac.Bluetooth, Macapi.Helpers, Macapi.Bluetooth, Macapi.Dispatch, Macapi.ObjectiveC,
  FMX.TabControl, FMX.Edit, FMX.Controls.Presentation, FMX.Memo,
{$IFDEF IOS}
  iOSapi.CocoaTypes, iOSapi.Foundation;
{$ELSE}
  Macapi.CocoaTypes, Macapi.Foundation, FMX.Colors, FMX.Objects, FMX.Ani;
{$ENDIF}


type
  TForm4 = class(TForm)
    Label1: TLabel;
    Button13: TButton;
    tmrRSSI: TTimer;
    swConnected: TSwitch;
    lbDeviceConnected: TLabel;
    tmrReviewDeviceConnected: TTimer;
    txtALERT: TText;
    Rectangle1: TRectangle;
    highAlertAnimation: TColorAnimation;
    medAlertAnimation: TColorAnimation;
    Memo1: TMemo;
    ImageControl1: TImageControl;
    procedure Button15Click(Sender: TObject);
    procedure tmrRSSITimer(Sender: TObject);
    procedure tmrReviewDeviceConnectedTimer(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure UpdateTextAndSwitch;
  private
    { Private declarations }
    FBluetoothLEDevice: TBluetoothLEDevice;
    FBluetoothManager: TBluetoothManager;
    FBluetoothManagerLE: TBluetoothLEManager;
    FDevices: TBluetoothDeviceList;
    FAdapter: TBluetoothAdapter;
    FCentralManager: CBCentralManager;

    FGattServer: TBluetoothGattServer;

    FGenericAccessService: TBluetoothGattService;
    FLinkLossService: TBluetoothGattService;
    FImmediateAlertService: TBluetoothGattService;
    FTxPowerService: TBluetoothGattService;

    {Bluetooth GATT Characteristics}

    FDeviceNameCharact: TBluetoothGattCharacteristic;

    FLinkLoss: TBluetoothGattCharacteristic;
    FImmediateAlert: TBluetoothGattCharacteristic;
    FTxPower: TBluetoothGattCharacteristic;

    AChar: TBluetoothGattCharacteristic;
    IByte: Integer;

    procedure DoOnConnectedDevice(const Sender: TObject; const ADevice: TBluetoothLEDevice);
    procedure MyReadEvent(const Sender: TObject; const ACharacteristic: TBluetoothGattCharacteristic;
      var AGattStatus: TBluetoothGattStatus);
    procedure MyWriteEvent(const Sender: TObject; const ACharacteristic: TBluetoothGattCharacteristic;
      var AGattStatus: TBluetoothGattStatus; const AValue: TBytes);
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}

{Publish service}
procedure TForm4.Button13Click(Sender: TObject);
var
  I: Integer;
  CharValue: TBytes;
  LCharacteristic: TBluetoothGattCharacteristic;
begin
  FBluetoothManagerLE := TBluetoothLEManager.Current;
  FGattServer := FBluetoothManagerLE.GetGattServer;
  FGattServer.OnConnectedDevice := DoOnConnectedDevice;
  FGattServer.OnCharacteristicRead := MyReadEvent;
  FGattServer.OnCharacteristicWrite := MyWriteEvent;
  Memo1.Lines.Add('[FGattServer] Started and announced');
  if FLinkLossService = nil then
  begin
    FLinkLossService := FGattServer.CreateService(StringToGUID('{00001803-0000-1000-8000-00805F9B34FB}'), TBluetoothServiceType.Primary);

    //(Name: 'Alert Level'; UUID:'{00002A06-0000-1000-8000-00805F9B34FB}'),
    (*
      (Name: 'IMMEDIATE ALERT'; UUID:'{00001802-0000-1000-8000-00805F9B34FB}'),
      (Name: 'LINK LOSS'; UUID:'{00001803-0000-1000-8000-00805F9B34FB}'),
      (Name: 'TX POWER'; UUID:'{00001804-0000-1000-8000-00805F9B34FB}'),
    *)
    (*
    0 “No Alert”
    1 “Mild Alert”
    2 “High Alert,”
    *)

    FLinkLoss := FGattServer.CreateCharacteristic(
                        FLinkLossService,
                        StringToGUID('{00002A06-0000-1000-8000-00805F9B34FB}'),
                        [TBluetoothProperty.Read, TBluetoothProperty.Write],
                        'This service defines behavior when a link is lost between two devices'
                      );

    SetLength(CharValue,1); //length('No alert'));
    CharValue[0] := 0;
    FLinkLoss.Value := CharValue;

    FGattServer.AddService(FLinkLossService);
  end;

  if FImmediateAlertService = nil then
  begin
    FImmediateAlertService := FGattServer.CreateService(StringToGUID('{00001802-0000-1000-8000-00805F9B34FB}'), TBluetoothServiceType.Primary);
    (*
      <Enumeration key="0" value="No Alert" />
      <Enumeration key="1" value="Mild Alert" />
      <Enumeration key="2" value="High Alert" />
    *)
    FImmediateAlert := FGattServer.CreateCharacteristic(
                          FImmediateAlertService,
                          StringToGUID('{00002A06-0000-1000-8000-00805F9B34FB}'),
                          [TBluetoothProperty.Read, TBluetoothProperty.Write]
                        );

    CharValue[0] := 0; // 'No Alert'
    FImmediateAlert.Value := CharValue;

    FGattServer.AddService(FImmediateAlertService);
    (*
     The value 0x12 is interpreted as +18dBm
     The value 0xEE is interpreted as -18dBm

      <Minimum>-100</Minimum>
      <Maximum>20</Maximum>
    *)
  end;
  if FTxPowerService = nil then
  begin
    FTxPowerService := FGattServer.CreateService(StringToGUID('{00001804-0000-1000-8000-00805F9B34FB}'), TBluetoothServiceType.Primary);
    FTxPower := FGattServer.CreateCharacteristic(
                        FTxPowerService,
                        StringToGUID('{00002A07-0000-1000-8000-00805F9B34FB}'),
                        [TBluetoothProperty.Read],
                        'Represents the current transmit power'
                      );

    CharValue[0] :=  $CD; // 'No Alert'
    FTxPower.Value := CharValue;
    FTxPowerService.AdvertiseData := 'POWERLINK';
    FGattServer.AddService(FTxPowerService);
  end;
  Memo1.Lines.Add('[FGattServer] Services created');
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

procedure TForm4.Button15Click(Sender: TObject);
var
  LEDevice: TBluetoothLEDevice;
  LDevices: TBluetoothDeviceList;
begin
  FBluetoothManager := TBluetoothManager.Current;
  LEDevice := FBluetoothManagerLE.LastDiscoveredDevices[0];

  for LEDevice in FBluetoothManagerLE.LastDiscoveredDevices do
  begin
    ShowMessage(LEDevice.DeviceName);
  end;
end;

procedure TForm4.DoOnConnectedDevice(const Sender: TObject;
  const ADevice: TBluetoothLEDevice);
begin

end;

procedure TForm4.UpdateTextAndSwitch();
begin
  if (swConnected.IsChecked = False) then
  begin
    swConnected.IsChecked := True;
    lbDeviceConnected.FontColor := TAlphaColorRec.Green;
    lbDeviceConnected.Text := 'Device connected';
    Rectangle1.Fill.Color := TAlphaColorRec.Green;
    Memo1.Lines.Add(lbDeviceConnected.Text);
  end;

end;

procedure TForm4.MyReadEvent(const Sender: TObject;
  const ACharacteristic: TBluetoothGattCharacteristic;
  var AGattStatus: TBluetoothGattStatus);
begin
  UpdateTextAndSwitch;
  Memo1.Lines.Add('MyReadEvent ' + ACharacteristic.UUIDName);
end;

procedure TForm4.MyWriteEvent(const Sender: TObject;
  const ACharacteristic: TBluetoothGattCharacteristic;
  var AGattStatus: TBluetoothGattStatus; const AValue: TBytes);
var
  IntValue : Integer;
begin
  UpdateTextAndSwitch;

  Memo1.Lines.Add('MyWriteEvent ' + ACharacteristic.UUIDName + ' Value: ' + BytesToString(AValue));

  if ACharacteristic.UUID.ToString = '{00002A06-0000-1000-8000-00805F9B34FB}' then
  begin
    IntValue := BytesToString(AValue).ToInteger;
    if (IntValue = 1 ) then
      begin
        txtALERT.Text := 'MILD ALERT';
        highAlertAnimation.Enabled := False;
        medAlertAnimation.Enabled := True;
      end
    else
      if (IntValue = 2 ) then
      begin
        txtALERT.Text := 'HIGH ALERT';
        medAlertAnimation.Enabled := False;
        highAlertAnimation.Enabled := True;
      end
      else
       if IntValue = 0 then
       begin
        txtALERT.Text := 'NO ALERT';
        medAlertAnimation.Enabled := False;
        highAlertAnimation.Enabled := False;
        Rectangle1.Fill.Color := TAlphaColorRec.Green;
       end;
  end;
end;

procedure TForm4.tmrReviewDeviceConnectedTimer(Sender: TObject);
begin
  FBluetoothManagerLE := TBluetoothLEManager.Current;
end;

procedure TForm4.tmrRSSITimer(Sender: TObject);
begin
  if FBluetoothLEDevice <> nil then
    FBluetoothLEDevice.ReadRemoteRSSI;
end;

end.
