//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.Bluetooth, FMX.Layouts, FMX.ListBox, FMX.TabControl, FMX.Edit,
  FMX.Controls.Presentation, FMX.Memo, FMX.Colors, FMX.Objects, FMX.Ani,
  FMX.ScrollBox;

type
  TForm4 = class(TForm)
    Label1: TLabel;
    BtnStartAnnounce: TButton;
    swConnected: TSwitch;
    Rectangle1: TRectangle;
    highAlertAnimation: TColorAnimation;
    medAlertAnimation: TColorAnimation;
    Memo1: TMemo;
    ImageControl1: TImageControl;
    lbALERT: TLabel;
    Panel1: TPanel;
    lbDeviceConnected: TLabel;
    Label2: TLabel;
    procedure BtnStartAnnounceClick(Sender: TObject);
    procedure UpdateTextAndSwitch;
  private
    { Private declarations }
    FBluetoothManagerLE: TBluetoothLEManager;
    FGattServer: TBluetoothGattServer;
    FLinkLossService: TBluetoothGattService;
    FImmediateAlertService: TBluetoothGattService;
    FTxPowerService: TBluetoothGattService;

    {Bluetooth GATT Characteristics}
    FLinkLoss: TBluetoothGattCharacteristic;
    FImmediateAlert: TBluetoothGattCharacteristic;
    FTxPower: TBluetoothGattCharacteristic;

    procedure DoOnConnectedDevice(const Sender: TObject; const ADevice: TBluetoothLEDevice);
    procedure MyReadEvent(const Sender: TObject; const ACharacteristic: TBluetoothGattCharacteristic;
      var AGattStatus: TBluetoothGattStatus);
    procedure MyWriteEvent(const Sender: TObject; const ACharacteristic: TBluetoothGattCharacteristic;
      var AGattStatus: TBluetoothGattStatus; const AValue: TByteDynArray);
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}

//  (Name: 'Alert Level'; UUID:'{00002A06-0000-1000-8000-00805F9B34FB}'),
//  (Name: 'LINK LOSS'; UUID:'{00001803-0000-1000-8000-00805F9B34FB}'),
//  (Name: 'IMMEDIATE ALERT'; UUID:'{00001802-0000-1000-8000-00805F9B34FB}'),
//    0 "No Alert"
//    1 "Mild Alert"
//    2 "High Alert,"
//  (Name: 'TX POWER'; UUID:'{00001804-0000-1000-8000-00805F9B34FB}'),
//    The value 0x12 is interpreted as +18dBm
//    The value 0xEE is interpreted as -18dBm
//    <Minimum>-100</Minimum>
//    <Maximum>20</Maximum>

const
  LINK_LOSS_SERVICE: TBluetoothUUID = '{00001803-0000-1000-8000-00805F9B34FB}';
  IMMEDIATE_ALERT_SERVICE: TBluetoothUUID = '{00001802-0000-1000-8000-00805F9B34FB}';
  TX_POWER_SERVICE: TBluetoothUUID = '{00001804-0000-1000-8000-00805F9B34FB}';

  ALERT_LEVEL_CHARACTERISTIC: TBluetoothUUID  = '{00002A06-0000-1000-8000-00805F9B34FB}';
  TX_POWER_LEVEL_CHARACTERISTIC: TBluetoothUUID  = '{00002A07-0000-1000-8000-00805F9B34FB}';


{Publish service}
procedure TForm4.BtnStartAnnounceClick(Sender: TObject);
var
  CharValue: TBytes;
begin
  FBluetoothManagerLE := TBluetoothLEManager.Current;
  FGattServer := FBluetoothManagerLE.GetGattServer;
  FGattServer.OnConnectedDevice := DoOnConnectedDevice;
  FGattServer.OnCharacteristicRead := MyReadEvent;
  FGattServer.OnCharacteristicWrite := MyWriteEvent;
  FGattServer.GattServerName := 'POWERLINK';
  Memo1.Lines.Add('[FGattServer] Started and announced');

  if FLinkLossService = nil then
  begin
    FLinkLossService := FGattServer.CreateService(LINK_LOSS_SERVICE, TBluetoothServiceType.Primary);
    FLinkLoss := FGattServer.CreateCharacteristic(FLinkLossService, ALERT_LEVEL_CHARACTERISTIC,
                        [TBluetoothProperty.Read, TBluetoothProperty.Write],
                        'This service defines behavior when a link is lost between two devices');
    SetLength(CharValue, 1); //length('No alert'));
    CharValue[0] := 0;
    FLinkLoss.Value := CharValue;
    FGattServer.AddService(FLinkLossService);
  end;

  if FImmediateAlertService = nil then
  begin
    FImmediateAlertService := FGattServer.CreateService(IMMEDIATE_ALERT_SERVICE, TBluetoothServiceType.Primary);
    FImmediateAlert := FGattServer.CreateCharacteristic( FImmediateAlertService, ALERT_LEVEL_CHARACTERISTIC,
                          [TBluetoothProperty.Read, TBluetoothProperty.Write]);
    CharValue[0] := 0; // 'No Alert'
    FImmediateAlert.Value := CharValue;
    FGattServer.AddService(FImmediateAlertService);
  end;

  if FTxPowerService = nil then
  begin
    FTxPowerService := FGattServer.CreateService(TX_POWER_SERVICE, TBluetoothServiceType.Primary);
    FTxPower := FGattServer.CreateCharacteristic(FTxPowerService, TX_POWER_LEVEL_CHARACTERISTIC,
                        [TBluetoothProperty.Read], 'Represents the current transmit power');
    CharValue[0] := $CD; // 'No Alert'
    FTxPower.Value := CharValue;
    FGattServer.AddService(FTxPowerService);
  end;
  Memo1.Lines.Add('[FGattServer] Services created');
end;

function BytesToString(const B: TByteDynArray): string;
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

procedure TForm4.DoOnConnectedDevice(const Sender: TObject;
  const ADevice: TBluetoothLEDevice);
begin
  TThread.Synchronize(nil,procedure begin
    UpdateTextAndSwitch;
  end);
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
  var AGattStatus: TBluetoothGattStatus; const AValue: TByteDynArray);
var
  IntValue : Integer;
begin
  UpdateTextAndSwitch;

  Memo1.Lines.Add('MyWriteEvent ' + ACharacteristic.UUIDName + ' Value: ' + BytesToString(AValue));

  if ACharacteristic.UUID = ALERT_LEVEL_CHARACTERISTIC then
  begin
    IntValue := BytesToString(AValue).ToInteger;
    case IntValue of
      0:
      begin
        lbALERT.Text := 'NO ALERT';
        medAlertAnimation.Enabled := False;
        highAlertAnimation.Enabled := False;
        Rectangle1.Fill.Color := TAlphaColorRec.Green;
      end;
      1:
      begin
        lbALERT.Text := 'MILD ALERT';
        highAlertAnimation.Enabled := False;
        medAlertAnimation.Enabled := True;
      end;
      2:
      begin
        lbALERT.Text := 'HIGH ALERT';
        medAlertAnimation.Enabled := False;
        highAlertAnimation.Enabled := True;
      end;
    end;
  end;
end;

end.
