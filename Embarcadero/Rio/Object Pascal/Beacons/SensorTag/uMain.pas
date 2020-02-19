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
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Bluetooth, FMX.StdCtrls, System.Bluetooth.Components, FMX.Layouts,
  FMX.ListBox, FMX.Controls.Presentation, FMX.Edit;

type
  TFrMainform = class(TForm)
    BluetoothLE1: TBluetoothLE;
    Button1: TButton;
    ListBox1: TListBox;
    Button2: TButton;
    EdAmbient: TEdit;
    EdTarget: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    EdAccelX: TEdit;
    EdAccelY: TEdit;
    EdAccelZ: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    EdHumidity: TEdit;
    Label8: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure BluetoothLE1EndDiscoverDevices(const Sender: TObject;
      const ADeviceList: TBluetoothLEDeviceList);
    procedure Button2Click(Sender: TObject);
    procedure BluetoothLE1EndDiscoverServices(const Sender: TObject;
      const AServiceList: TBluetoothGattServiceList);
    procedure BluetoothLE1CharacteristicRead(const Sender: TObject;
      const ACharacteristic: TBluetoothGattCharacteristic;
      AGattStatus: TBluetoothGattStatus);
  private
    FCurrentDevice: TBluetoothLEDevice;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrMainform: TFrMainform;

implementation

{$R *.fmx}

Uses
  System.Math;


{
  These functions have been translated from the examples provided by TE in its wiki
  http://processors.wiki.ti.com/index.php/SensorTag_User_Guide#Contactless_IR_Temperature_Sensor
}

function shortSignedAtOffset(const ACharacteristic: TBluetoothGattCharacteristic; offset: Integer): Integer;
var
  lowerByte: Integer;
  upperByte: Integer;
begin
    lowerByte := ACharacteristic.GetValueAsUInt8(offset);
    upperByte := ACharacteristic.GetValueAsInt8(offset + 1); // Note: interpret MSB as signed.

    Result := (upperByte shl 8) + lowerByte;
end;

function shortUnsignedAtOffset(const ACharacteristic: TBluetoothGattCharacteristic; offset: Integer): Integer;
var
  lowerByte: Integer;
  upperByte: Integer;
begin
    lowerByte := ACharacteristic.GetValueAsUInt8(offset);
    upperByte := ACharacteristic.GetValueAsUInt8(offset + 1); // Note: interpret MSB as unsigned.

    Result := (upperByte shl 8) + lowerByte;
end;


function extractAmbientTemperature(const ACharacteristic: TBluetoothGattCharacteristic): double;
begin
    Result := shortUnsignedAtOffset(ACharacteristic, 2) / 128.0;
end;

function extractTargetTemperature(const ACharacteristic: TBluetoothGattCharacteristic; ambient: double ): double;
const
    S0 = 5.593E-14;	// Calibration factor
    a1 = 1.75E-3;
    a2 = -1.678E-5;
    b0 = -2.94E-5;
    b1 = -5.7E-7;
    b2 = 4.63E-9;
    c2 = 13.4;
    Tref = 298.15;
var
  twoByteValue: Integer;
  Vobj2: Double;
  Tdie: Double;
  S: Double;
  Vos: Double;
  fObj: Double;
  tObj: Double;
begin
    twoByteValue := shortSignedAtOffset(ACharacteristic, 0);
    Vobj2 := twoByteValue;
    Vobj2 := Vobj2 * 0.00000015625;
    Tdie := ambient + 273.15;
    S := S0*( 1 + a1 * (Tdie - Tref) + a2 * Power((Tdie - Tref), 2));
    Vos := b0 + b1 *(Tdie - Tref) + b2 * Power((Tdie - Tref), 2);
    fObj := (Vobj2 - Vos) + c2 * Power((Vobj2 - Vos), 2);
    tObj := Power(Power(Tdie, 4) + (fObj / S), 0.25);

    Result := tObj - 273.15;
end;

procedure TFrMainform.BluetoothLE1CharacteristicRead(const Sender: TObject;
  const ACharacteristic: TBluetoothGattCharacteristic;
  AGattStatus: TBluetoothGattStatus);
var
  LAmbient: Double;
  LTarget: Double;
  LAccel: Double;
  LHum: Integer;
  LHHum: Double;
begin
  if ACharacteristic.UUIDName = 'UUID_IRT_DATA' then
  begin
    LAmbient := extractAmbientTemperature(ACharacteristic);
    LTarget := extractTargetTemperature(ACharacteristic, LAmbient);
    EdAmbient.Text := Format('%f ºC',[LAmbient]);
    EdTarget.Text := Format('%f ºC',[LTarget]);
  end
  else if ACharacteristic.UUIDName = 'UUID_ACC_DATA' then
  begin
    LAccel := ACharacteristic.GetValueAsInt8(0) / 64;
    EdAccelX.Text := Format('%f',[LAccel]);
    LAccel := ACharacteristic.GetValueAsInt8(1) / 64;
    EdAccelY.Text := Format('%f',[LAccel]);
    LAccel := ACharacteristic.GetValueAsInt8(2) / 64 * -1;
    EdAccelZ.Text := Format('%f',[LAccel]);
  end
  else if ACharacteristic.UUIDName = 'UUID_HUM_DATA' then
  begin
    LHum := shortUnsignedAtOffset(ACharacteristic, 2);
    LHum := LHum - (LHum mod 4);
    LHHum := (-6.0) + 125.0 * (LHum / 65535.0);
    EdHumidity.Text := Format('%f %',[LHHum]);
  end;
end;

procedure TFrMainform.BluetoothLE1EndDiscoverDevices(const Sender: TObject;
  const ADeviceList: TBluetoothLEDeviceList);
var
  LDevice: TBluetoothLEDevice;
begin
  ListBox1.Items.Clear;
  for LDevice in ADeviceList do
    // asking for both names since some OS can provide a different name for the device
    if (LDevice.DeviceName = 'SensorTag') or (LDevice.DeviceName = 'TI BLE Sensor Tag') then
      ListBox1.Items.AddObject(LDevice.DeviceName, LDevice);
end;

procedure TFrMainform.BluetoothLE1EndDiscoverServices(const Sender: TObject;
  const AServiceList: TBluetoothGattServiceList);
var
  I, J: Integer;
begin
  for I := 0 to AServiceList.Count - 1 do
  begin
    if AServiceList[I].UUIDName = 'UUID_IRT_SERV' then // Temperature service
    begin
      for J := 0 to AServiceList[I].Characteristics.Count - 1 do
      begin
        if AServiceList[I].Characteristics[J].UUIDName = 'UUID_IRT_CONF' then
        begin
          AServiceList[I].Characteristics[J].SetValueAsUInt8(1); // this is needed to start receiving data
          BluetoothLE1.WriteCharacteristic(FcurrentDevice, AServiceList[I].Characteristics[J])
        end
        else if AServiceList[I].Characteristics[J].UUIDName = 'UUID_IRT_DATA' then
          BluetoothLE1.SubscribeToCharacteristic(FCurrentDevice, AServiceList[I].Characteristics[J])
      end;
    end
    else if AServiceList[I].UUIDName = 'UUID_ACC_SERV' then // Accelerometer service
    begin
      for J := 0 to AServiceList[I].Characteristics.Count - 1 do
      begin
        if AServiceList[I].Characteristics[J].UUIDName = 'UUID_ACC_CONF' then
        begin
          AServiceList[I].Characteristics[J].SetValueAsUInt8(1); // this is needed to start receiving data
          BluetoothLE1.WriteCharacteristic(FcurrentDevice, AServiceList[I].Characteristics[J])
        end
        else if AServiceList[I].Characteristics[J].UUIDName = 'UUID_ACC_DATA' then
          BluetoothLE1.SubscribeToCharacteristic(FCurrentDevice, AServiceList[I].Characteristics[J])
      end;
    end
    else if AServiceList[I].UUIDName = 'UUID_HUM_SERV' then
    begin
      for J := 0 to AServiceList[I].Characteristics.Count - 1 do
      begin
        if AServiceList[I].Characteristics[J].UUIDName = 'UUID_HUM_CONF' then
        begin
          AServiceList[I].Characteristics[J].SetValueAsUInt8(1); // this is needed to start receiving data
          BluetoothLE1.WriteCharacteristic(FcurrentDevice, AServiceList[I].Characteristics[J])
        end
        else if AServiceList[I].Characteristics[J].UUIDName = 'UUID_HUM_DATA' then
          BluetoothLE1.SubscribeToCharacteristic(FCurrentDevice, AServiceList[I].Characteristics[J])
      end;
    end;
  end;
end;

procedure TFrMainform.Button1Click(Sender: TObject);
begin
  BluetoothLE1.DiscoverDevices(4000);
end;

procedure TFrMainform.Button2Click(Sender: TObject);
begin
  if ListBox1.ItemIndex >= 0 then
  begin
    FCurrentDevice := ListBox1.Items.Objects[ListBox1.ItemIndex] as TBluetoothLEDevice;
    BluetoothLE1.DiscoverServices(FCurrentDevice);
  end
  else
    ShowMessage('Please select a device from the list');
end;

end.
