//---------------------------------------------------------------------------
// Copyright (c) 2016 Embarcadero Technologies, Inc. All rights reserved.  
//
// This software is the copyrighted property of Embarcadero Technologies, Inc. 
// ("Embarcadero") and its licensors. You may only use this software if you 
// are an authorized licensee of Delphi, C++Builder or RAD Studio 
// (the "Embarcadero Products").  This software is subject to Embarcadero's 
// standard software license and support agreement that accompanied your 
// purchase of the Embarcadero Products and is considered a Redistributable, 
// as such term is defined thereunder. Your use of this software constitutes 
// your acknowledgement of your agreement to the foregoing software license 
// and support agreement. 
//---------------------------------------------------------------------------
unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Beacon,
  System.Beacon.Components, FMX.Layouts, FMX.ListBox, FMX.StdCtrls,
  FMX.Controls.Presentation, System.Bluetooth, System.Bluetooth.Components, System.Math,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView, System.Actions, FMX.ActnList;

type

  TRssiToDistance = function (ARssi, ATxPower: Integer; ASignalPropagationConst: Single): Double of object;

  TForm1 = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Beacon1: TBeacon;
    Timer1: TTimer;
    Panel2: TPanel;
    CheckBoxiBeacon: TCheckBox;
    CheckBoxAltBeacon: TCheckBox;
    CheckBoxEddystone: TCheckBox;
    CheckBoxStandard: TCheckBox;
    CheckBoxAlternative: TCheckBox;
    CheckBoxScanEddystone: TCheckBox;
    CheckBoxExtended: TCheckBox;
    ListView1: TListView;
    StyleBook1: TStyleBook;
    CheckBoxDeviceInf: TCheckBox;
    Panel3: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure CheckBoxiBeaconChange(Sender: TObject);
    procedure CheckBoxiScanningChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Beacon1NewBLEScanFilter(const Sender: TObject;
      AKindofScanFilter: TKindofScanFilter;
      const ABluetoothLEScanFilter: TBluetoothLEScanFilter);
  private
    FBeacon : IBeacon;
    FRssiToDistance: TRssiToDistance;
    FCurrentBeaconList: TBeaconList;
    FTXCount: Integer;
    FTXArray: Array [0..99] of integer;
    FBluetoothLEDeviceList: TBluetoothLEDeviceList;
  public
    function GetScanningModeChecked: TBeaconScanMode;
    function GetKindOfBeaconsChecked: TKindofBeacons;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

//procedure TForm1.Beacon1CalculateDistances(const Sender: TObject;
//  const ABeacon: IBeacon; ATxPower, ARssi: Integer; var NewDistance: Double);
//var
//  I: Integer;
//begin // This event is used if we want to use our own calcDistance formula
//  if not Assigned(FRssiToDistance) then
//    FRssiToDistance := TBluetoothLEManager.Current.RssiToDistance;
////property LastDiscoveredDevices: TBluetoothLEDeviceList read FDiscoveredLEDevices;
//  NewDistance := System.Math.RoundTo(FRssiToDistance(ARssi, ATXPower, 0.5), DISTANCE_DECIMALS);
//  if FCurrentBeaconList <> nil then
//    for I := 0 to Length(FCurrentBeaconList) - 1 do
//    begin
//      if I >= FTXCount then
//        Inc(FTXCount);
//      if ABeacon.DeviceIdentifier = FCurrentBeaconList[I].DeviceIdentifier then
//        FTXArray[I] := ATxPower;
//    end;
//end;

procedure TForm1.Beacon1NewBLEScanFilter(const Sender: TObject;
  AKindofScanFilter: TKindofScanFilter;
  const ABluetoothLEScanFilter: TBluetoothLEScanFilter);
var
  TB: TBytes;
  LServiceDataRawData: TServiceDataRawData;
begin
//  if ABluetoothLEScanFilter <> nil then
//  begin
//    case AKindofScanFilter of
//      TKindofScanFilter.ManufacturerData: TB := ABluetoothLEScanFilter.ManufacturerSpecificData;
//      TKindofScanFilter.ServiceData:  LServiceDataRawData := ABluetoothLEScanFilter.ServiceData;
//      TKindofScanFilter.Service: showmessage(ABluetoothLEScanFilter.ServiceUUID.tostring);
//    end;
//  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
//  Listbox1.Clear;
  Beacon1.Mode := GetScanningModeChecked;
  Beacon1.ModeExtended := GetKindOfBeaconsChecked;
  if not(Beacon1.Enabled) then
  begin
    Beacon1.Enabled := True;
  end
  else
  begin
    Beacon1.StartScan;
  end;

  Timer1.Enabled := True;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Timer1.Enabled := False;
  Beacon1.StopScan;
end;

function TForm1.GetKindOfBeaconsChecked: TKindofBeacons;
begin
  Result := [];
  if CheckBoxiBeacon.IsChecked then
    Result := [TKindofBeacon.iBeacons];
  if CheckBoxAltBeacon.IsChecked then
    Result := Result + [TKindofBeacon.AltBeacons];
  if CheckBoxEddystone.IsChecked then
    Result := Result + [TKindofBeacon.Eddystones];
end;

function TForm1.GetScanningModeChecked: TBeaconScanMode;
begin
  if (CheckBoxStandard.IsChecked) then
    exit(TBeaconScanMode.Standard);
  if (CheckBoxAlternative.IsChecked) then
    exit(TBeaconScanMode.Alternative);
  if (CheckBoxScanEddystone.IsChecked) then
    exit(TBeaconScanMode.Eddystone);
  if (CheckBoxExtended.IsChecked) then
    exit(TBeaconScanMode.Extended);
end;

procedure TForm1.CheckBoxiScanningChange(Sender: TObject);
begin
  if TCheckBox(Sender).IsChecked then
  begin
  if TCheckBox(Sender) <> CheckBoxStandard then
    CheckBoxStandard.IsChecked := False;
  if TCheckBox(Sender) <> CheckBoxAlternative then
    CheckBoxAlternative.IsChecked := False;
  if TCheckBox(Sender) <> CheckBoxScanEddystone then
    CheckBoxScanEddystone.IsChecked := False;
  if TCheckBox(Sender) <> CheckBoxExtended then
    CheckBoxExtended.IsChecked := False;
  end;
end;

procedure TForm1.CheckBoxiBeaconChange(Sender: TObject);
var
  LKindofBeacons: TKindofBeacons;
begin
//  if not (csLoading in ComponentState) then
//    Beacon1.StopScan;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
   case Beacon1.Mode of  // TBeaconScanMode = (Standard, Alternative, Eddystone, Extended);
     TBeaconScanMode.Standard:
       begin
        CheckBoxStandard.IsChecked := True;
        CheckBoxAlternative.IsChecked := False;
        CheckBoxEddystone.IsChecked := False;
        CheckBoxExtended.IsChecked := False;
       end;
     TBeaconScanMode.Alternative:
       begin
        CheckBoxStandard.IsChecked := False;
        CheckBoxAlternative.IsChecked := True;
        CheckBoxEddystone.IsChecked := False;
        CheckBoxExtended.IsChecked := False;
       end;
     TBeaconScanMode.Eddystone:
       begin
        CheckBoxStandard.IsChecked := False;
        CheckBoxAlternative.IsChecked := False;
        CheckBoxEddystone.IsChecked := True;
        CheckBoxExtended.IsChecked := False;
       end;
     TBeaconScanMode.Extended:
       begin
        CheckBoxStandard.IsChecked := False;
        CheckBoxAlternative.IsChecked := False;
        CheckBoxEddystone.IsChecked := False;
        CheckBoxExtended.IsChecked := True;
       end;
   end;

  if TKindofBeacon.iBeacons in Beacon1.ModeExtended then
    CheckBoxiBeacon.IsChecked := True
  else
    CheckBoxiBeacon.IsChecked := False;

  if TKindofBeacon.AltBeacons in Beacon1.ModeExtended then
    CheckBoxAltBeacon.IsChecked := True
  else
    CheckBoxAltBeacon.IsChecked := False;

  if TKindofBeacon.Eddystones in Beacon1.ModeExtended then
    CheckBoxEddystone.IsChecked := True
  else
    CheckBoxEddystone.IsChecked := False;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  I,B: Integer;
  Pos, NOfDevices: Integer;
  ST1, ST2, TX: string;
  DeviceName, DeviceIdentifier: string;
  LEddystoneTLM: TEddystoneTLM;
  LEddyHandler: IEddystoneBeacon;
  LiBeacon: IiBeacon;
  LAltBeacon: IAltBeacon;
  MSData: TBytes;
  isDeviceInfChecked: Boolean;

  procedure PrintIt(const Line1: string = ''; const Line2: string = '');
  var
    LItem: TListViewItem;
  begin
   Inc(Pos);
   if (ListView1.Items.Count - 1) < Pos then
     LItem := ListView1.Items.Add;
    ListView1.Items[Pos].Text := Line1;
    ListView1.Items[Pos].Detail := Line2;
  end;

begin
  try
    FCurrentBeaconList := Beacon1.BeaconList;

    // This is a Backdoor to get acces to the BLE devices (TBluetoothLEDevice) associated to Beacons.
    if FBluetoothLEDeviceList = nil then
      FBluetoothLEDeviceList := TBluetoothLEManager.Current.LastDiscoveredDevices;

    Pos := -1;
    isDeviceInfChecked := CheckBoxDeviceInf.IsChecked;

    if Length(FCurrentBeaconList) > 0 then
    begin
      // The access to BLE Devices is not Thread Safe so we must protect it under its objectList Monitor
      TMonitor.Enter(FBluetoothLEDeviceList);
      try
        if isDeviceInfChecked then
          NOfDevices := FBluetoothLEDeviceList.Count
        else
          NOfDevices := 1;

        for B := 0 to NOfDevices - 1 do
        begin
          if isDeviceInfChecked then
          begin
            DeviceIdentifier := FBluetoothLEDeviceList[B].Identifier;
            DeviceName := FBluetoothLEDeviceList[B].DeviceName;
          end
          else
            DeviceIdentifier := '';
          if DeviceName = '' then
            DeviceName := 'No Name';

          for I := 0 to Length(FCurrentBeaconList) - 1 do
            if (FCurrentBeaconList[I] <> nil) and (FCurrentBeaconList[I].itsAlive) and
              ((not isDeviceInfChecked) or (DeviceIdentifier = FCurrentBeaconList[I].DeviceIdentifier)) then // There are BLE Devices that advertised two or more kind of beacons.
            begin
              ST1 := '';
              ST2 := '';
              TX := '';
              case FCurrentBeaconList[I].KindofBeacon of
                TKindofBeacon.iBeacons:
                  if (Supports(FCurrentBeaconList[I], IiBeacon, LiBeacon)) then
                  begin
                    ST1 := 'iBeacon, GUID: ' + LiBeacon.GUID.ToString+#13+'Major: ' + LiBeacon.Major.ToString+' Minor: ' + LiBeacon.Minor.ToString;
                    if isDeviceInfChecked then
                    begin
                      MSData := FBluetoothLEDeviceList[B].ScannedAdvertiseData.ManufacturerSpecificData;
                      if Length(MSData) = STANDARD_DATA_LENGTH  then // There are BLE Devices that advertised two or more kind of beacons, so The MSData might Change.
                        TX := ShortInt(MSData[Length(MSData) - 1]).ToString
                      else
                        TX := ShortInt(MSData[Length(MSData) - 2]).ToString;
                    end;
                  end;

                TKindofBeacon.AltBeacons:
                  if (Supports(FCurrentBeaconList[I], IAltBeacon, LAltBeacon)) then
                  begin
                    ST1 := 'AltBeacon, GUID: ' + LAltBeacon.GUID.ToString+#13+'Major: ' + LAltBeacon.Major.ToString+' Minor: ' + LAltBeacon.Minor.ToString;
                    if isDeviceInfChecked then
                    begin
                      MSData := FBluetoothLEDeviceList[B].ScannedAdvertiseData.ManufacturerSpecificData;
                      if Length(MSData) = STANDARD_DATA_LENGTH  then // There are BLE Devices that advertise tw0 or more kind of beacons, so The MSData might Change.
                        TX := ShortInt(MSData[Length(MSData) - 1]).ToString
                      else
                        TX := ShortInt(MSData[Length(MSData) - 2]).ToString;
                     end;
                  end;

                 TKindofBeacon.Eddystones:
                   begin
                     if (Supports(FCurrentBeaconList[I], IEddystoneBeacon, LEddyHandler)) then
                     begin
                       ST1 := 'Eddystone'+#13+' ';
                       if isDeviceInfChecked then
                         MSData := FBluetoothLEDeviceList[B].ScannedAdvertiseData.ServiceData[0].Value;

                       if  (TKindofEddystone.UID in LEddyHandler.KindofEddystones) then
                       begin
                         ST1 := 'Eddystone, UID-NameSpace: '+LEddyHandler.EddystoneUID.NamespaceToString
                           +#13+ 'UID-Instance: '+LEddyHandler.EddystoneUID.InstanceToString;
                         if isDeviceInfChecked and (MSData[EDDY_FRAMETYPE_POS] = EDDYSTONE_UID) then
                           TX := ShortInt(MSData[EDDY_TX_POS] - EDDY_SIGNAL_LOSS_METER).ToString + '/UID';
                       end;

                       if TKindofEddystone.URL in LEddyHandler.KindofEddystones then
                       begin
                         ST2 := ST2 +#13+'URL: ' + LEddyHandler.EddystoneURL.URL;
                         if isDeviceInfChecked and (MSData[EDDY_FRAMETYPE_POS] = EDDYSTONE_URL) then
                           TX := ShortInt(MSData[EDDY_TX_POS] - EDDY_SIGNAL_LOSS_METER).ToString + '/URL';
                       end;

                       if TKindofEddystone.TLM in LEddyHandler.KindofEddystones then
                       begin
                         ST2 := ST2 +#13+'TLM:  BattVol: '+ LEddyHandler.EddystoneTLM.BattVoltageToString +
                         ', B.Temp: '+LEddyHandler.EddystoneTLM.BeaconTempToString;
                         ST2 := ST2 +#13+'  AdvPDUCount: '+LEddyHandler.EddystoneTLM.AdvPDUCountToString+
                         ', SincePOn: '+LEddyHandler.EddystoneTLM.TimeSincePowerOnToString;

                         if isDeviceInfChecked and (MSData[EDDY_FRAMETYPE_POS] = EDDYSTONE_TLM) then
                           TX := '/TLM';
                       end;

                     end;
                   end;
               end;

             ST2 := ST2 +#13+ DeviceName + ', ID: '+   DeviceIdentifier+#13;
             if isDeviceInfChecked then
               ST2 := ST2 +'TX: '+TX+', ';
              ST2 := ST2+'RSSI:'+FCurrentBeaconList[I].Rssi.ToString+', Distance: '+FCurrentBeaconList[I].Distance.ToString+' m';
             PrintIt(ST1,ST2);
           end;
        end;

      finally
        TMonitor.Exit(FBluetoothLEDeviceList);
      end;

      if (ListView1.Items.Count - 1) > Pos then
        for I := ListView1.Items.Count - 1 downto Pos + 1 do
          ListView1.Items.Delete(I);
    end;

  except
    On E : Exception do
      ShowMessage(E.Message);
  end;
end;

end.
