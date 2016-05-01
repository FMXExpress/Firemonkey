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
unit Unit4;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Bluetooth, System.Bluetooth.Components, FMX.StdCtrls,
  FMX.Controls.Presentation,  System.Generics.Collections, FMX.Layouts,
  FMX.ListBox, System.Math;

const
  //TSCANRESPONSE POSITIONS
  BEACON_TYPE_POSITION = 2;
  BEACON_GUID_POSITION = 4;
  BEACON_MAJOR_POSITION = 20;
  BEACON_MINOR_POSITION = 22;
  BEACON_ST_TYPE: Word = $0215;

type

 TBeaconDevice = Record
    Device: TBluetoothLEDevice;
    GUID: TGUID;
    Major: Word;
    Minor: Word;
    TxPower: Integer;
    Rssi: Integer;
    Distance: Double;
    Alt: Boolean;
  end;
  TBeaconDeviceList = TList<TBeaconDevice>;

type
  TForm4 = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    ListBox1: TListBox;
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    FManager: TBluetoothLEManager;
    BeaconDeviceList: TBeaconDeviceList;
  public
    { Public declarations }
    procedure DiscoverLEDevice(const Sender: TObject; const ADevice: TBluetoothLEDevice; Rssi: Integer; const ScanResponse: TScanResponse);
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}

procedure TForm4.Button1Click(Sender: TObject);
begin
  if FManager = nil then
  begin
    FManager :=  TBluetoothLEManager.Current;
    FManager.OnDiscoverLeDevice := DiscoverLEDevice;
  end;
  FManager.StartDiscovery(20000);
end;

procedure TForm4.Button2Click(Sender: TObject);
begin
  FManager.CancelDiscovery;
end;

procedure TForm4.DiscoverLEDevice(const Sender: TObject; const ADevice: TBluetoothLEDevice;
          Rssi: Integer; const ScanResponse: TScanResponse);

  procedure BinToHex(Buffer: TBytes; Text: PChar; BufSize: Integer);
  const
    Convert: array[0..15] of WideChar = '0123456789ABCDEF';
  var
    I: Integer;
  begin
    for I := 0 to BufSize - 1 do
    begin
      Text[0] := Convert[Buffer[I] shr 4];
      Text[1] := Convert[Buffer[I] and $F];
      Inc(Text, 2);
    end;
  end;

  function DecodeScanResponse: TBeaconDevice;
  const
    GUID_LENGTH = 16;
    MARK_POSITION = 9;
  var
    LSTBuff: string;
  begin
    SetLength(LSTBuff, GUID_LENGTH * 2);
    BinToHex(@ScanResponse.Items[TScanResponseKey.ManufacturerSpecificData][BEACON_GUID_POSITION], PChar(LSTBuff), GUID_LENGTH);
    LSTBuff := '{' + LSTBuff + '}';
    LSTBuff.Insert(MARK_POSITION,'-');
    LSTBuff.Insert(MARK_POSITION + 5,'-');
    LSTBuff.Insert(MARK_POSITION + 10,'-');
    LSTBuff.Insert(MARK_POSITION + 15,'-');
    Result.GUID := TGUID.Create(LSTBuff);


    WordRec(Result.Major).Hi := ScanResponse.Items[TScanResponseKey.ManufacturerSpecificData][BEACON_MAJOR_POSITION];
    WordRec(Result.Major).Lo := ScanResponse.Items[TScanResponseKey.ManufacturerSpecificData][BEACON_MAJOR_POSITION + 1];
    WordRec(Result.Minor).Hi := ScanResponse.Items[TScanResponseKey.ManufacturerSpecificData][BEACON_MINOR_POSITION];
    WordRec(Result.Minor).Lo := ScanResponse.Items[TScanResponseKey.ManufacturerSpecificData][BEACON_MINOR_POSITION + 1];

    if (ScanResponse.Items[TScanResponseKey.ManufacturerSpecificData][BEACON_TYPE_POSITION] = WordRec(BEACON_ST_TYPE).Hi) then //iBeacon
    begin
      Result.TxPower := ShortInt(ScanResponse.Items[TScanResponseKey.ManufacturerSpecificData]
                          [length(ScanResponse.Items[TScanResponseKey.ManufacturerSpecificData]) - 1]);
      Result.Alt := False;
    end
    else
    begin
      Result.TxPower := ShortInt(ScanResponse.Items[TScanResponseKey.ManufacturerSpecificData]
                          [length(ScanResponse.Items[TScanResponseKey.ManufacturerSpecificData]) - 2]);
      Result.Alt := True;
    end;
      Result.Rssi := Rssi;
      Result.Distance := FManager.RssiToDistance(Rssi, Result.TxPower, 0.5);
      Result.Device := ADevice;
  end;

var
  LBeaconDevice: TBeaconDevice;
  NewBeacon: Integer;
  I: Integer;
  ScanData: string;
begin
  if ScanResponse.ContainsKey(TScanResponseKey.ManufacturerSpecificData) then
  begin
    I := Length(ScanResponse.Items[TScanResponseKey.ManufacturerSpecificData]);
    if I<21 then
      exit;
    LBeaconDevice := DecodeScanResponse;
    NewBeacon := 0;
    if BeaconDeviceList.Count > 0 then
    begin
      for I := 0 to BeaconDeviceList.count-1 do
        if ((BeaconDeviceList[I].GUID = LBeaconDevice.GUID) and (BeaconDeviceList[I].Major = LBeaconDevice.Major)
          and (BeaconDeviceList[I].Minor = LBeaconDevice.Minor)) then
        begin
          BeaconDeviceList[I] := LBeaconDevice;
          NewBeacon := I+1;
          Break;
        end;
    end;

    TThread.Synchronize(nil, procedure
    begin
      if NewBeacon = 0 then
      begin
        BeaconDeviceList.Add(LBeaconDevice);
        ListBox1.Items.Add('-------------------------------------');
        ScanData := 'Beacon Found: ' + Adevice.DeviceName;
        if LBeaconDevice.Alt then
          ScanData := ScanData + '; AltB'
        else
          ScanData := ScanData + '; iB';
        ListBox1.Items.Add(ScanData);
        ScanData := '';
        if ScanResponse.ContainsKey(TScanResponseKey.CompleteLocalName) then
          ScanData := TEncoding.UTF8.GetString(ScanResponse.Items[TScanResponseKey.CompleteLocalName]);
        ListBox1.Items.Add('Device Complete name: ' + ScanData);
        ListBox1.Items.Add(' UUID: '+LBeaconDevice.GUID.ToString);
        ListBox1.Items.Add(' Major:'+LBeaconDevice.Major.ToString+',  Minor:'+LBeaconDevice.Minor.ToString+',  txPower: '+LBeaconDevice.TxPower.ToString());
        ListBox1.Items.Add(' Rssi: ' + LBeaconDevice.Rssi.ToString + Format(' Distance: %f m',[LBeaconDevice.Distance]));
      end
      else
      begin
        ScanData := 'Beacon Found: ' + Adevice.DeviceName;
        if LBeaconDevice.Alt then
          ScanData := ScanData + '; AltB'
        else
          ScanData := ScanData + '; iB';
        ListBox1.Items[(NewBeacon-1)*6+1] := ScanData;
        ListBox1.Items[(NewBeacon-1)*6+5] := (' Rssi: ' + LBeaconDevice.Rssi.ToString + Format(' Distance: %f m',[LBeaconDevice.Distance]));
      end;
    end);
  end;

end;

procedure TForm4.FormShow(Sender: TObject);
begin
  BeaconDeviceList := TBeaconDeviceList.Create;
end;

end.
