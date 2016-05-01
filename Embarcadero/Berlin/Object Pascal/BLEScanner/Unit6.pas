//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit Unit6;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Bluetooth, FMX.StdCtrls, System.Bluetooth.Components, FMX.Layouts,
  FMX.ListBox, FMX.Controls.Presentation;

type
  TForm6 = class(TForm)
    Panel1: TPanel;
    ListBox1: TListBox;
    BluetoothLE1: TBluetoothLE;
    Button1: TButton;
    Button2: TButton;
    Timer1: TTimer;
    ProgressBar1: TProgressBar;
    ListBox2: TListBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure BluetoothLE1DiscoverLEDevice(const Sender: TObject;
      const ADevice: TBluetoothLEDevice; Rssi: Integer;
      const ScanResponse: TScanResponse);
    procedure Timer1Timer(Sender: TObject);
    procedure BluetoothLE1EndDiscoverDevices(const Sender: TObject;
      const ADeviceList: TBluetoothLEDeviceList);
    procedure FormShow(Sender: TObject);
    procedure BluetoothLE1ServicesDiscovered(const Sender: TObject;
      const AServiceList: TBluetoothGattServiceList);
    procedure ListBox1ItemClick(const Sender: TCustomListBox;
      const Item: TListBoxItem);
  private
    Scanning: Boolean;
    ScanningStart: DWORD;
    { Private declarations }
  public
    { Public declarations }
  end;

const
  ScanningTime = 10000; // 10s in msecs

var
  Form6: TForm6;

implementation

{$R *.fmx}

procedure TForm6.BluetoothLE1DiscoverLEDevice(const Sender: TObject;
  const ADevice: TBluetoothLEDevice; Rssi: Integer;
  const ScanResponse: TScanResponse);
var
  Name: string;
  I: Integer;
  DCount: Integer;
  NumberOfDevices: Integer;
begin
  DCount := BluetoothLE1.DiscoveredDevices.Count;
  NumberOfDevices := Listbox1.Count;
  for I  := 0 to DCount - 1 do
  begin
    Name := BluetoothLE1.DiscoveredDevices[I].DeviceName;
    if Name = '' then
      Name := 'Unknown device';
    Name := ' - '+ Name + ' - ' + BluetoothLE1.DiscoveredDevices[I].Identifier;
    if NumberOfDevices = I then
      Listbox1.Items.Add((NumberOfDevices + 1).ToString+Name)
    else
      Listbox1.Items[I] := (I + 1).ToString+Name;
  end;
end;

procedure TForm6.BluetoothLE1EndDiscoverDevices(const Sender: TObject;
  const ADeviceList: TBluetoothLEDeviceList);
begin
  if Scanning then
    ProgressBar1.Value := ProgressBar1.Max;
  Timer1.Enabled := False;
  Scanning := False;
end;

procedure TForm6.BluetoothLE1ServicesDiscovered(const Sender: TObject;
  const AServiceList: TBluetoothGattServiceList);
var
  I, C: Integer;
begin
  if AServiceList.Count > 0 then
  begin
    for I := 0 to AServiceList.Count - 1 do
    begin
      Listbox2.Items.Add((I + 1).ToString+' - '+AServiceList[I].UUIDName+' - '+AServiceList[I].UUID.ToString);
      for C := 0 to AServiceList[I].Characteristics.Count - 1 do
        Listbox2.Items.Add('    - '+AServiceList[I].Characteristics[C].UUIDName+' - '+AServiceList[I].Characteristics[C].UUID.ToString);
    end;
  end
  else
    Listbox2.Items.Add('- Not allow access or no services');
    Listbox1.Enabled := True;
end;

procedure TForm6.Button1Click(Sender: TObject);
begin
  if not Scanning then
  begin
    Listbox1.Clear;
    ScanningStart := TThread.GetTickCount;
    BluetoothLE1.DiscoverDevices(ScanningTime);
    ProgressBar1.Value := 0;
    Timer1.Enabled := True;
    Scanning := True;
  end;
end;

procedure TForm6.Button2Click(Sender: TObject);
begin
  Timer1.Enabled := False;
  Scanning := False;
  BluetoothLE1.CancelDiscovery;
end;

procedure TForm6.FormShow(Sender: TObject);
begin
  Scanning := False;
end;

procedure TForm6.ListBox1ItemClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
begin
  Button2Click(Sender);
  Listbox2.Clear;
  Listbox2.Items.Add('- Discovering services -->');
  TThread.CreateAnonymousThread(procedure begin
    if not BluetoothLE1.DiscoveredDevices[ListBox1.ItemIndex].DiscoverServices then
      TThread.Synchronize(nil, procedure begin
        Listbox2.Items.Add('- Discover services not allow');
        Listbox1.Enabled := True;
      end);
  end).Start;
  Listbox1.Enabled := False;
end;

procedure TForm6.Timer1Timer(Sender: TObject);
var
  LElapsed: DWORD;
begin
  LElapsed := TThread.GetTickCount - ScanningStart;
  ProgressBar1.Value := ProgressBar1.Max * LElapsed.ToSingle/ScanningTime;
end;

end.
