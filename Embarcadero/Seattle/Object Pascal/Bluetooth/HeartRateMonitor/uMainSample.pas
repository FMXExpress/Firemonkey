unit uMainSample;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, System.Bluetooth,
  uBluetoothComponent;

type
  TForm4 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FBluetooth: TBluetoothLE;
    procedure OnEndDiscoverDevices(const Sender: TObject; const ADeviceList: TBluetoothLEDeviceList);
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}

procedure TForm4.Button1Click(Sender: TObject);
var
  List: TBluetoothUUIDList;
begin
  FBluetooth := TBluetoothLE.Create(nil);
  FBluetooth.OnEndDiscoverDevices := OnEndDiscoverDevices;
  //FBluetooth.DiscoverDevices(5000);
  SetLength(List, 1);
  List[0] := StringToGUID('{0000180D-0000-1000-8000-00805F9B34FB}');
  FBluetooth.DiscoverDevices(5000, List);
end;

procedure TForm4.OnEndDiscoverDevices(const Sender: TObject; const ADeviceList: TBluetoothLEDeviceList);
begin

end;

end.
