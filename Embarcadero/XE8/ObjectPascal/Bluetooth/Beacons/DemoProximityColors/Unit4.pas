unit Unit4;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Beacon,
  FMX.StdCtrls;

type
  TForm4 = class(TForm)
    Company: TLabel;
    BeaconType: TLabel;
    MajorMinor: TLabel;
    Button1: TButton;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FBeaconManager: TBeaconManager;
    FisScanning: Boolean;
    procedure BeaconProximity(const Sender: TObject; const ABeacon: IBeacon; Proximity: TBeaconProximity);
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}

procedure TForm4.BeaconProximity(const Sender: TObject; const ABeacon: IBeacon; Proximity: TBeaconProximity);
begin
  if Proximity = TBeaconProximity.Inmediate  then
  begin
    case ABeacon.Major of
      10: Form4.Fill.Color := TAlphaColorRec.Springgreen;
      20: Form4.Fill.Color := TAlphaColorRec.Slateblue;
      30: Form4.Fill.Color := TAlphaColorRec.Aqua;
    end;
    Company.Text := 'Estimote';
    BeaconType.Text := 'iBeacon';
    MajorMinor.Text := 'Major: '+ABeacon.Major.ToString+' Minor: '+ABeacon.Minor.ToString;
  end;
end;

procedure TForm4.Button1Click(Sender: TObject);
begin
  if FisScanning then
  begin
    FBeaconManager.StopScan;
    Button1.Text := 'START';
    FisScanning := False;
  end
  else
  begin
    FBeaconManager.StartScan;
    Button1.Text := 'STOP';
    FisScanning := True;
  end;
end;

procedure TForm4.FormShow(Sender: TObject);
var
  GUID: TGUID;
begin
  if FBeaconManager = nil then
  begin
    FBeaconManager := TBeaconManager.GetBeaconManager(TBeaconScanMode.Standard);
    FBeaconManager.OnBeaconProximity := BeaconProximity;
  end;
  GUID := StringToGUID('{B9407F30-F5F8-466E-AFF9-25556B57FE6D}'); //Estimote
  FBeaconManager.RegisterBeacon(GUID);

  FBeaconManager.CalcMode := TBeaconCalcMode.Raw;
  FBeaconManager.ScanningTime := 109;
  FBeaconManager.ScanningSleepingTime := 15;
  FBeaconManager.StartScan;
  FisScanning := True;
end;

procedure TForm4.FormDestroy(Sender: TObject);
begin
  FBeaconManager.StopScan;
end;

end.
