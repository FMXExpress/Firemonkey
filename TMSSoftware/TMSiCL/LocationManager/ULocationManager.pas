unit ULocationManager;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TMSNativeCLLocationManager, FMX.TMSNativeUIBaseControl,
  FMX.TMSNativeMKMapView, FMX.TMSNativeUICore;

type
  TForm1185 = class(TForm)
    TMSFMXNativeMKMapView1: TTMSFMXNativeMKMapView;
    TMSFMXNativeCLLocationManager1: TTMSFMXNativeCLLocationManager;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure TMSFMXNativeCLLocationManager1DidChangeAuthorizationStatus(
      Sender: TObject;
      AAuthorizationStatus: TTMSFMXNativeCLLocationManagerAuthorizationStatus);
    procedure TMSFMXNativeCLLocationManager1DidUpdateLocations(Sender: TObject;
      ALocations: TArray<FMX.TMSNativeUICore.TTMSFMXNativeCLLocation>);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure StartLocationUpdates;
  end;

var
  Form1185: TForm1185;

implementation

{$R *.fmx}

procedure TForm1185.Button1Click(Sender: TObject);
begin
  if TMSFMXNativeCLLocationManager1.LocationServicesEnabled then
  begin
    if TMSFMXNativeCLLocationManager1.AuthorizationStatus = asAuthorizationStatusNotDetermined then
      TMSFMXNativeCLLocationManager1.RequestAlwaysAuthorization
    else
      StartLocationUpdates;
  end;
end;

procedure TForm1185.StartLocationUpdates;
begin
  TMSFMXNativeCLLocationManager1.StartUpdatingLocation;
end;

procedure TForm1185.TMSFMXNativeCLLocationManager1DidChangeAuthorizationStatus(
  Sender: TObject;
  AAuthorizationStatus: TTMSFMXNativeCLLocationManagerAuthorizationStatus);
begin
  if AAuthorizationStatus = asAuthorizationStatusAuthorizedAlways then
    StartLocationUpdates;
end;

procedure TForm1185.TMSFMXNativeCLLocationManager1DidUpdateLocations(
  Sender: TObject;
  ALocations: TArray<FMX.TMSNativeUICore.TTMSFMXNativeCLLocation>);
var
  ann: TTMSFMXNativeMKAnnotation;
begin
  if Length(ALocations) > 0 then
  begin
    TMSFMXNativeMKMapView1.BeginUpdate;
    if TMSFMXNativeMKMapView1.Annotations.Count = 0 then
      ann := TMSFMXNativeMKMapView1.Annotations.Add
    else
      ann := TMSFMXNativeMKMapView1.Annotations[0];

    ann.Location := MakeMapLocation(ALocations[0].Coordinate.Latitude, ALocations[0].Coordinate.Longitude);
    TMSFMXNativeMKMapView1.SetCenterLocation(ann.Location, True);
    TMSFMXNativeMKMapView1.EndUpdate;
  end;
end;

end.
