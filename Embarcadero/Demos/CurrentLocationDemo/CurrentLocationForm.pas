unit CurrentLocationForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Sensors,
  System.Sensors.Components, FMX.Maps, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Objects, FMX.ListView.Types, FMX.ListView, FMX.ListBox, FMX.Layouts,
  System.Actions, FMX.ActnList, FMX.StdActns, FMX.MediaLibrary.Actions;

type
  TMapsForm = class(TForm)
    MapView1: TMapView;
    LocationSensor1: TLocationSensor;
    Long: TLabel;
    Lat: TLabel;
    BitmapSource: TImage;
    ListBox1: TListBox;
    LocationSensorItem: TListBoxItem;
    LongitudeItem: TListBoxItem;
    LatitudeItem: TListBoxItem;
    Switch1: TSwitch;
    ScreenshotItem: TListBoxItem;
    btnShareImage: TButton;
    ToolBar1: TToolBar;
    Label1: TLabel;
    Image1: TImage;
    ToolBar2: TToolBar;
    Label2: TLabel;
    ActionList1: TActionList;
    ShowShareSheetAction1: TShowShareSheetAction;
    btnScreenshot: TSpeedButton;
    procedure EnableLocationSensorClick(Sender: TObject);
    procedure LocationSensor1LocationChanged(Sender: TObject; const OldLocation,
      NewLocation: TLocationCoord2D);
    procedure Switch1Switch(Sender: TObject);
    procedure MapView1MarkerClick(Marker: TMapMarker);
    procedure ShowShareSheetAction1BeforeExecute(Sender: TObject);
    procedure btnScreenshotClick(Sender: TObject);
  private
    const Accuracy = 0.0005;
  private
    FCurrentPosition: TLocationCoord2D;
    { Private declarations }
    procedure SnapShotReady(const Bitmap: TBitmap);
  public
    { Public declarations }
  end;

  TLocationCoord2DHelper = record helper for TLocationCoord2D
    function Distance(const NewCoord: TLocationCoord2D): Double;
  end;

var
  MapsForm: TMapsForm;

implementation

{$R *.fmx}

procedure TMapsForm.EnableLocationSensorClick(Sender: TObject);
begin
  LocationSensor1.Active := True;
end;

procedure TMapsForm.LocationSensor1LocationChanged(Sender: TObject;
  const OldLocation, NewLocation: TLocationCoord2D);
var
  MyLocation: TMapCoordinate;
  Desqr: TMapMarkerDescriptor;
begin
  Lat.Text  := Format('%2.6f', [NewLocation.Latitude]);
  Long.Text := Format('%2.6f', [NewLocation.Longitude]);
  MyLocation := TMapCoordinate.Create(StrToFloat(Lat.Text),StrToFloat(Long.Text));
  MapView1.Location :=  MyLocation;
  if FCurrentPosition.Distance(NewLocation) > Accuracy then
  begin
    FCurrentPosition := NewLocation;
    Desqr := TMapMarkerDescriptor.Create(MyLocation, 'Dropped marker');
    Desqr.Icon := BitmapSource.Bitmap;
    Desqr.Draggable := True;
    MapView1.AddMarker(Desqr);
    MapView1.Zoom := 7;
  end;
end;


procedure TMapsForm.MapView1MarkerClick(Marker: TMapMarker);
begin
  Marker.DisposeOf;
end;

procedure TMapsForm.Switch1Switch(Sender: TObject);
begin
  LocationSensor1.Active := Switch1.IsChecked;
end;


procedure TMapsForm.ShowShareSheetAction1BeforeExecute(Sender: TObject);
begin
  ShowShareSheetAction1.Bitmap.Assign(Image1.Bitmap);
end;

procedure TMapsForm.SnapShotReady(const Bitmap: TBitmap);
begin
  Image1.Bitmap.Assign(Bitmap);
end;

procedure TMapsForm.btnScreenshotClick(Sender: TObject);
begin
  MapView1.Snapshot(SnapshotReady);
end;

{ TLocationCoord2DHelper }

function TLocationCoord2DHelper.Distance(const NewCoord: TLocationCoord2D): Double;
begin
  Result := Sqrt(Sqr(Abs(Self.Latitude - NewCoord.Latitude)) + Sqr(Abs(Self.Longitude - NewCoord.Longitude)));
end;

end.
