unit UMap;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TMSNativeMKMapView,
  FMX.TMSNativeUIBaseControl, FMX.TMSNativeUIToolBar, iOSApi.Foundation,
  FMX.TMSNativeUIImageView, iOSApi.UIKit,
  FMX.TMSNativeUIPopoverController, FMX.TMSNativeUIButton, FMX.TMSNativeUICore;

type
  TForm913 = class(TForm)
    TMSFMXNativeUIToolBar1: TTMSFMXNativeUIToolBar;
    TMSFMXNativeMKMapView1: TTMSFMXNativeMKMapView;
    TMSFMXNativeUIPopoverController1: TTMSFMXNativeUIPopoverController;
    TMSFMXNativeUIImageView1: TTMSFMXNativeUIImageView;
    procedure FormCreate(Sender: TObject);
    procedure TMSFMXNativeUIToolBar1ItemClick(ASender: TObject;
      AItem: TTMSFMXNativeUIToolBarItem);
    procedure TMSFMXNativeMKMapView1DidUpdateUserLocation(Sender: TObject;
      AUserLocation: TTMSFMXNativeMKMapLocation);
    procedure TMSFMXNativeMKMapView1AnnotationRightCalloutAccessoryTapped(
      Sender: TObject; AAnnotation: TTMSFMXNativeMKAnnotation);
    procedure TMSFMXNativeUIButton1Click(Sender: TObject);
  private
    { Private declarations }
    procedure AddItems;
  public
    { Public declarations }
  end;

var
  Form913: TForm913;

implementation

{$R *.fmx}

function GetRootDirectory: String;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

procedure TForm913.AddItems;
var
  lat, lon: Double;
  lc: TTMSFMXNativeMKMapLocation;
  b: TTMSFMXNativeUIButton;
begin
  TMSFMXNativeMKMapView1.RemoveAllAnnotations;
  TMSFMXNativeMKMapView1.BeginUpdate;
  with TMSFMXNativeMKMapView1.Annotations.Add do
  begin
    Title := 'Paris';
    SubTitle := 'The capital and most populous city of France';
    lat := 48.856667;
    lon := 2.350833;
    lc.Latitude := lat;
    lc.Longitude := lon;
    Location := lc;
    PinColor := TTMSFMXNativeMKPinAnnotationColor(Random(3));
    AnimatesDrop := True;
    b := TTMSFMXNativeUIButton.Create(Self);
    b.Width := 32;
    b.Height := 32;
    b.Style := bsButtonTypeDetailDisclosure;
    RightCalloutAccessoryView := b;
    DataString := GetRootDirectory + 'Paris.jpg';
  end;

  with TMSFMXNativeMKMapView1.Annotations.Add do
  begin
    Title := 'Berlin';
    SubTitle := 'The capital city of Germany and one of the 16 states of Germany';
    lat := 52.519171;
    lon := 13.406091199999992;
    lc.Latitude := lat;
    lc.Longitude := lon;
    Location := lc;
    PinColor := TTMSFMXNativeMKPinAnnotationColor(Random(3));
    AnimatesDrop := True;
    b := TTMSFMXNativeUIButton.Create(Self);
    b.Width := 32;
    b.Height := 32;
    b.Style := bsButtonTypeDetailDisclosure;
    RightCalloutAccessoryView := b;
    DataString := GetRootDirectory + 'Berlin.jpg';
  end;

  with TMSFMXNativeMKMapView1.Annotations.Add do
  begin
    Title := 'Brussels';
    SubTitle := 'The capital city of Belgium.';
    lat := 50.8503396;
    lon := 4.351710300000036;
    lc.Latitude := lat;
    lc.Longitude := lon;
    Location := lc;
    PinColor := TTMSFMXNativeMKPinAnnotationColor(Random(3));
    AnimatesDrop := True;
    b := TTMSFMXNativeUIButton.Create(Self);
    b.Width := 32;
    b.Height := 32;
    b.Style := bsButtonTypeDetailDisclosure;
    RightCalloutAccessoryView := b;
    DataString := GetRootDirectory + 'Brussels.jpg';
  end;

  with TMSFMXNativeMKMapView1.Annotations.Add do
  begin
    Title := 'Dubai';
    SubTitle := 'A city in the United Arab Emirates, located within the emirate';
    lat := 25.271139;
    lon := 55.30748500000004;
    lc.Latitude := lat;
    lc.Longitude := lon;
    Location := lc;
    PinColor := TTMSFMXNativeMKPinAnnotationColor(Random(3));
    AnimatesDrop := True;
    b := TTMSFMXNativeUIButton.Create(Self);
    b.Width := 32;
    b.Height := 32;
    b.Style := bsButtonTypeDetailDisclosure;
    RightCalloutAccessoryView := b;
    DataString := GetRootDirectory + 'Dubai.jpg';
  end;

  with TMSFMXNativeMKMapView1.Annotations.Add do
  begin
    Title := 'New York City';
    SubTitle := 'The most populous city in the United States';
    lat := 40.7142;
    lon := -74.0064;
    lc.Latitude := lat;
    lc.Longitude := lon;
    Location := lc;
    PinColor := TTMSFMXNativeMKPinAnnotationColor(Random(3));
    AnimatesDrop := True;
    b := TTMSFMXNativeUIButton.Create(Self);
    b.Width := 32;
    b.Height := 32;
    b.Style := bsButtonTypeDetailDisclosure;
    RightCalloutAccessoryView := b;
    DataString := GetRootDirectory + 'NewYorkCity.jpg';
  end;

  with TMSFMXNativeMKMapView1.Annotations.Add do
  begin
    Title := 'Tunis';
    SubTitle := 'The capital of both the Tunisian Republic and the Tunis Governorate';
    lat := 36.81881;
    lon := 10.16596;
    lc.Latitude := lat;
    lc.Longitude := lon;
    Location := lc;
    PinColor := TTMSFMXNativeMKPinAnnotationColor(Random(3));
    AnimatesDrop := True;
    b := TTMSFMXNativeUIButton.Create(Self);
    b.Width := 32;
    b.Height := 32;
    b.Style := bsButtonTypeDetailDisclosure;
    RightCalloutAccessoryView := b;
    DataString := GetRootDirectory + 'Tunis.jpg';
  end;

  with TMSFMXNativeMKMapView1.Annotations.Add do
  begin
    Title := 'Budapest';
    SubTitle := 'The capital and the largest city of Hungary';
    lat := 47.497912;
    lon := 19.04023499999994;
    lc.Latitude := lat;
    lc.Longitude := lon;
    Location := lc;
    PinColor := TTMSFMXNativeMKPinAnnotationColor(Random(3));
    AnimatesDrop := True;
    b := TTMSFMXNativeUIButton.Create(Self);
    b.Width := 32;
    b.Height := 32;
    b.Style := bsButtonTypeDetailDisclosure;
    RightCalloutAccessoryView := b;
    DataString := GetRootDirectory + 'Budapest.jpg';
  end;

  TMSFMXNativeMKMapView1.EndUpdate;
end;

procedure TForm913.FormCreate(Sender: TObject);
var
  rgn: TTMSFMXNativeMKMapRegion;
begin
  TMSFMXNativeUIToolBar1.BeginUpdate;
  TMSFMXNativeUIToolBar1.Items.Add.Text := 'Toggle Current Location';
  TMSFMXNativeUIToolBar1.Items.Add.Text := 'Toggle Zooming';
  TMSFMXNativeUIToolBar1.Items.Add.Text := 'Toggle Scrolling';
  TMSFMXNativeUIToolBar1.Items.Add.Text := 'Add Annotations';
  TMSFMXNativeUIToolBar1.Items.Add.Text := 'Locate Bermuda Triangle';
  TMSFMXNativeUIToolBar1.Items.Add.Text := 'Add Circle';
  TMSFMXNativeUIToolBar1.Items.Add.Text := 'Add Image';
  TMSFMXNativeUIToolBar1.EndUpdate;

  rgn.Center.Latitude := TMSFMXNativeMKMapView1.MapView.centerCoordinate.latitude;
  rgn.Center.Longitude := TMSFMXNativeMKMapView1.MapView.centerCoordinate.longitude;
  rgn.Span.latitudeDelta := 180;
  rgn.Span.longitudeDelta := 360;

  TMSFMXNativeMKMapView1.SetRegion(rgn, True);
end;

procedure TForm913.TMSFMXNativeMKMapView1AnnotationRightCalloutAccessoryTapped(
  Sender: TObject; AAnnotation: TTMSFMXNativeMKAnnotation);
var
  r: TRectF;
  rs: NSRect;
  vw: UIView;
begin
  if Assigned(AAnnotation.AnnotationView) then
  begin
    vw := AAnnotation.AnnotationView.rightCalloutAccessoryView;
    if Assigned(vw) then
    begin
      rs := vw.bounds;
      r.Left := rs.origin.x;
      r.Top := rs.origin.y;
      r.Width := rs.size.width;
      r.Height := rs.size.height;
      TMSFMXNativeUIImageView1.Bitmap.Assign(nil);
      TMSFMXNativeUIImageView1.Bitmap.LoadFromFile(AAnnotation.DataString);
      TMSFMXNativeUIImageView1.Width := TMSFMXNativeUIImageView1.Bitmap.Width;
      TMSFMXNativeUIImageView1.Height := TMSFMXNativeUIImageView1.Bitmap.Height;
      TMSFMXNativeUIPopoverController1.ShowFromRectInView(r, vw);
    end;
  end;
end;

procedure TForm913.TMSFMXNativeMKMapView1DidUpdateUserLocation(Sender: TObject;
  AUserLocation: TTMSFMXNativeMKMapLocation);
begin
  TMSFMXNativeMKMapView1.SetCenterLocation(AUserLocation, True);
end;

procedure TForm913.TMSFMXNativeUIButton1Click(Sender: TObject);
begin
  TMSFMXNativeMKMapView1.BeginUpdate;
  TMSFMXNativeMKMapView1.AddTiling('http://mt0.google.com/vt/x={x}&y={y}&z={z}');
  TMSFMXNativeMKMapView1.EndUpdate;
end;

procedure TForm913.TMSFMXNativeUIToolBar1ItemClick(ASender: TObject;
  AItem: TTMSFMXNativeUIToolBarItem);
var
  arr: TArray<TTMSFMXNativeMKMapLocation>;
  c: TTMSFMXNativeMKOverlay;
  tl, tr, bl, br, mr: TTMSFMXNativeMKMapLocation;
  splat, splon: Double;
  rgn: TTMSFMXNativeMKMapRegion;
begin
  case AItem.Index of
    0: TMSFMXNativeMKMapView1.ShowsUserLocation := not TMSFMXNativeMKMapView1.ShowsUserLocation;
    1: TMSFMXNativeMKMapView1.ZoomEnabled := not TMSFMXNativeMKMapView1.ZoomEnabled;
    2: TMSFMXNativeMKMapView1.ScrollEnabled := not TMSFMXNativeMKMapView1.ScrollEnabled;
    3: AddItems;
    4:
    begin
      TMSFMXNativeMKMapView1.BeginUpdate;

      SetLength(arr, 4);

      //Bermuda triangle
      arr[0].Latitude :=  25.774252;
      arr[0].Longitude := -80.190262;
      arr[1].Latitude := 18.466465;
      arr[1].Longitude := -66.118292;
      arr[2].Latitude := 32.321384;
      arr[2].Longitude := -64.75737;
      arr[3] := arr[0];

      c := TMSFMXNativeMKMapView1.AddPolyline(arr);
      c.LineColor := TAlphaColorRec.Red;
      TMSFMXNativeMKMapView1.EndUpdate;
    end;
    5:
    begin
      TMSFMXNativeMKMapView1.BeginUpdate;
      c := TMSFMXNativeMKMapView1.AddCircle(TMSFMXNativeMKMapView1.XYToCoordinate(TMSFMXNativeMKMapView1.Width / 2,
        TMSFMXNativeMKMapView1.Height / 2), 500000);
      c.LineWidth := 3;
      c.LineColor := TAlphaColorRec.Greenyellow;
      c.Color := TAlphaColorRec.Darkgoldenrod;
      c.Opacity := 0.5;
      c.LineOpacity := 0.5;
      TMSFMXNativeMKMapView1.EndUpdate;
    end;
    6:
    begin
      TMSFMXNativeMKMapView1.BeginUpdate;
      tl := MakeMapLocation(34.4311, -118.6012);
      tr := MakeMapLocation(34.4311, -118.5912);
      bl := MakeMapLocation(34.4194, -118.6012);
      br := MakeMapLocation(34.4194, -118.5912);
      mr := MakeMapLocation(34.4248, -118.5971);
      splat := Abs(br.Latitude - tl.Latitude);
      splon := Abs(br.Longitude - tl.Longitude);
      rgn.Center.Latitude := mr.Latitude;
      rgn.Center.Longitude := mr.Longitude;
      rgn.Span.latitudeDelta := splat;
      rgn.Span.longitudeDelta := splon;
      TMSFMXNativeMKMapView1.SetRegion(rgn, True);
      TMSFMXNativeMKMapView1.AddImage(ExtractFilePath(ParamStr(0)) + 'overlay_park.png', tl, br);
      TMSFMXNativeMKMapView1.EndUpdate;
    end;
  end;
end;

end.
