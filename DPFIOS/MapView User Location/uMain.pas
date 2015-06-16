unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  iOSapi.CoreLocation,

  DPF.iOS.Common,
  DPF.iOS.MapKit,
  DPF.iOS.BaseControl,
  DPF.iOS.MKMapView,
  DPF.iOS.UIButton,
  DPF.iOS.UIView,
  DPF.iOS.UILabel,
  DPF.iOS.ApplicationManager;

type
  TFMapView = class( TForm )
    DPFMapView1: TDPFMapView;
    DPFButton1: TDPFButton;
    DPFButton2: TDPFButton;
    DPFUIView1: TDPFUIView;
    DPFButton3: TDPFButton;
    DPFButton4: TDPFButton;
    DPFLabel1: TDPFLabel;
    DPFUIView2: TDPFUIView;
    DPFApplicationManager1: TDPFApplicationManager;
    DPFUIView3: TDPFUIView;
    DPFLabelSpeed: TDPFLabel;
    DPFLabelLocation: TDPFLabel;
    procedure DPFButton1Click( Sender: TObject );
    procedure DPFButton2Click( Sender: TObject );
    procedure DPFButton3Click( Sender: TObject );
    procedure DPFButton4Click( Sender: TObject );
    procedure FormShow( Sender: TObject );
    procedure DPFApplicationManager1MemoryWarning( Sender: TObject );
    procedure FormCreate( Sender: TObject );
    procedure DPFMapView1ZoomChanged( sender: TObject );
    procedure DPFMapView1DidUpdateUserLocation( sender: TObject; Location: TDPFLocation );

  private
    { Private declarations }
    Marker1 : MKPointAnnotation;
    Marker2 : MKPointAnnotation;
    Circle  : MKCircle;
    Polyline: MKPolyline;
    Polygon : MKPolygon;

    procedure OnPolygonClick( sender: TObject; PolygonView: MKPolygonView; Polygon: MKPolygon );
    procedure OnPolylineClick( sender: TObject; PolylineView: MKPolylineView; Polyline: MKPolyline );
    procedure OnCircleClick( sender: TObject; CircleView: MKCircleView; Circle: MKCircle );
    procedure OnAnnotationClick( sender: TObject; AnnotationView: MKAnnotationView; Annotation: MKAnnotation; AnnotationInfo: PAnnotationInfo );
    procedure OnAnnotationCalloutClick( sender: TObject; AnnotationView: MKAnnotationView; Annotation: MKAnnotation; AnnotationInfo: PAnnotationInfo );
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FMapView: TFMapView;

implementation

{$R *.fmx}

procedure TFMapView.DPFApplicationManager1MemoryWarning( Sender: TObject );
begin
  DPFLabelLocation.Text := 'Low Memory';

end;

procedure TFMapView.DPFButton1Click( Sender: TObject );
var
  Polys: TArray<CLLocationCoordinate2D>;
begin
  setLength( Polys, 4 );
  Polys[0].latitude  := 35.738362;
  Polys[0].longitude := 51.402916;
  Polys[1].latitude  := 35.738745;
  Polys[1].longitude := 51.408387;
  Polys[2].latitude  := 35.731185;
  Polys[2].longitude := 51.408355;
  Polys[3].latitude  := 35.734268;
  Polys[3].longitude := 51.402422;

  Circle   := DPFMapView1.AddCircle( 35.735775, 51.405775, 100, TAlphaColors.Blue, TAlphaColors.Darkblue, 5, 0.2, 'Circle Title', 'Circle SubTitle' );
  Polyline := DPFMapView1.AddPolyLine( Polys, TAlphaColors.Red, TAlphaColors.Black, 20, 0.6, 'Polyline Title', 'Polyline Sub Title' );

  setLength( Polys, 4 );
  Polys[0].latitude  := 35.735313;
  Polys[0].longitude := 51.404461;

  Polys[1].latitude  := 35.735766;
  Polys[1].longitude := 51.40725;

  Polys[2].latitude  := 35.733659;
  Polys[2].longitude := 51.406735;

  Polys[3].latitude  := 35.733798;
  Polys[3].longitude := 51.404632;
  Polygon            := DPFMapView1.AddPolygon( Polys, TAlphaColors.Yellow, TAlphaColors.Red, 10, 0.6, 'Polygon Title', 'Polygon Sub Title' );

  Marker1 := DPFMapView1.AddAnnotation( 'Iran', 'Tehran - Asad Abadi SQ.', 35.737177, 51.405807, 15, '', true, 1.0, pcRed, 'flag_iran.png', 'flag_iran.png', true, true );
  Marker2 := DPFMapView1.AddAnnotation( 'Iran', 'Tehran - Traffic Zone!', 35.736202, 51.403280, 15, 'TagStr 2', false, 1.0, pcGreen, 'trafficlight_on.png', 'flag_iran.png', true, true );
end;

procedure TFMapView.DPFButton2Click( Sender: TObject );
begin
  DPFMapView1.RemoveAnnotation( Marker1 );
  DPFMapView1.RemoveAnnotation( Marker2 );
  DPFMapView1.RemoveOverlay( Circle );
  DPFMapView1.RemoveOverlay( Polyline );
  DPFMapView1.RemoveOverlay( Polygon );
end;

procedure TFMapView.DPFButton3Click( Sender: TObject );
begin
  DPFMapView1.ZoomLevel := DPFMapView1.ZoomLevel + 1;
end;

procedure TFMapView.DPFButton4Click( Sender: TObject );
begin
  DPFMapView1.ZoomLevel := DPFMapView1.ZoomLevel - 1;
end;

procedure TFMapView.DPFMapView1DidUpdateUserLocation( sender: TObject; Location: TDPFLocation );
begin
  DPFLabelSpeed.Text    := 'Speed: ' + FormatFloat( '0.00', Location.speed );
  DPFLabelLocation.Text := 'Loc: ' + FormatFloat( '0.000000', Location.latitude ) + ' - ' + FormatFloat( '0.000000', Location.longitude );
end;

procedure TFMapView.DPFMapView1ZoomChanged( sender: TObject );
begin
  DPFLabel1.Text := FloatToStr( DPFMapView1.ZoomLevel );
end;

procedure TFMapView.FormCreate( Sender: TObject );
begin

  DPFMapView1.OnPolygonClick           := OnPolygonClick;
  DPFMapView1.OnPolylineClick          := OnPolylineClick;
  DPFMapView1.OnCircleClick            := OnCircleClick;
  DPFMapView1.OnAnnotationClick        := OnAnnotationClick;
  DPFMapView1.OnAnnotationCalloutClick := OnAnnotationCalloutClick;
end;

procedure TFMapView.FormShow( Sender: TObject );
begin
  DPFLabel1.Text := FloatToStr( DPFMapView1.ZoomLevel );
end;

procedure TFMapView.OnAnnotationCalloutClick( sender: TObject; AnnotationView: MKAnnotationView; Annotation: MKAnnotation; AnnotationInfo: PAnnotationInfo );
var
  tagStr: string;
begin
  tagStr := '';
  if assigned( AnnotationInfo ) then
    tagStr := AnnotationInfo^.TagStr;
  ShowAlert( 'call out Clicked on :  ' + UTF8ToString( Annotation.title.UTF8String ) + ', ' + AnnotationInfo^.TagStr );
end;

procedure TFMapView.OnAnnotationClick( sender: TObject; AnnotationView: MKAnnotationView; Annotation: MKAnnotation; AnnotationInfo: PAnnotationInfo );
var
  tagStr: string;
begin
  tagStr := '';
  if assigned( AnnotationInfo ) then
    tagStr := AnnotationInfo^.TagStr;
  ShowAlert( 'Clicked on :  ' + UTF8ToString( Annotation.title.UTF8String ) + ', ' + TagStr );
end;

procedure TFMapView.OnCircleClick( sender: TObject; CircleView: MKCircleView; Circle: MKCircle );
begin
  ShowAlert( 'Clicked on : ' + UTF8ToString( Circle.title.UTF8String ) );
end;

procedure TFMapView.OnPolygonClick( sender: TObject; PolygonView: MKPolygonView; Polygon: MKPolygon );
begin
  ShowAlert( 'Clicked on : ' + UTF8ToString( Polygon.title.UTF8String ) );
end;

procedure TFMapView.OnPolylineClick( sender: TObject; PolylineView: MKPolylineView; Polyline: MKPolyline );
begin
  ShowAlert( 'Clicked on :  ' + UTF8ToString( Polyline.title.UTF8String ) );
end;

procedure TFMapView.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
