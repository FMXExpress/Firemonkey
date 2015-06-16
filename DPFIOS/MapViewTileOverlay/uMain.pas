unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  DPF.iOS.MapKit,

  DPF.iOS.BaseControl,
  DPF.iOS.MKMapView,
  DPF.iOS.UIButton,
  DPF.iOS.UIView,
  DPF.iOS.Common,
  DPF.iOS.UILabel,
  DPF.iOS.UITextField;

type
  TFMapView = class( TForm )
    DPFMapView1: TDPFMapView;
    procedure DPFMapView1DrawCustomImage( sender: TObject; ZoomLevel: byte; var Image: string; var Alpha: Single );

  private
    { Private declarations }
    Marker: MKPointAnnotation;

  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FMapView: TFMapView;

implementation

{$R *.fmx}

procedure TFMapView.DPFMapView1DrawCustomImage( sender: TObject; ZoomLevel: byte; var Image: string; var Alpha: Single );
begin
  Image := 'clouds.jpg';
end;

procedure TFMapView.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
