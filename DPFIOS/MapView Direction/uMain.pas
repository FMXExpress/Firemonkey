unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,

  DPF.iOS.BaseControl,
  DPF.iOS.MapKit,
  DPF.iOS.MKMapView,
  DPF.iOS.UIButton,
  DPF.iOS.UIView,
  DPF.iOS.Common,
  DPF.iOS.UILabel,
  DPF.iOS.UITextField;

type
  TFMapView = class( TForm )
    DPFMapView1: TDPFMapView;
    DPFButton1: TDPFButton;
    DPFUIView1: TDPFUIView;
    DPFTextFieldFrom: TDPFTextField;
    DPFTextFieldTo: TDPFTextField;
    DPFLabel1: TDPFLabel;
    DPFLabel2: TDPFLabel;
    procedure DPFButton1Click( Sender: TObject );

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

procedure TFMapView.DPFButton1Click( Sender: TObject );
begin
  DPFMapView1.ShowRoute( DPFTextFieldFrom.Text, DPFTextFieldTo.Text );
  HideKeyBoard;
end;

procedure TFMapView.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.

  http: // maps.googleapis.com/maps/api/directions/json?origin=Tehran,Vanak&destination=Karaj&sensor=false&dirflg=w&mode=driving
