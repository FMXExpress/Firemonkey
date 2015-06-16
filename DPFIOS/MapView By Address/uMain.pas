unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, DPF.iOS.MapKit,
  DPF.iOS.BaseControl, DPF.iOS.MKMapView, DPF.iOS.UIButton,
  DPF.iOS.UIView,
  DPF.iOS.Common,
  DPF.iOS.UILabel, DPF.iOS.UITextField;

type
  TFMapView = class( TForm )
    DPFMapView1: TDPFMapView;
    DPFButton1: TDPFButton;
    DPFUIView1: TDPFUIView;
    DPFTextField1: TDPFTextField;
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
  Marker := DPFMapView1.AddAnnotation( DPFTextField1.Text, '', DPFTextField1.Text, 14 );
  HideKeyBoard;
end;

procedure TFMapView.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
