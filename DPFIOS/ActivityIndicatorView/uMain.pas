unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  DPF.iOS.BaseControl, DPF.iOS.UIView, DPF.iOS.UIActivityIndicatorView,
  DPF.iOS.UIButton, DPF.iOS.UILabel;

type
  TActivityIndicator = class( TForm )
    DPFUIView1: TDPFUIView;
    DPFActivityIndicatorView1: TDPFActivityIndicatorView;
    DPFButton1: TDPFButton;
    DPFButton2: TDPFButton;
    DPFButton3: TDPFButton;
    DPFLabel1: TDPFLabel;
    DPFButton4: TDPFButton;
    procedure DPFButton1Click( Sender: TObject );
    procedure DPFButton2Click( Sender: TObject );
    procedure DPFButton3Click( Sender: TObject );
    procedure DPFButton4Click( Sender: TObject );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  ActivityIndicator: TActivityIndicator;

implementation

{$R *.fmx}

procedure TActivityIndicator.DPFButton1Click( Sender: TObject );
begin
  DPFActivityIndicatorView1.startAnimating;
end;

procedure TActivityIndicator.DPFButton2Click( Sender: TObject );
begin
  DPFActivityIndicatorView1.StopAnimating( True );
end;

procedure TActivityIndicator.DPFButton3Click( Sender: TObject );
begin
  DPFActivityIndicatorView1.StopAnimating( false );
end;

procedure TActivityIndicator.DPFButton4Click( Sender: TObject );
begin
  DPFActivityIndicatorView1.startAnimating( 5 );
end;

procedure TActivityIndicator.PaintRects( const UpdateRects: array of TRectF );
begin
  { }

end;

end.
