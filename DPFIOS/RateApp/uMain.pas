unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,

  DPF.iOS.Common,
  DPF.iOS.BaseControl,
  DPF.iOS.UIButton,
  DPF.iOS.UILabel,
  DPF.iOS.UIView,
  DPF.iOS.RateApp;

type
  TFRateApp = class( TForm )
    DPFLabel1: TDPFLabel;
    DPFUIView1: TDPFUIView;
    DPFRateApp1: TDPFRateApp;
    DPFButton1: TDPFButton;
    procedure DPFButton1Click( Sender: TObject );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FRateApp: TFRateApp;

implementation

{$R *.fmx}

procedure TFRateApp.DPFButton1Click( Sender: TObject );
var
  r: TDPFRateAppState;
begin
  r := DPFRateApp1.CheckRateApp( true );
  if r = raRated then
    DPFLabel1.Text := 'Rated'
  else if r = raNoThanks then
    DPFLabel1.Text := 'No, Thanks'
  else if r = raRemindMeLater then
    DPFLabel1.Text := 'Remind Me Later'
  else
    DPFLabel1.Text := 'Unknown';

end;

procedure TFRateApp.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
