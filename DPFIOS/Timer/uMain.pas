unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  DPF.iOS.BaseControl, DPF.iOS.UIButton, DPF.iOS.UILabel, DPF.iOS.UIView,
  DPF.iOS.NSTimer;

type
  TFTimer = class( TForm )
    DPFLabel2: TDPFLabel;
    DPFUIView1: TDPFUIView;
    DPFNSTimer1: TDPFNSTimer;
    DPFButton1: TDPFButton;
    procedure DPFButton1Click( Sender: TObject );
    procedure DPFNSTimer1Timer( Sender: TObject );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FTimer: TFTimer;

implementation

{$R *.fmx}

procedure TFTimer.DPFButton1Click( Sender: TObject );
begin
  DPFNSTimer1.Enabled := not DPFNSTimer1.Enabled;
end;

procedure TFTimer.DPFNSTimer1Timer( Sender: TObject );
begin
  DPFLabel2.Text := FormatDateTime( 'HH:mm:ss', now );
  DPFLabel2.SetAnimationTransition( vatFlipFromLeft, 0.5 );
end;

procedure TFTimer.PaintRects( const UpdateRects: array of TRectF );
begin

  { }

end;

end.
