unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, DPF.iOS.BaseControl,
  DPF.iOS.UIButton, DPF.iOS.UIView;

type
  TFButtons = class( TForm )
    DPFButton1: TDPFButton;
    DPFButton2: TDPFButton;
    DPFButton3: TDPFButton;
    DPFButton4: TDPFButton;
    DPFButton5: TDPFButton;
    DPFButton6: TDPFButton;
    DPFUIView1: TDPFUIView;
    DPFButton7: TDPFButton;
    procedure DPFButton1Click( Sender: TObject );
    procedure DPFButton6DoubleClick( Sender: TObject );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FButtons: TFButtons;

implementation

{$R *.fmx}
{ TForm3 }

procedure TFButtons.DPFButton1Click( Sender: TObject );
begin
  if DPFButton1.Tag = 0 then
  begin
    DPFButton1.SetAnimationTransition( DPFButton1.Position.X, DPFButton1.Position.X + 60, DPFButton1.Position.Y, DPFButton1.Position.Y, DPFButton1.Width, DPFButton1.Width, DPFButton1.Height, DPFButton1.Height, 0.5 );
    DPFButton1.Tag := 1;
  end
  else
  begin
    DPFButton1.SetAnimationTransition( DPFButton1.Position.X, DPFButton1.Position.X - 60, DPFButton1.Position.Y, DPFButton1.Position.Y, DPFButton1.Width, DPFButton1.Width, DPFButton1.Height, DPFButton1.Height, 0.5 );
    DPFButton1.Tag := 0;
  end;
end;

procedure TFButtons.DPFButton6DoubleClick( Sender: TObject );
begin
  ShowMessage( 'Double Clicked!' );
end;

procedure TFButtons.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
