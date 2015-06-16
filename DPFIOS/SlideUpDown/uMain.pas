unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  DPF.iOS.BaseControl,
  DPF.iOS.UILabel, DPF.iOS.UIView, DPF.iOS.UIButton, DPF.iOS.UIToolbar,
  uFrame1, uFrame2;

type
  TFSlideUpDown = class( TForm )
    DPFUIViewBack: TDPFUIView;
    DPFUIView1: TDPFUIView;
    DPFUIView2: TDPFUIView;
    DPFToolbar1: TDPFToolbar;
    procedure FormShow( Sender: TObject );
    procedure DPFToolbar1BarItems0Click( Sender: TObject );
    procedure DPFToolbar1BarItems2Click( Sender: TObject );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FSlideUpDown: TFSlideUpDown;

implementation

{$R *.fmx}

procedure TFSlideUpDown.DPFToolbar1BarItems0Click( Sender: TObject );
begin
  if DPFUIView2.Tag = 0 then
  begin
    DPFUIView2.Tag        := 1;
    DPFUIView2.Position.Y := Height + 1;
    DPFUIView2.BringToFront;
    DPFUIView2.SetAnimationTransition( 0, 0, Height, 0, Width, Width, DPFUIView1.Height, DPFUIView1.Height );
  end
  else
  begin
    DPFUIView2.Tag        := 0;
    DPFUIView1.Position.Y := Height + 1;
    DPFUIView1.BringToFront;
    DPFUIView1.SetAnimationTransition( 0, 0, Height, 0, Width, Width, DPFUIView1.Height, DPFUIView1.Height );
  end

end;

procedure TFSlideUpDown.DPFToolbar1BarItems2Click( Sender: TObject );
begin
  if DPFUIView2.Tag = 0 then
  begin
    DPFUIView2.Tag        := 1;
    DPFUIView2.Position.Y := -Height;
    DPFUIView2.BringToFront;
    DPFUIView2.SetAnimationTransition( 0, 0, -Height, 0, Width, Width, DPFUIView1.Height, DPFUIView1.Height );
  end
  else
  begin
    DPFUIView2.Tag        := 0;
    DPFUIView1.Position.Y := -Height;
    DPFUIView1.BringToFront;
    DPFUIView1.SetAnimationTransition( 0, 0, -Height, 0, Width, Width, DPFUIView1.Height, DPFUIView1.Height );
  end
end;

procedure TFSlideUpDown.FormShow( Sender: TObject );
begin
  DPFUIView1.Frame := uFrame1.TFrame1;
  DPFUIView2.Frame := uFrame2.TFrame2;

  DPFUIView1.Align := TAlignLayout.Client;
  DPFUIView2.Align := TAlignLayout.Client;
  DPFUIView1.BringToFront;
end;

procedure TFSlideUpDown.PaintRects( const UpdateRects: array of TRectF );
begin
  { }

end;

end.
