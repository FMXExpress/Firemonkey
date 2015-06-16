unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,

  DPF.Android.BaseControl,
  DPF.Android.JView,
  DPF.Android.JAnimation,
  DPF.Android.JRelativeLayout,
  DPF.Android.JButton, DPF.Android.JAnalogClock;

type
  TFSlideAnimation = class( TForm )
    DPFJView1: TDPFJRelativeLayout;
    DPFJView2: TDPFJRelativeLayout;
    DPFJAnimation1: TDPFJAnimation;
    DPFJAnimation2: TDPFJAnimation;
    DPFJButton3: TDPFJButton;
    DPFJRelativeLayout1: TDPFJRelativeLayout;
    DPFJButton1: TDPFJButton;
    DPFJAnalogClock1: TDPFJAnalogClock;
    procedure DPFJButton1Click( Sender: TObject );
    procedure DPFJTranslateAnimation1AnimationEnd( sender: TObject );
    procedure DPFJAnimation2AnimationEnd( sender: TObject );
    procedure DPFJButton3Click( Sender: TObject );
  private
    { Private declarations }
  protected
    curPos: Single;
  public
    { Public declarations }
  end;

var
  FSlideAnimation: TFSlideAnimation;

implementation

{$R *.fmx}

procedure TFSlideAnimation.DPFJButton3Click( Sender: TObject );
begin
  if DPFJButton3.Tag = 1 then
    DPFJButton3.Tag := -1
  else
    DPFJButton3.Tag := 1;
  DPFJAnimation2.StartTranslateAnimation( DPFJButton3, DPFJButton3.Position.x, DPFJButton3.Position.x + DPFJButton3.Tag * 100, DPFJButton3.Position.y, DPFJButton3.Position.y, 1.0, 1.0, 1000 );
end;

procedure TFSlideAnimation.DPFJAnimation2AnimationEnd( sender: TObject );
begin
  DPFJButton3.Position.x := DPFJButton3.Position.x + DPFJButton3.Tag * 100;
end;

procedure TFSlideAnimation.DPFJButton1Click( Sender: TObject );
begin
  if DPFJView2.Tag = 1 then
  begin
    DPFJView2.Tag := -1;
    curPos        := 0;
  end
  else
  begin
    DPFJView2.Tag := 1;
    curPos        := DPFJView1.Width - 50;
  end;

  DPFJAnimation1.StartTranslateAnimation( DPFJView2, DPFJView2.Position.x, curPos, DPFJView2.Position.y, DPFJView2.Position.y, 1.0, 1.0, 400 );
end;

procedure TFSlideAnimation.DPFJTranslateAnimation1AnimationEnd( sender: TObject );
begin
  DPFJView2.Position.x := curPos;
end;

end.
