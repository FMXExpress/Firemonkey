unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Helpers.Android,

  DPF.Android.BaseControl,
  DPF.Android.JView,
  DPF.Android.JButton,
  DPF.Android.JToast, DPF.Android.JRelativeLayout;

type
  TFToast = class( TForm )
    DPFJView1: TDPFJRelativeLayout;
    DPFJButton1: TDPFJButton;
    DPFJToast1: TDPFJToast;
    DPFJButton2: TDPFJButton;
    procedure DPFJButton1Click( Sender: TObject );
    procedure DPFJButton2Click( Sender: TObject );
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FToast: TFToast;

implementation

{$R *.fmx}

procedure TFToast.DPFJButton1Click( Sender: TObject );
begin
  DPFJToast1.Show( 'Hello!', 5000, TDPFTextAlignment.taCenter_Vertical );
end;

procedure TFToast.DPFJButton2Click( Sender: TObject );
begin
  DPFJToast1.Show( 'Hello!' + #10#13 + 'This is test message', 5000, TDPFTextAlignment.taBottom );
end;

end.
