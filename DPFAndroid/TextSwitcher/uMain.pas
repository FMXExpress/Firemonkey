unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,

  DPF.Android.BaseControl,
  DPF.Android.JView,
  DPF.Android.JTextSwitcher,
  DPF.Android.JTextView,
  DPF.Android.JButton, DPF.Android.JRelativeLayout;

type
  TFTextSwitcher = class( TForm )
    DPFJView1: TDPFJRelativeLayout;
    DPFJTextSwitcher1: TDPFJTextSwitcher;
    DPFJButton1: TDPFJButton;
    DPFJButton2: TDPFJButton;
    DPFJButton3: TDPFJButton;
    procedure DPFJButton1Click( Sender: TObject );
    procedure DPFJButton2Click( Sender: TObject );
    procedure DPFJButton3Click( Sender: TObject );
  private
    { Private declarations }
  protected
  public
    { Public declarations }
  end;

var
  FTextSwitcher: TFTextSwitcher;

implementation

{$R *.fmx}

procedure TFTextSwitcher.DPFJButton1Click( Sender: TObject );
begin
  DPFJTextSwitcher1.setText( 'D.P.F Android Native Components ', 21, TAlphaColors.Gray, taCenter, 1000 );
end;

procedure TFTextSwitcher.DPFJButton2Click( Sender: TObject );
begin
  DPFJTextSwitcher1.setText( 'Develop Native Android applications with Delphi XE5', 21, TAlphaColors.Gray, taCenter, 1000 );
end;

procedure TFTextSwitcher.DPFJButton3Click( Sender: TObject );
begin
  DPFJTextSwitcher1.setText( 'Enjoy For Performance!', 32, TAlphaColors.Gray, taCenter, 1000 );
end;

end.
