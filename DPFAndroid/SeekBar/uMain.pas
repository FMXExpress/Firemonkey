unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Helpers.Android,

  DPF.Android.BaseControl,
  DPF.Android.JView,
  DPF.Android.JSeekBar,
  DPF.Android.JTextView, DPF.Android.JRelativeLayout;

type
  TFProgressBar = class( TForm )
    DPFJView1: TDPFJRelativeLayout;
    DPFJSeekBar1: TDPFJSeekBar;
    DPFJSeekBar2: TDPFJSeekBar;
    DPFJTextView1: TDPFJTextView;
    DPFJTextView2: TDPFJTextView;
    procedure DPFJSeekBar1ProgressChanged( Sender: TObject; progress: Integer; fromUser: Boolean );
    procedure DPFJSeekBar2ProgressChanged( Sender: TObject; progress: Integer; fromUser: Boolean );
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FProgressBar: TFProgressBar;

implementation

{$R *.fmx}

procedure TFProgressBar.DPFJSeekBar1ProgressChanged( Sender: TObject; progress: Integer; fromUser: Boolean );
begin
  DPFJTextView1.Text.Text := IntToStr( progress );
end;

procedure TFProgressBar.DPFJSeekBar2ProgressChanged( Sender: TObject; progress: Integer; fromUser: Boolean );
begin
  DPFJTextView2.Text.Text := IntToStr( progress );
end;

end.
