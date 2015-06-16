unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  DPF.Android.BaseControl,
  DPF.Android.JView,
  DPF.Android.JRadioButton,
  DPF.Android.JTextView,
  DPF.Android.JButton, DPF.Android.JRelativeLayout;

type
  TFButton = class( TForm )
    DPFJView1: TDPFJRelativeLayout;
    DPFJRadioButton1: TDPFJRadioButton;
    DPFJTextView1: TDPFJTextView;
    DPFJButton1: TDPFJButton;
    procedure DPFJRadioButton1CheckedChanged( sender: TObject; isChecked: Boolean );
    procedure DPFJButton1Click( Sender: TObject );
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FButton: TFButton;

implementation

{$R *.fmx}

procedure TFButton.DPFJButton1Click( Sender: TObject );
begin
  DPFJRadioButton1.Toggle;
end;

procedure TFButton.DPFJRadioButton1CheckedChanged( sender: TObject; isChecked: Boolean );
begin
  if isChecked then
    DPFJTextView1.Text.Text := 'Checked'
  else
    DPFJTextView1.Text.Text := 'UnChecked';

end;

end.
