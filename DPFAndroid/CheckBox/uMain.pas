unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  DPF.Android.BaseControl,

  DPF.Android.JView,
  DPF.Android.JCheckBox,
  DPF.Android.JButton,
  DPF.Android.JTextView, DPF.Android.JRelativeLayout;

type
  TFCheckBox = class( TForm )
    DPFJView1: TDPFJRelativeLayout;
    DPFJCheckBox1: TDPFJCheckBox;
    DPFJTextView1: TDPFJTextView;
    DPFJButton1: TDPFJButton;
    procedure DPFJCheckBox1CheckedChanged( sender: TObject; isChecked: Boolean );
    procedure DPFJButton1Click( Sender: TObject );
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FCheckBox: TFCheckBox;

implementation

{$R *.fmx}

procedure TFCheckBox.DPFJButton1Click( Sender: TObject );
begin
  DPFJCheckBox1.Toggle;
end;

procedure TFCheckBox.DPFJCheckBox1CheckedChanged( sender: TObject; isChecked: Boolean );
begin
  if isChecked then
    DPFJTextView1.Text.Text := 'Checked'
  else
    DPFJTextView1.Text.Text := 'UnChecked';

end;

end.
