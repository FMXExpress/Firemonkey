unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  DPF.Android.BaseControl,
  DPF.Android.JTextView,
  DPF.Android.JView,
  DPF.Android.JRadioGroup,
  DPF.Android.JButton, DPF.Android.JRelativeLayout;

type
  TFRadioGroup = class( TForm )
    DPFJView1: TDPFJRelativeLayout;
    DPFJTextView1: TDPFJTextView;
    DPFJRadioGroup1: TDPFJRadioGroup;
    DPFJButton1: TDPFJButton;
    DPFJButton2: TDPFJButton;
    procedure DPFJRadioGroup1CheckedChanged( sender: TObject; CheckedID: Integer );
    procedure DPFJButton1Click( Sender: TObject );
    procedure DPFJButton2Click( Sender: TObject );
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FRadioGroup: TFRadioGroup;

implementation

{$R *.fmx}

procedure TFRadioGroup.DPFJButton1Click( Sender: TObject );
begin
  DPFJRadioGroup1.ClearCheck;
end;

procedure TFRadioGroup.DPFJButton2Click( Sender: TObject );
begin
  DPFJRadioGroup1.CheckRadioButton( 3 );
end;

procedure TFRadioGroup.DPFJRadioGroup1CheckedChanged( sender: TObject; CheckedID: Integer );
begin
  if CheckedID = -1 then
    DPFJTextView1.Text.Text := 'Cleared!'
  else
    DPFJTextView1.Text.Text := 'ID: ' + IntToStr( CheckedID ) + ' Checked';
end;

end.
