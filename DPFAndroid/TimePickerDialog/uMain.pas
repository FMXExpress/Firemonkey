unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Helpers.Android,

  DPF.Android.BaseControl,
  DPF.Android.JView,
  DPF.Android.JButton,
  DPF.Android.JTimePickerDialog,
  DPF.Android.JTextView,
  DPF.Android.JRelativeLayout;

type
  TFTimePickerDialog = class( TForm )
    DPFJView1: TDPFJRelativeLayout;
    DPFJButton1: TDPFJButton;
    DPFJTimePickerDialog1: TDPFJTimePickerDialog;
    DPFJTextView1: TDPFJTextView;
    procedure DPFJButton1Click( Sender: TObject );
    procedure DPFJTimePickerDialog1TimeSet( sender: TObject; const HourOfDay, Minute: Integer );
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FTimePickerDialog: TFTimePickerDialog;

implementation

{$R *.fmx}

procedure TFTimePickerDialog.DPFJButton1Click( Sender: TObject );
begin
  DPFJTimePickerDialog1.Show( 'Time Picker', 'Please select a time', 10, 12, true );
end;

procedure TFTimePickerDialog.DPFJTimePickerDialog1TimeSet( sender: TObject; const HourOfDay, Minute: Integer );
begin
  DPFJTextView1.Text.Text := HourOfDay.ToString + ':' + Minute.ToString;
end;

end.
