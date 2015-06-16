unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  DPF.Android.JDatePickerDialog,
  DPF.Android.JTextView,
  DPF.Android.JButton,
  DPF.Android.BaseControl,
  DPF.Android.JView, DPF.Android.JRelativeLayout;

type
  TFTimePickerDialog = class( TForm )
    DPFJView1: TDPFJRelativeLayout;
    DPFJButton1: TDPFJButton;
    DPFJTextView1: TDPFJTextView;
    DPFJDatePickerDialog1: TDPFJDatePickerDialog;
    procedure DPFJButton1Click( Sender: TObject );
    procedure DPFJDatePickerDialog1DateSet( sender: TObject; const year, monthOfYear, dayOfMonth: Integer );
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
  // -----------------------------------
  // month 0 ~ 11
  // -----------------------------------
  DPFJDatePickerDialog1.Show( 'Date Picker', 'Please select a date', 2013, 10, 9 );
end;

procedure TFTimePickerDialog.DPFJDatePickerDialog1DateSet( sender: TObject; const year, monthOfYear, dayOfMonth: Integer );
begin
  DPFJTextView1.Text.Clear;
  DPFJTextView1.Text.Add( IntToStr( year ) + '/' + IntToStr( monthOfYear ) + '/' + IntToStr( dayOfMonth ) )
end;

end.
