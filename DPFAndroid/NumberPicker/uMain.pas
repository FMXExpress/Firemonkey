unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  DPF.Android.BaseControl,
  DPF.Android.JView,
  DPF.Android.JEditText,
  FMX.Platform.Android,
  DPF.Android.JTextView,
  DPF.Android.JNumberPicker, DPF.Android.JRelativeLayout;

type
  TFNumberPicker = class( TForm )
    DPFJView1: TDPFJRelativeLayout;
    DPFJNumberPicker1: TDPFJNumberPicker;
    DPFJTextView1: TDPFJTextView;
    procedure DPFJNumberPicker1ValueChange( sender: TObject; const oldVal, newVal: Integer );
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FNumberPicker: TFNumberPicker;

implementation

{$R *.fmx}

procedure TFNumberPicker.DPFJNumberPicker1ValueChange( sender: TObject; const oldVal, newVal: Integer );
begin
  DPFJTextView1.Text.Text := IntToStr( OldVal ) + ' to ' + IntToStr( newVal );
end;

end.
