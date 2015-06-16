unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  DPF.Android.BaseControl,
  DPF.Android.JView,
  DPF.Android.JAnalogClock,
  DPF.Android.JDatePicker,
  DPF.Android.JTextView, DPF.Android.JRelativeLayout;

type
  TFDatePicker = class( TForm )
    DPFJView1: TDPFJRelativeLayout;
    DPFJDatePicker1: TDPFJDatePicker;
    DPFJTextView1: TDPFJTextView;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FDatePicker: TFDatePicker;

implementation

{$R *.fmx}

end.
