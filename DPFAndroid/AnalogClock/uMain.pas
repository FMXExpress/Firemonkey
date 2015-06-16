unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  DPF.Android.BaseControl,
  DPF.Android.JView,
  DPF.Android.JAnalogClock, DPF.Android.JRelativeLayout;

type
  TFAnalogClock = class( TForm )
    DPFJView1: TDPFJRelativeLayout;
    DPFJAnalogClock1: TDPFJAnalogClock;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FAnalogClock: TFAnalogClock;

implementation

{$R *.fmx}

end.
