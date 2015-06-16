unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Helpers.Android,

  DPF.Android.BaseControl,
  DPF.Android.JView,
  DPF.Android.JProgressBar, DPF.Android.JRelativeLayout;

type
  TFProgressBar = class( TForm )
    DPFJView1: TDPFJRelativeLayout;
    DPFJProgressBar1: TDPFJProgressBar;
    DPFJProgressBar2: TDPFJProgressBar;
    DPFJProgressBar3: TDPFJProgressBar;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FProgressBar: TFProgressBar;

implementation

{$R *.fmx}

end.
