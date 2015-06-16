unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  DPF.Android.BaseControl,
  DPF.Android.JView, DPF.Android.JTextClock, DPF.Android.JButton,
  DPF.Android.JRelativeLayout;

type
  TFTextClock = class( TForm )
    DPFJView1: TDPFJRelativeLayout;
    DPFJTextClock1: TDPFJTextClock;
    DPFJTextClock2: TDPFJTextClock;
    DPFJTextClock3: TDPFJTextClock;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FTextClock: TFTextClock;

implementation

{$R *.fmx}

end.
