unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,

  DPF.Android.BaseControl,
  DPF.Android.JView,
  DPF.Android.JTextView,
  DPF.Android.JRelativeLayout;

type
  TFLabels = class( TForm )
    DPFJView1: TDPFJRelativeLayout;
    DPFANDLabel1: TDPFJTextView;
    DPFANDLabel2: TDPFJTextView;
    DPFJView2: TDPFJRelativeLayout;
    DPFJView3: TDPFJRelativeLayout;
    DPFANDLabel5: TDPFJTextView;
    DPFJTextView1: TDPFJTextView;
    DPFANDLabel4: TDPFJTextView;
    DPFJTextView2: TDPFJTextView;
    DPFJTextView3: TDPFJTextView;
    DPFJTextView4: TDPFJTextView;
    DPFJTextView5: TDPFJTextView;
  private
    { Private declarations }
  protected
  public
    { Public declarations }
  end;

var
  FLabels: TFLabels;

implementation

{$R *.fmx}

end.
