unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  DPF.Android.JImageView,
  DPF.Android.BaseControl,
  DPF.Android.JView,
  DPF.Android.JTextView, DPF.Android.JRelativeLayout;

type
  TFImageView = class( TForm )
    DPFJView1: TDPFJRelativeLayout;
    DPFJImageView1: TDPFJImageView;
    DPFJImageView2: TDPFJImageView;
    DPFJTextView1: TDPFJTextView;
    DPFJTextView2: TDPFJTextView;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FImageView: TFImageView;

implementation

{$R *.fmx}

end.
