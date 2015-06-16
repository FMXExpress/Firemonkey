unit uFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, DPF.iOS.UILabel, DPF.iOS.UIImageView, DPF.iOS.BaseControl,
  DPF.iOS.UIView;

type
  TFrame1 = class(TFrame)
    DPFUIView3: TDPFUIView;
    DPFImageView1: TDPFImageView;
    DPFLabel1: TDPFLabel;
    DPFLabel2: TDPFLabel;
    DPFLabel3: TDPFLabel;
    DPFLabel4: TDPFLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

end.
