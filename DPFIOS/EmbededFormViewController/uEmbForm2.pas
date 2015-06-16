unit uEmbForm2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, DPF.iOS.UISwitch,
  DPF.iOS.BaseControl, DPF.iOS.UILabel, DPF.iOS.UISegmentedControl,
  DPF.iOS.UIButton, DPF.iOS.UIView;

type
  TFEmbForm2 = class(TForm)
    DPFLabel1: TDPFLabel;
    DPFSegmentedControl1: TDPFSegmentedControl;
    DPFButton1: TDPFButton;
    Form2MainView: TDPFUIView;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FEmbForm2: TFEmbForm2;

implementation

{$R *.fmx}

end.
