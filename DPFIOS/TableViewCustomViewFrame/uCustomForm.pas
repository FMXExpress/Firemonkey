unit uCustomForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, DPF.iOS.UILabel,
  DPF.iOS.UIImageView, DPF.iOS.BaseControl, DPF.iOS.UIView;

type
  TForm1 = class(TForm)
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

var
  Form1: TForm1;

implementation

{$R *.fmx}

end.
