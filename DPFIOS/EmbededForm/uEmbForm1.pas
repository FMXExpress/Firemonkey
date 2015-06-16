unit uEmbForm1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, DPF.iOS.BaseControl,
  DPF.iOS.UILabel, DPF.iOS.UISwitch, DPF.iOS.UITableView, DPF.iOS.UIButton,
  DPF.iOS.UIView;

type
  TFEmbForm1 = class( TForm )
    DPFUITableView1: TDPFUITableView;
    DPFButton1InForm1: TDPFButton;
    Form1MainView: TDPFUIView;
  private
    { Private declarations }
  protected
  public
    { Public declarations }
  end;

var
  FEmbForm1: TFEmbForm1;

implementation

{$R *.fmx}
{ TFEmbForm1 }

end.
