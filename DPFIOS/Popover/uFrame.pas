unit uFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, DPF.iOS.BaseControl, DPF.iOS.UITableView;

type
  TFrame1 = class(TFrame)
    DPFUITableView1: TDPFUITableView;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

end.
