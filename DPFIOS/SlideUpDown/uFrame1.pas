unit uFrame1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, DPF.iOS.BaseControl, DPF.iOS.UISegmentedControl,
  DPF.iOS.UIWebView, DPF.iOS.UISearchBar, DPF.iOS.UITableView;

type
  TFrame1 = class(TFrame)
    DPFSearchBar1: TDPFSearchBar;
    DPFUITableView1: TDPFUITableView;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

end.
