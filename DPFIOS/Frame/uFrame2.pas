unit uFrame2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, DPF.iOS.BaseControl, DPF.iOS.UILabel, DPF.iOS.UITableView,
  DPF.iOS.UITextView, DPF.iOS.UIView, DPF.iOS.UIButton, DPF.iOS.UIImageView,
  DPF.iOS.UIScrollView, DPF.iOS.UIViewController, DPF.iOS.UITabBarController;

type
  TFrame2 = class( TFrame )
    DPFUITableView1: TDPFUITableView;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

end.
