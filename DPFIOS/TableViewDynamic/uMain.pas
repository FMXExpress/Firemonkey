unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, DPF.iOS.BaseControl,
  iOSapi.UIKit,
  DPF.iOS.Common,
  DPF.iOS.UITableView,
  DPF.iOS.UITableViewItems,
  DPF.iOS.UIView,
  DPF.iOS.UIButton,
  DPF.iOS.UIViewController,
  DPF.iOS.UITabBarController,
  DPF.iOS.UILabel,
  uSecForm;

type
  TFTableViewDyn = class( TForm )
    DPFUIView1: TDPFUIView;
    DPFTabBarController1: TDPFTabBarController;
    DPFTabBarItem1: TDPFTabBarItem;
    DPFTabBarItem2: TDPFTabBarItem;
    DPFUITableViewMain: TDPFUITableView;
    DPFUIView2: TDPFUIView;
    DPFButton1: TDPFButton;
    DPFButton2: TDPFButton;
    DPFButton3: TDPFButton;
    procedure FormCreate( Sender: TObject );
    procedure DPFButton1Click( Sender: TObject );
    procedure DPFButton2Click( Sender: TObject );
    procedure DPFButton3Click( Sender: TObject );
    procedure DPFUITableViewMainItemSelect( Sender: TObject; Section, RowNo: Integer; var CellAccessory: TTableViewCellAccessory );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FTableViewDyn: TFTableViewDyn;

implementation

{$R *.fmx}
{ TFTableView1 }

procedure TFTableViewDyn.DPFButton1Click( Sender: TObject );
begin
  FSecForm.CreateTable( DPFUITableViewMain );
end;

procedure TFTableViewDyn.DPFButton2Click( Sender: TObject );
begin
  DPFUITableViewMain.Options.SelectedColor := TAlphaColors.Orange;
  DPFUITableViewMain.SelectRow( 0, 38, true, TDPFTableViewScrollPosition.tvspMiddle );
end;

procedure TFTableViewDyn.DPFButton3Click( Sender: TObject );
begin
  DPFUITableViewMain.DeSelectRow( 0, 38 );
end;

procedure TFTableViewDyn.DPFUITableViewMainItemSelect( Sender: TObject; Section, RowNo: Integer; var CellAccessory: TTableViewCellAccessory );
begin
  { }
end;

procedure TFTableViewDyn.FormCreate( Sender: TObject );
begin
  FSecForm.CreateTable( DPFUITableViewMain );
end;

procedure TFTableViewDyn.PaintRects( const UpdateRects: array of TRectF );
begin
  { }

end;

end.
