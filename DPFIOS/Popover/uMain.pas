unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  DPF.iOS.BaseControl, DPF.iOS.UIView, DPF.iOS.UILabel,
  DPF.iOS.UIViewController, DPF.iOS.UIPopoverController, DPF.iOS.UIButton,
  DPF.iOS.UIDatePicker, DPF.iOS.UINavigationController, DPF.iOS.UITableView,
  DPF.iOS.UITableViewItems;

type
  TFPopover = class( TForm )
    DPFUIView1: TDPFUIView;
    DPFPopover1: TDPFPopover;
    DPFButton1: TDPFButton;
    DPFDatePicker1: TDPFDatePicker;
    DPFButton2: TDPFButton;
    DPFLabel1: TDPFLabel;
    DPFPopover2: TDPFPopover;
    DPFNavigationController1: TDPFNavigationController;
    DPFNavigationControllerPage1: TDPFNavigationControllerPage;
    DPFUITableView1: TDPFUITableView;
    procedure DPFButton1Click( Sender: TObject );
    procedure DPFButton2Click( Sender: TObject );
    procedure DPFDatePicker1Change( Sender: TObject );
    procedure DPFNavigationControllerPage1BarButtons2Click( Sender: TObject );
    procedure DPFUITableView1ItemSelect( Sender: TObject; Section, RowNo: Integer; var CellAccessory: TTableViewCellAccessory );
    procedure DPFUITableView1ItemDeSelect( Sender: TObject; Section, RowNo: Integer; var CellAccessory: TTableViewCellAccessory );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FPopover: TFPopover;

implementation

{$R *.fmx}

uses uFrame;

procedure TFPopover.DPFButton1Click( Sender: TObject );
begin
  DPFPopover1.ShowPopup( DPFButton1, TDPFPopoverArrowDirection.padUp );
end;

procedure TFPopover.DPFButton2Click( Sender: TObject );
begin
  DPFPopover2.Frame := TFrame1;
  DPFPopover2.ShowPopup( DPFButton2, TDPFPopoverArrowDirection.padRight );
end;

procedure TFPopover.DPFDatePicker1Change( Sender: TObject );
begin

  DPFLabel1.Text := DateTimeToStr( DPFDatePicker1.PickerDate );
end;

procedure TFPopover.DPFNavigationControllerPage1BarButtons2Click( Sender: TObject );
begin
  DPFPopover2.ShowPopup( DPFNavigationControllerPage1.BarButtons[2].GetUIBarButtonItem, TDPFPopoverArrowDirection.padUp );
end;

procedure TFPopover.DPFUITableView1ItemDeSelect( Sender: TObject; Section, RowNo: Integer; var CellAccessory: TTableViewCellAccessory );
begin
  CellAccessory := TTableViewCellAccessory.tvcaNone;
end;

procedure TFPopover.DPFUITableView1ItemSelect( Sender: TObject; Section, RowNo: Integer; var CellAccessory: TTableViewCellAccessory );
begin
  CellAccessory := TTableViewCellAccessory.tvcaCheckmark;
end;

procedure TFPopover.PaintRects( const UpdateRects: array of TRectF );
begin
  { }

end;

end.
