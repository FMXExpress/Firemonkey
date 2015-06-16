unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,

  DPF.iOS.BaseControl,
  DPF.iOS.Common,
  DPF.iOS.UITableView,
  DPF.iOS.UIButton,
  DPF.iOS.UILabel,
  DPF.iOS.UITableViewItems,
  DPF.iOS.UIToolbar,
  DPF.iOS.UIView,
  DPF.iOS.UISwitch;

type
  TFTableView1 = class( TForm )
    DPFUITableView1: TDPFUITableView;
    DPFUIView1: TDPFUIView;
    DPFToolbar1: TDPFToolbar;
    DPFSwitch1: TDPFSwitch;
    DPFLabel1: TDPFLabel;
    procedure DPFToolbar1BarItems0Click( Sender: TObject );
    procedure DPFUITableView1AccessoryButtonTapped( Sender: TObject; Section, RowNo: Integer );
    procedure DPFUITableView1ItemSelect( Sender: TObject; Section, RowNo: Integer; var CellAccessory: TTableViewCellAccessory );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FTableView1: TFTableView1;

implementation

{$R *.fmx}
{ TFTableView1 }

procedure TFTableView1.DPFToolbar1BarItems0Click( Sender: TObject );
begin
  DPFUITableView1.Options.Edition.Moving  := DPFSwitch1.ISON;
  DPFUITableView1.Options.Edition.EditAll := not DPFUITableView1.Options.Edition.EditAll;
  if DPFToolbar1.BarItems.Items[0].ButtonSystemItem = bbsiDone then
    DPFToolbar1.BarItems.Items[0].ButtonSystemItem := bbsiEdit
  else
    DPFToolbar1.BarItems.Items[0].ButtonSystemItem := bbsiDone;
end;

procedure TFTableView1.DPFUITableView1AccessoryButtonTapped( Sender: TObject; Section, RowNo: Integer );
begin
  ShowMessage( 'Accessory Button Tapped. Section: ' + Section.ToString + ', Row: ' + RowNo.ToString );
end;

procedure TFTableView1.DPFUITableView1ItemSelect( Sender: TObject; Section, RowNo: Integer; var CellAccessory: TTableViewCellAccessory );
begin
  // DPFUITableView1.DeSelectRow( Section, RowNo, False );
end;

procedure TFTableView1.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
