unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, DPF.iOS.BaseControl,
  DPF.iOS.UITableView, DPF.iOS.UITableViewItems;

type
  TFTableView = class( TForm )
    DPFUITableView1: TDPFUITableView;
    procedure DPFUITableView1ItemDeSelect( Sender: TObject; Section, RowNo: Integer; var CellAccessory: TTableViewCellAccessory );
    procedure DPFUITableView1ItemSelect( Sender: TObject; Section, RowNo: Integer; var CellAccessory: TTableViewCellAccessory );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FTableView: TFTableView;

implementation

{$R *.fmx}
{ TFTableView }

procedure TFTableView.DPFUITableView1ItemDeSelect( Sender: TObject; Section, RowNo: Integer; var CellAccessory: TTableViewCellAccessory );
begin
  CellAccessory := TTableViewCellAccessory.tvcaNone;
end;

procedure TFTableView.DPFUITableView1ItemSelect( Sender: TObject; Section, RowNo: Integer; var CellAccessory: TTableViewCellAccessory );
begin
  CellAccessory := TTableViewCellAccessory.tvcaCheckmark;
end;

procedure TFTableView.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
