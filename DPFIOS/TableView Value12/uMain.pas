unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, DPF.iOS.BaseControl,
  DPF.iOS.UITableView;

type
  TFTableViewValue12 = class( TForm )
    DPFUITableView1: TDPFUITableView;
    procedure DPFUITableView1CellClick( Sender: TObject; SectionNo, RowNo: Integer; TapPosition: TTapPosition; State: TDPFGestureRecognizerState; var CancelsTouchesInView: Boolean );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FTableViewValue12: TFTableViewValue12;

implementation

{$R *.fmx}
{ TFTableView }

procedure TFTableViewValue12.DPFUITableView1CellClick( Sender: TObject; SectionNo, RowNo: Integer; TapPosition: TTapPosition; State: TDPFGestureRecognizerState; var CancelsTouchesInView: Boolean );
begin
  { }
end;

procedure TFTableViewValue12.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
