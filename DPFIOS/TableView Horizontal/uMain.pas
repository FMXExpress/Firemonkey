unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, DPF.iOS.BaseControl,
  DPF.iOS.Common, DPF.iOS.UITableView, DPF.iOS.UIButton, DPF.iOS.UILabel, DPF.iOS.UITableViewItems,
  DPF.iOS.UIToolbar, DPF.iOS.UIView, DPF.iOS.UISwitch;

type
  TFTableView1 = class( TForm )
    DPFUITableView1: TDPFUITableView;
    DPFUIView1: TDPFUIView;
    DPFLabel1: TDPFLabel;
    procedure DPFUITableView1GetRowHeight( Sender: TObject; Section, RowNo: Integer; var RowHeight: single );
    procedure DPFUITableView1PageChanged( Sender: TObject; PageNO: Integer );
    procedure FormShow( Sender: TObject );
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

procedure TFTableView1.DPFUITableView1GetRowHeight( Sender: TObject; Section, RowNo: Integer; var RowHeight: single );
begin
  RowHeight := DPFUITableView1.Width;
end;

procedure TFTableView1.DPFUITableView1PageChanged( Sender: TObject; PageNO: Integer );
begin
  DPFLabel1.Text := 'Page ' + PageNo.ToString;
end;

procedure TFTableView1.FormShow( Sender: TObject );
begin
  DPFUITableView1PageChanged( DPFUITableView1, DPFUITableView1.PageNo );
end;

procedure TFTableView1.PaintRects( const UpdateRects: array of TRectF );
begin
  { }

end;

end.
