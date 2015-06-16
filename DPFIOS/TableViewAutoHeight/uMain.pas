unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, DPF.iOS.BaseControl,
  DPF.iOS.UITableView, DPF.iOS.UITableViewItems, DPF.iOS.UILabel, DPF.iOS.UIFont,
  iOSapi.Foundation, DPF.iOS.UIView;

type
  TFTableView = class( TForm )
    DPFUITableView1: TDPFUITableView;
    procedure DPFUITableView1GetRowHeight( Sender: TObject; Section, RowNo: Integer; var RowHeight: Single );
    procedure DPFUITableView1DrawCell( Sender: TObject; Section, RowNo: Integer; TableItem: TTableItem; var Objects: TArray<DPF.iOS.BaseControl.TDPFiOSBaseControl>; var Handled: Boolean );
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

procedure TFTableView.DPFUITableView1DrawCell( Sender: TObject; Section, RowNo: Integer; TableItem: TTableItem; var Objects: TArray<DPF.iOS.BaseControl.TDPFiOSBaseControl>; var Handled: Boolean );
begin
  Handled := true;
  if length( Objects ) = 0 then
  begin
    SetLength( Objects, 1 );
    TDPFLabel( Objects[0] )               := TDPFLabel.Create( nil );
    TDPFLabel( Objects[0] ).Font.FontName := ios_AppleGothic;
    TDPFLabel( Objects[0] ).Font.FontSize := 14;
    TDPFLabel( Objects[0] ).TextColor     := TAlphaColors.Black;
    TDPFLabel( Objects[0] ).Position.X    := 0;
    TDPFLabel( Objects[0] ).Position.Y    := 0;
    TDPFLabel( Objects[0] ).NumberOfLines := 0;
    TDPFLabel( Objects[0] ).LineBreak     := lbWordWrap;
    TDPFLabel( Objects[0] ).AutoSizeToFit := true;;
  end;
  TDPFLabel( Objects[0] ).Width  := DPFUITableView1.Width;
  TDPFLabel( Objects[0] ).Height := 1000;
  TDPFLabel( Objects[0] ).Text   := TableItem.ItemText.Text;
end;

procedure TFTableView.DPFUITableView1GetRowHeight( Sender: TObject; Section, RowNo: Integer; var RowHeight: Single );
var
  DPFLabel: TDPFLabel;
  // DPFUIView: TDPFUIView;
  cDPFNSize: DPFNSize;
begin
  // DPFUIView        := TDPFUIView.Create( nil );
  // DPFUIView.Width  := DPFUITableView1.Width;
  // DPFUIView.Height := 1000;
  // DPFUIView.Loaded;

  DPFLabel := TDPFLabel.Create( nil );
  // DPFLabel.Parent        := DPFUIView;
  DPFLabel.Font.FontName := ios_AppleGothic;
  DPFLabel.Font.FontSize := 14;
  DPFLabel.LineBreak     := lbWordWrap;
  DPFLabel.NumberOfLines := 0;
  DPFLabel.Text          := DPFUITableView1.Row[Section, RowNo].ItemText.Text;
  DPFLabel.Width         := DPFUITableView1.Width;
  DPFLabel.Loaded;
  cDPFNSize := DPFLabel.sizeToFit;
  RowHeight := cDPFNSize.Height;

  DPFLabel.DisposeOf;
  // DPFUIView.DisposeOf;
end;

procedure TFTableView.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
