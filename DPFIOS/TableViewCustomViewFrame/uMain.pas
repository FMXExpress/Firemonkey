unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Math,
  System.DateUtils,
  System.IOUtils,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,

  DPF.iOS.BaseControl,
  DPF.iOS.Common,
  DPF.iOS.UIFont,
  DPF.iOS.UITableViewItems,
  DPF.iOS.UILabel,
  DPF.iOS.UITableView,
  DPF.iOS.UIImageView,
  DPF.iOS.UIView,
  DPF.iOS.UIButton,
  DPF.iOS.UISwitch;

type
  TFTableViewCVF = class( TForm )
    DPFUITableView1: TDPFUITableView;
    DPFUIView1: TDPFUIView;
    procedure FormCreate( Sender: TObject );

    procedure DPFUITableView1GetRowHeight( Sender: TObject; Section: Integer; RowNo: Integer; var RowHeight: Single );

    procedure DPFUITableView1ApplyFrameData( Sender: TObject; Section, RowNo: Integer; TableItem: TTableItem; var Frame: TFrame; var Handled: Boolean );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FTableViewCVF: TFTableViewCVF;

implementation

{$R *.fmx}

uses uFrame;

procedure TFTableViewCVF.DPFUITableView1ApplyFrameData( Sender: TObject; Section, RowNo: Integer; TableItem: TTableItem; var Frame: TFrame; var Handled: Boolean );
begin
  if Frame = nil then
  begin
    Frame        := TFrame1.Create( DPFUITableView1 );
    Frame.Name   := 'F' + RowNo.ToString;
    Frame.Parent := DPFUITableView1;
  end;

  TFrame1( Frame ).DPFImageView1.ImageList.Clear;
  TFrame1( Frame ).DPFImageView1.LoadAsync      := true;
  TFrame1( Frame ).DPFImageView1.ImageList.Text := '/Documents/admin.png';
  TFrame1( Frame ).DPFImageView1.ReloadImage;
  TFrame1( Frame ).DPFLabel1.Text := 'Left Label ' + RowNo.ToString;
  TFrame1( Frame ).DPFLabel2.Text := 'Right Label ' + RowNo.ToString;
  TFrame1( Frame ).DPFLabel3.Text := 'Description ' + RowNo.ToString;
  TFrame1( Frame ).DPFLabel4.Text := FormatDateTime( 'hh:mm', IncSecond( now, RandomRange( 1, 10000 ) ) );
end;

procedure TFTableViewCVF.DPFUITableView1GetRowHeight( Sender: TObject; Section: Integer; RowNo: Integer; var RowHeight: Single );
begin
  RowHeight := 80;
end;

procedure TFTableViewCVF.FormCreate( Sender: TObject );
var
  I: Integer;
begin
  DPFUITableView1.ClearAll;
  with DPFUITableView1.Sections.Add do
  begin
    for I := 0 to 100 do
      TableItems.Add;
  end;
  DPFUITableView1.RefreshNeeded;
end;

procedure TFTableViewCVF.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
