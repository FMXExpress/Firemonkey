unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  System.Math,
  System.DateUtils,

  DPF.iOS.BaseControl,
  DPF.iOS.Common,
  DPF.iOS.UIFont,
  DPF.iOS.UITableView,
  DPF.iOS.UITableViewItems,
  DPF.iOS.UILabel,
  DPF.iOS.UIImageView,
  DPF.iOS.UIView,
  DPF.iOS.UIButton,
  DPF.iOS.UISwitch;

type
  TFTableViewCustomViewSection = class( TForm )
    DPFUITableView1: TDPFUITableView;
    DPFUIView1: TDPFUIView;
    procedure FormCreate( Sender: TObject );
    procedure DPFUITableView1DrawCell( Sender: TObject; Section, RowNo: Integer; TableItem: TTableItem; var Objects: TArray<DPF.iOS.BaseControl.TDPFiOSBaseControl>; var Handled: Boolean );

    procedure DPFUITableView1GetRowHeight( Sender: TObject; Section, RowNo: Integer; var RowHeight: Single );
    procedure DPFUITableView1ItemSelect( Sender: TObject; Section, RowNo: Integer; var CellAccessory: TTableViewCellAccessory );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FTableViewCustomViewSection: TFTableViewCustomViewSection;

implementation

{$R *.fmx}

// ------------------------------------------------------------------------------
// Notes:
// Dont create again objects, if length( Objects ) > 0
// ------------------------------------------------------------------------------
procedure TFTableViewCustomViewSection.DPFUITableView1DrawCell( Sender: TObject; Section, RowNo: Integer; TableItem: TTableItem; var Objects: TArray<DPF.iOS.BaseControl.TDPFiOSBaseControl>; var Handled: Boolean );
const
  icons: array [0 .. 24] of string = ( 'admin', 'businessman', 'businessmen', 'calendar', 'flag_iran', 'flower_blue', 'flower_red', 'flower_white', 'flower_yellow', 'lock', 'lock_open', 'loudspeaker', 'mail', 'news', 'shield_green', 'table', 'trafficlight_green', 'trafficlight_off', 'trafficlight_on', 'trafficlight_red', 'trafficlight_red_yellow',
    'trafficlight_yellow', 'user_headphones', 'users2', 'users4' );
var
  MustBeCreate: Boolean;
begin
  if Section > 0 then
  begin
    Handled := false;
    exit;
  end;
  Handled := true;

  MustBeCreate := length( Objects ) = 0;
  if MustBeCreate then
    SetLength( Objects, 5 );

  if MustBeCreate then
  begin
    TDPFImageView( Objects[0] )            := TDPFImageView.Create( nil );
    TDPFImageView( Objects[0] ).Position.X := 0;
    TDPFImageView( Objects[0] ).Position.X := 0;
    TDPFImageView( Objects[0] ).Width      := 64;
    TDPFImageView( Objects[0] ).Shadow     := false;
    TDPFImageView( Objects[0] ).Height     := 62;
    TDPFImageView( Objects[0] ).CacheImage := true;
  end;
  TDPFImageView( Objects[0] ).ClearImage;
  TDPFImageView( Objects[0] ).ImageList.text := '/Documents/' + icons[RandomRange( 0, 25 )] + '.png';
  TDPFImageView( Objects[0] ).ReloadImage;

  if MustBeCreate then
  begin
    TDPFLabel( Objects[1] )               := TDPFLabel.Create( nil );
    TDPFLabel( Objects[1] ).TextColor     := TAlphaColors.Steelblue;
    TDPFLabel( Objects[1] ).Font.FontName := ios_Verdana_Bold;
    TDPFLabel( Objects[1] ).Width         := 50;
    TDPFLabel( Objects[1] ).Height        := 15;
    TDPFLabel( Objects[1] ).Position.x    := DPFUITableView1.Width - TDPFImageView( Objects[1] ).Width - 1;
    TDPFLabel( Objects[1] ).Position.Y    := 0;
  end;
  TDPFLabel( Objects[1] ).Text := FormatDateTime( 'hh:mm', IncSecond( now, RandomRange( 1, 10000 ) ) );

  if MustBeCreate then
  begin
    TDPFSwitch( Objects[2] )               := TDPFSwitch.Create( nil );
    TDPFSwitch( Objects[2] ).Font.FontName := ios_Verdana_Bold;
    TDPFSwitch( Objects[2] ).Width         := 50;
    TDPFSwitch( Objects[2] ).Height        := 15;
    TDPFSwitch( Objects[2] ).Position.x    := DPFUITableView1.Width - TDPFImageView( Objects[1] ).Width - 15;
    TDPFSwitch( Objects[2] ).Position.Y    := 32;
  end;

  if MustBeCreate then
  begin
    TDPFLabel( Objects[3] )               := TDPFLabel.Create( nil );
    TDPFLabel( Objects[3] ).TextColor     := TAlphaColors.Black;
    TDPFLabel( Objects[3] ).Font.FontName := ios_Verdana_Bold;
    TDPFLabel( Objects[3] ).Width         := 200;
    TDPFLabel( Objects[3] ).Height        := 15;
    TDPFLabel( Objects[3] ).Position.x    := TDPFImageView( Objects[0] ).Position.X + TDPFImageView( Objects[0] ).Width + 1;
    TDPFLabel( Objects[3] ).Position.Y    := 10;
  end;
  TDPFLabel( Objects[3] ).Text := 'Custom Label ' + TableItem.ItemText.Text;

  if MustBeCreate then
  begin
    TDPFLabel( Objects[4] )               := TDPFLabel.Create( nil );
    TDPFLabel( Objects[4] ).TextColor     := TAlphaColors.Gray;
    TDPFLabel( Objects[4] ).Font.FontName := ios_Verdana_Bold;
    TDPFLabel( Objects[4] ).Width         := 200;
    TDPFLabel( Objects[4] ).Height        := 15;
    TDPFLabel( Objects[4] ).Position.x    := TDPFImageView( Objects[0] ).Position.X + TDPFImageView( Objects[0] ).Width + 1;
    TDPFLabel( Objects[4] ).Position.Y    := 40;
  end;
  TDPFLabel( Objects[4] ).Text := 'Custom Detail Label';
end;

procedure TFTableViewCustomViewSection.DPFUITableView1GetRowHeight( Sender: TObject; Section, RowNo: Integer; var RowHeight: Single );
begin
  RowHeight := 80;
end;

procedure TFTableViewCustomViewSection.DPFUITableView1ItemSelect( Sender: TObject; Section, RowNo: Integer; var CellAccessory: TTableViewCellAccessory );
var
  Objects: TArray<DPF.iOS.BaseControl.TDPFiOSBaseControl>;
begin
  Objects := DPFUITableView1.GetRowCustomViews( Section, RowNo );
  // TDPFImageView( Objects[0] )
  // TDPFLabel( Objects[1] )
  // TDPFSwitch( Objects[2] )
  // TDPFSwitch( Objects[2] )
  // TDPFLabel( Objects[3] )
  // TDPFLabel( Objects[4] )
end;

procedure TFTableViewCustomViewSection.FormCreate( Sender: TObject );
var
  I: Integer;
begin
  GetSharedApplication.setStatusBarHidden( true );
  DPFUITableView1.ClearAll( true, true );

  with DPFUITableView1.Sections.Add do
  begin
    Header.Text := 'Section 1';
    for I       := 0 to 2 do
    begin
      with TableItems.Add do
      begin
        BackgroundColor := TAlphaColors.white;
        ItemText.Text   := 'Item ' + IntToStr( i );
        ImageName       := '/Documents/admin.png';
        Style           := tvcsDefault;
      end;
    end;
  end;

  with DPFUITableView1.Sections.Add do
  begin
    Header.Kind            := TDPFTableHeaderFooterKind.kCustom;
    Header.BackgroundColor := TAlphaColors.Yellow;
    Header.Text            := 'Section 2';
    for I                  := 0 to 2 do
    begin
      with TableItems.Add do
      begin
        BackgroundColor       := TAlphaColors.white;
        ItemText.Text         := 'Item ' + IntToStr( i );
        ItemText.Color        := TAlphaColors.Blue;
        ItemDescription.Text  := 'Description ' + IntToStr( i );
        ItemDescription.Color := TAlphaColors.Gray;
        ImageName             := '/Documents/admin.png';
        Style                 := tvcsSubtitle;
      end;
    end;
  end;
  DPFUITableView1.RefreshNeeded;
end;

procedure TFTableViewCustomViewSection.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
