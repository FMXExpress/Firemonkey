unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, DPF.iOS.BaseControl,
  System.Math,
  System.DateUtils,
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
  TFTableViewCustomView = class( TForm )
    DPFUITableView1: TDPFUITableView;
    DPFUIView1: TDPFUIView;
    DPFUIView2: TDPFUIView;
    DPFSwitch1: TDPFSwitch;
    DPFButton1: TDPFButton;
    DPFButton2: TDPFButton;
    procedure FormCreate( Sender: TObject );
    procedure DPFUITableView1DrawCell( Sender: TObject; Section, RowNo: Integer; TableItem: TTableItem; var Objects: TArray<DPF.iOS.BaseControl.TDPFiOSBaseControl>; var Handled: Boolean );
    procedure DPFSwitch1Changed( Sender: TObject; ISON: Boolean );
    procedure DPFButton1Click( Sender: TObject );
    procedure DPFUITableView1GetRowHeight( Sender: TObject; Section, RowNo: Integer; var RowHeight: Single );
    procedure DPFButton2Click( Sender: TObject );
    procedure DPFUITableView1ItemSelect( Sender: TObject; Section, RowNo: Integer; var CellAccessory: TTableViewCellAccessory );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FTableViewCustomView: TFTableViewCustomView;

implementation

{$R *.fmx}

procedure TFTableViewCustomView.DPFButton1Click( Sender: TObject );
begin
  FormCreate( nil );
end;

procedure TFTableViewCustomView.DPFButton2Click( Sender: TObject );
begin
  DPFUITableView1.ClearAll( true, true );
end;

procedure TFTableViewCustomView.DPFSwitch1Changed( Sender: TObject; ISON: Boolean );
begin
  DPFUITableView1.AutoScroll := ISON;
end;

// ------------------------------------------------------------------------------
// Notes:
// Dont create again objects, if length( Objects ) > 0
// ------------------------------------------------------------------------------
procedure TFTableViewCustomView.DPFUITableView1DrawCell( Sender: TObject; Section, RowNo: Integer; TableItem: TTableItem; var Objects: TArray<DPF.iOS.BaseControl.TDPFiOSBaseControl>; var Handled: Boolean );
const
  icons: array [0 .. 24] of string = ( 'admin', 'businessman', 'businessmen', 'calendar', 'flag_iran', 'flower_blue', 'flower_red', 'flower_white', 'flower_yellow', 'lock', 'lock_open', 'loudspeaker', 'mail', 'news', 'shield_green', 'table', 'trafficlight_green', 'trafficlight_off', 'trafficlight_on', 'trafficlight_red', 'trafficlight_red_yellow',
    'trafficlight_yellow', 'user_headphones', 'users2', 'users4' );
var
  MustBeCreate: Boolean;
begin
  Handled      := true;
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
    TDPFImageView( Objects[0] ).Height     := 64;
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

procedure TFTableViewCustomView.DPFUITableView1GetRowHeight( Sender: TObject; Section, RowNo: Integer; var RowHeight: Single );
begin
  RowHeight := 80;

end;

procedure TFTableViewCustomView.DPFUITableView1ItemSelect( Sender: TObject; Section, RowNo: Integer; var CellAccessory: TTableViewCellAccessory );
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

procedure TFTableViewCustomView.FormCreate( Sender: TObject );
var
  I: Integer;
begin
  GetSharedApplication.setStatusBarHidden( true );
  DPFUITableView1.ClearAll( true, true );
  with DPFUITableView1.Sections.Add do
  begin
    for I := 0 to 150 do
    begin
      with TableItems.Add do
      begin
        BackgroundColor := TAlphaColors.white;
        ItemText.Text   := 'Item ' + IntToStr( i );
        ImageName       := '/Documents/admin.png';
      end;
    end;
  end;
  DPFUITableView1.RefreshNeeded;
end;

procedure TFTableViewCustomView.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
