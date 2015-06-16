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
  TFTableViewVirtualCustomView = class( TForm )
    DPFUITableView1: TDPFUITableView;
    DPFUIView1: TDPFUIView;
    DPFUIView2: TDPFUIView;
    DPFSwitch1: TDPFSwitch;
    DPFButton1: TDPFButton;
    DPFButton2: TDPFButton;
    DPFLabel1: TDPFLabel;
    procedure DPFUITableView1DrawCell( Sender: TObject; Section, RowNo: Integer; TableItem: TTableItem; var Objects: TArray<DPF.iOS.BaseControl.TDPFiOSBaseControl>; var Handled: Boolean );
    procedure DPFSwitch1Changed( Sender: TObject; ISON: Boolean );
    procedure DPFButton1Click( Sender: TObject );
    procedure DPFButton2Click( Sender: TObject );
    procedure DPFUITableView1GetNumberOfSections( Sender: TObject; inSerachMode: Boolean; var NumberOfSections: Integer );
    procedure DPFUITableView1GetNumberOfRowsInSection( Sender: TObject; inSerachMode: Boolean; SectionNo: Integer; var numberOfRowsInSection: Integer );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FTableViewVirtualCustomView: TFTableViewVirtualCustomView;

implementation

{$R *.fmx}

procedure TFTableViewVirtualCustomView.DPFButton1Click( Sender: TObject );
begin
  DPFUITableView1.Tag := 0;
  DPFUITableView1.ClearAll( false, true );
  DPFUITableView1.RefreshNeeded;
end;

procedure TFTableViewVirtualCustomView.DPFButton2Click( Sender: TObject );
begin
  DPFUITableView1.Tag := 10;
  DPFUITableView1.ClearAll( true, true );
end;

procedure TFTableViewVirtualCustomView.DPFSwitch1Changed( Sender: TObject; ISON: Boolean );
begin
  DPFUITableView1.AutoScroll := ISON;
end;

// ------------------------------------------------------------------------------
// Notes:
// Dont create again objects, if length( Objects ) > 0
// ------------------------------------------------------------------------------
procedure TFTableViewVirtualCustomView.DPFUITableView1DrawCell( Sender: TObject; Section, RowNo: Integer; TableItem: TTableItem; var Objects: TArray<DPF.iOS.BaseControl.TDPFiOSBaseControl>; var Handled: Boolean );
const
  icons: array [0 .. 24] of string = ( 'admin', 'businessman', 'businessmen', 'calendar', 'flag_iran', 'flower_blue', 'flower_red', 'flower_white', 'flower_yellow', 'lock', 'lock_open', 'loudspeaker', 'mail', 'news', 'shield_green', 'table', 'trafficlight_green', 'trafficlight_off', 'trafficlight_on', 'trafficlight_red', 'trafficlight_red_yellow',
    'trafficlight_yellow', 'user_headphones', 'users2', 'users4' );
var
  MustBeCreate: Boolean;
begin
  Handled      := true;
  MustBeCreate := length( Objects ) = 0;
  if MustBeCreate then
    SetLength( Objects, 4 );

  if MustBeCreate then
  begin
    TDPFImageView( Objects[0] )            := TDPFImageView.Create( nil );
    TDPFImageView( Objects[0] ).Position.X := 0;
    TDPFImageView( Objects[0] ).Position.X := 0;
    TDPFImageView( Objects[0] ).Width      := 64;
    TDPFImageView( Objects[0] ).Shadow     := false;
    TDPFImageView( Objects[0] ).Height     := 64;
  end;
  TDPFImageView( Objects[0] ).ImageList.Text := '/Documents/' + icons[RandomRange( 0, 25 )] + '.png';

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
    TDPFLabel( Objects[2] )               := TDPFLabel.Create( nil );
    TDPFLabel( Objects[2] ).Text          := 'Custom Label';
    TDPFLabel( Objects[2] ).TextColor     := TAlphaColors.Black;
    TDPFLabel( Objects[2] ).Font.FontName := ios_Verdana_Bold;
    TDPFLabel( Objects[2] ).Width         := 200;
    TDPFLabel( Objects[2] ).Height        := 15;
    TDPFLabel( Objects[2] ).Position.x    := TDPFImageView( Objects[0] ).Position.X + TDPFImageView( Objects[0] ).Width + 1;
    TDPFLabel( Objects[2] ).Position.Y    := 10;
  end;

  if MustBeCreate then
  begin
    TDPFLabel( Objects[3] )               := TDPFLabel.Create( nil );
    TDPFLabel( Objects[3] ).Text          := 'Custom Detail Label';
    TDPFLabel( Objects[3] ).TextColor     := TAlphaColors.Gray;
    TDPFLabel( Objects[3] ).Font.FontName := ios_Verdana_Bold;
    TDPFLabel( Objects[3] ).Width         := 200;
    TDPFLabel( Objects[3] ).Height        := 15;
    TDPFLabel( Objects[3] ).Position.x    := TDPFImageView( Objects[0] ).Position.X + TDPFImageView( Objects[0] ).Width + 1;
    TDPFLabel( Objects[3] ).Position.Y    := 40;
  end;
end;

procedure TFTableViewVirtualCustomView.DPFUITableView1GetNumberOfRowsInSection( Sender: TObject; inSerachMode: Boolean; SectionNo: Integer; var numberOfRowsInSection: Integer );
begin
  if DPFUITableView1.Tag <> 10 then
    numberOfRowsInSection := 1000
  else
    numberOfRowsInSection := 0;
end;

procedure TFTableViewVirtualCustomView.DPFUITableView1GetNumberOfSections( Sender: TObject; inSerachMode: Boolean; var NumberOfSections: Integer );
begin
  if DPFUITableView1.Tag <> 10 then
    NumberOfSections := 1
  else
    NumberOfSections := 0;
end;

procedure TFTableViewVirtualCustomView.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
