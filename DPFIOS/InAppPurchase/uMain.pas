// ---------------------------------------------------------------------------------------
//
// 1.	In iTunes connect ensure that you have a unique App ID and when we create the application update with the bundle ID and code signing in Xcode with corresponding provisioning profile.
// 2.	Create a new application and update application information. You can know more about this in apple's Add new apps documentation.
// 3.	Add a new product for in-app purchase in Manage In-App Purchase of your application's page.
// 4.	Ensure you setup the bank details for your application. This needs to be setup for In-App purchase to work. Also create a test user account using Manage Users option in iTunes connect page of your app.
// 5. Set this project Bundle ID with your itunesconnect app Bundle ID
// ---------------------------------------------------------------------------------------

unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, IniFiles,

  DPF.iOS.StoreKit,

  DPF.iOS.Common,
  DPF.iOS.BaseControl,
  DPF.iOS.UIButton,
  DPF.iOS.UILabel,
  DPF.iOS.UIView,
  DPF.iOS.UITextView,
  DPF.iOS.UIActivityIndicatorView,
  DPF.iOS.UITableViewItems,
  DPF.iOS.UITableView,

  DPF.iOS.UIToolbar,
  DPF.iOS.SlideDialog;

type
  TFInAppPurchase = class( TForm )
    DPFUIView1: TDPFUIView;
    DPFInAppPurchase1: TDPFInAppPurchase;
    DPFActivityIndicatorView1: TDPFActivityIndicatorView;
    DPFUITableView1: TDPFUITableView;
    DPFToolbar1: TDPFToolbar;
    DPFButton1: TDPFButton;
    DPFSlideDialog1: TDPFSlideDialog;
    DPFLabel1: TDPFLabel;
    DPFButton2: TDPFButton;
    procedure DPFButton1Click( Sender: TObject );
    procedure DPFInAppPurchase1ProductsRequest( sender: TObject; Products: array of TProductInfo; InvalidProducts: array of string );
    procedure DPFInAppPurchase1ProductsRequestFinished( sender: TObject );
    procedure DPFInAppPurchase1ProductsRequestError( sender: TObject; Error: string );
    procedure FormShow( Sender: TObject );
    procedure DPFUITableView1GetRowHeight( Sender: TObject; Section, RowNo: Integer; var RowHeight: Single );
    procedure DPFUITableView1ApplyFrameData( Sender: TObject; Section, RowNo: Integer; TableItem: TTableItem; var Frame: TFrame; var Handled: Boolean );
    procedure DPFButton2Click( Sender: TObject );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FInAppPurchase: TFInAppPurchase;

implementation

uses uRowFrame;

{$R *.fmx}

procedure TFInAppPurchase.DPFButton1Click( Sender: TObject );
begin
  DPFActivityIndicatorView1.StartAnimating;
  // com.dpfaragir.dpfinapptest
  DPFInAppPurchase1.FetchAvailableProducts( ['com.dpfaragir.dpfinapptest'] );
end;

procedure TFInAppPurchase.DPFButton2Click( Sender: TObject );
begin
  DPFInAppPurchase1.ShowProductInAppStore( 733449578, nil );
end;

procedure TFInAppPurchase.DPFInAppPurchase1ProductsRequest( sender: TObject; Products: array of TProductInfo; InvalidProducts: array of string );
var
  i : Integer;
  TE: TTableItem;
begin
  DPFActivityIndicatorView1.StopAnimating;
  DPFUITableView1.ClearAll( );
  if Length( Products ) = 0 then
  begin
    DPFSlideDialog1.Show( 'No product available!', 200, 80 );
    exit;
  end
  else
    DPFUITableView1.Sections.Add;

  for I := 0 to Length( Products ) - 1 do
  begin
    TE                      := DPFUITableView1.Sections[0].TableItems.Add;
    TE.Style                := TTableViewCellStyle.tvcsSubtitle;
    TE.ItemText.Text        := Products[i].localizedTitle;
    TE.ItemDescription.Text := Products[i].localizedDescription;
    TE.TagFloat             := Products[i].price;
    TE.TagStr               := Products[i].productIdentifier;
  end;
  DPFUITableView1.RefreshNeeded;
end;

procedure TFInAppPurchase.DPFInAppPurchase1ProductsRequestError( sender: TObject; Error: string );
begin
  DPFActivityIndicatorView1.StopAnimating;
  DPFSlideDialog1.Show( 'Request Error!' + #10#13 + Error, 200, 80 );
end;

procedure TFInAppPurchase.DPFInAppPurchase1ProductsRequestFinished( sender: TObject );
begin
  DPFActivityIndicatorView1.StopAnimating;
end;

procedure TFInAppPurchase.DPFUITableView1ApplyFrameData( Sender: TObject; Section, RowNo: Integer; TableItem: TTableItem; var Frame: TFrame; var Handled: Boolean );
begin
  if Frame = nil then
    Frame := TFrameRow.Create( nil );

  TFrameRow( Frame ).SectionNo          := Section;
  TFrameRow( Frame ).RowNo              := RowNo;
  TFrameRow( Frame ).DPFLabelTitle.Text := DPFUITableView1.Row[Section, RowNo].ItemText.Text;
  TFrameRow( Frame ).DPFLabelDesc.Text  := DPFUITableView1.Row[Section, RowNo].ItemDescription.Text;
  TFrameRow( Frame ).DPFLabelPrice.Text := DPFUITableView1.Row[Section, RowNo].TagFloat.ToString;
end;

procedure TFInAppPurchase.DPFUITableView1GetRowHeight( Sender: TObject; Section, RowNo: Integer; var RowHeight: Single );
begin
  RowHeight := 80;
end;

procedure TFInAppPurchase.FormShow( Sender: TObject );
var
  n    : TIniFile;
  v    : integer;
  DPath: string;
begin
  DPath := GetDocumentsFolder;
  ForceDirectories( DPath );
  n       := TIniFile.Create( DPath + 'config.ini' );
  v       := n.ReadInteger( 'Rate', 'counter', 0 );
  Caption := v.ToString( );
  n.WriteInteger( 'Rate', 'counter', 1231 );
  n.Free;
  DPFActivityIndicatorView1.Visible := false;

end;

procedure TFInAppPurchase.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
