unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, DPF.iOS.BaseControl,
  DPF.iOS.UITableView, DPF.iOS.UITableViewItems,
  System.Math,
  System.DateUtils,
  DPF.iOS.Common,
  DPF.iOS.UIFont,

  DPF.iOS.UILabel,
  DPF.iOS.UIImageView,
  DPF.iOS.UIView,
  DPF.iOS.UIButton,
  XML.xmldom,
  XML.XMLDoc,
  XML.XMLIntf,
  DPF.iOS.HTTP,
  DPF.iOS.UIViewController,
  DPF.iOS.UINavigationController,
  DPF.iOS.UITextView,
  DPF.iOS.CacheManager;

type
  TFTableView = class( TForm )
    DPFUITableView1: TDPFUITableView;
    DPFHttp1: TDPFHttp;
    DPFNavigationController1: TDPFNavigationController;
    DPFNavigationControllerPage1: TDPFNavigationControllerPage;
    DPFNavigationControllerPage2: TDPFNavigationControllerPage;
    DPFImageView1: TDPFImageView;
    DPFLabelTitle: TDPFLabel;
    DPFTextViewDetail: TDPFTextView;
    procedure DPFUITableView1ItemDeSelect( Sender: TObject; Section, RowNo: Integer; var CellAccessory: TTableViewCellAccessory );
    procedure DPFUITableView1ItemSelect( Sender: TObject; Section, RowNo: Integer; var CellAccessory: TTableViewCellAccessory );
    procedure DPFUITableView1RefreshNeeded( Sender: TObject );
    procedure DPFHttp1ReceiveData( Sender: TObject; Data: string; var isFree: Boolean );
    procedure DPFHttp1ReceiveError( Sender: TObject; Error: string; var isFree: Boolean );
    procedure DPFUITableView1DrawCell( Sender: TObject; Section, RowNo: Integer; TableItem: TTableItem; var Objects: TArray<DPF.iOS.BaseControl.TDPFiOSBaseControl>; var Handled: Boolean );
    procedure DPFUITableView1GetRowHeight( Sender: TObject; Section, RowNo: Integer; var RowHeight: Single );
    procedure FormCreate( Sender: TObject );
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

function StripHTML( S: string ): string;
var
  TagBegin, TagEnd, TagLength: integer;
begin
  TagBegin := Pos( '<', S ); // search position of first <

  while ( TagBegin > 0 ) do
  begin                         // while there is a < in S
    TagEnd    := Pos( '>', S ); // find the matching >
    TagLength := TagEnd - TagBegin + 1;
    Delete( S, TagBegin, TagLength ); // delete the tag
    TagBegin := Pos( '<', S );        // search for next <
  end;

  Result := S.Replace( #10, '', [rfReplaceAll] ).Replace( #13, '', [rfReplaceAll] );
end;

procedure TFTableView.DPFUITableView1RefreshNeeded( Sender: TObject );
begin
  DPFHttp1.GetUrlContentString( 'http://www.pcworld.com/index.rss', [], true );
  // DPFHttp1.GetUrlContentString( 'http://finance.yahoo.com/lifestyle/?format=rss', [], true );
  // DPFHttp1.GetUrlContentString( 'http://finance.yahoo.com/video/?format=rss', [], true );
end;

procedure TFTableView.FormCreate( Sender: TObject );
begin
  InitCache( 4 * 1024 * 1024, 32 * 1024 * 1024, 'DPFURLCache' );
end;

procedure TFTableView.DPFHttp1ReceiveData( Sender: TObject; Data: string; var isFree: Boolean );
var
  XMLDoc                      : IXMLDocument;
  StartItemNode               : IXMLNode;
  ANode                       : IXMLNode;
  sMedia, STitle, sDesc, sLink: string;
  TI                          : TTableItem;
  V                           : OleVariant;
begin
  if Data = '' then
  begin
    ShowMessage( 'No Data Received !' );
    exit;
  end;

  DPFUITableView1.ClearAll( );
  DPFUITableView1.Sections.Add;

  try
    XMLDoc := LoadXMLData( Data );

    StartItemNode := XMLDoc.DocumentElement.ChildNodes.First.ChildNodes.FindNode( 'item' );
    ANode         := StartItemNode;
    repeat
      STitle := ANode.ChildNodes['title'].Text;
      sLink  := ANode.ChildNodes['link'].Text;
      sDesc  := ANode.ChildNodes['description'].Text;
      V      := varNull;
      try
        V := ANode.ChildNodes['media:content'].GetAttributeNS( 'url', '' );
      except
      end;
      sMedia := '';
      if not VarIsNull( V ) then
        sMedia := V;
      if STitle <> '' then
      begin
        TI                        := DPFUITableView1.Sections[0].TableItems.Add;
        TI.ItemText.Text          := STitle;
        TI.ItemText.NumberOfLines := 0;
        TI.ItemDescription.Text   := StripHTML( sDesc );
        TI.ImageName              := sMedia;
      end;

      ANode := ANode.NextSibling;
    until ANode = nil;
    DPFUITableView1.RefreshNeeded;
    DPFUITableView1.EndRefreshing;
  except
    ShowMessage( 'No Valid Data Recieved !' );
  end;
end;

procedure TFTableView.DPFHttp1ReceiveError( Sender: TObject; Error: string; var isFree: Boolean );
begin
  DPFUITableView1.EndRefreshing;
  ShowMessage( Error );
end;

procedure TFTableView.DPFUITableView1DrawCell( Sender: TObject; Section, RowNo: Integer; TableItem: TTableItem; var Objects: TArray<DPF.iOS.BaseControl.TDPFiOSBaseControl>; var Handled: Boolean );
var
  MustBeCreate: Boolean;
  // TI          : TTableItem;
begin
  Handled      := true;
  MustBeCreate := length( Objects ) = 0;
  if MustBeCreate then
    SetLength( Objects, 3 );

  // TI := DPFUITableView1.Row[Section, RowNo];
  if MustBeCreate then
  begin
    TDPFImageView( Objects[0] )               := TDPFImageView.Create( nil );
    TDPFImageView( Objects[0] ).Position.X    := 0;
    TDPFImageView( Objects[0] ).Width         := 80;
    TDPFImageView( Objects[0] ).Shadow        := false;
    TDPFImageView( Objects[0] ).ShowIndicator := true;
    TDPFImageView( Objects[0] ).Height        := 80;
    TDPFImageView( Objects[0] ).CacheImage    := true;
    TDPFImageView( Objects[0] ).LoadAsync     := true;
    TDPFImageView( Objects[0] ).LoadFromURL   := true;
  end;
  TDPFImageView( Objects[0] ).ClearImage;
  TDPFImageView( Objects[0] ).ImageList.Text := TableItem.ImageName;
  TDPFImageView( Objects[0] ).ReloadImage;

  if MustBeCreate then
  begin
    TDPFLabel( Objects[1] )               := TDPFLabel.Create( nil );
    TDPFLabel( Objects[1] ).TextColor     := TAlphaColors.Navy;
    TDPFLabel( Objects[1] ).Font.FontName := ios_HelveticaNeue_Bold;
    TDPFLabel( Objects[1] ).Font.FontSize := 16;
    TDPFLabel( Objects[1] ).Width         := DPFUITableView1.Width - TDPFImageView( Objects[0] ).Width - 5;
    TDPFLabel( Objects[1] ).Height        := 20;
    TDPFLabel( Objects[1] ).NumberOfLines := 0;
    TDPFLabel( Objects[1] ).LineBreak     := TDPFLineBreak.lbTailTruncation;
    TDPFLabel( Objects[1] ).Position.x    := TDPFImageView( Objects[0] ).Position.X + TDPFImageView( Objects[0] ).Width + 5;
    TDPFLabel( Objects[1] ).Position.Y    := 5;
  end;
  TDPFLabel( Objects[1] ).Text := TableItem.ItemText.Text;

  if MustBeCreate then
  begin
    TDPFLabel( Objects[2] )               := TDPFLabel.Create( nil );
    TDPFLabel( Objects[2] ).TextColor     := TAlphaColors.Gray;
    TDPFLabel( Objects[2] ).Font.FontSize := 12;
    TDPFLabel( Objects[2] ).Width         := TDPFLabel( Objects[1] ).Width;
    TDPFLabel( Objects[2] ).Height        := 50;
    TDPFLabel( Objects[2] ).NumberOfLines := 2;
    TDPFLabel( Objects[2] ).LineBreak     := TDPFLineBreak.lbTailTruncation;
    TDPFLabel( Objects[2] ).Position.x    := TDPFImageView( Objects[0] ).Position.X + TDPFImageView( Objects[0] ).Width + 1;
    TDPFLabel( Objects[2] ).Position.Y    := TDPFLabel( Objects[1] ).Position.Y + TDPFLabel( Objects[1] ).Height + 1;
  end;
  TDPFLabel( Objects[2] ).Text := TableItem.ItemDescription.Text;
end;

procedure TFTableView.DPFUITableView1GetRowHeight( Sender: TObject; Section, RowNo: Integer; var RowHeight: Single );
begin
  RowHeight := 80;
end;

procedure TFTableView.DPFUITableView1ItemDeSelect( Sender: TObject; Section, RowNo: Integer; var CellAccessory: TTableViewCellAccessory );
begin
  // CellAccessory := TTableViewCellAccessory.tvcaNone;
end;

procedure TFTableView.DPFUITableView1ItemSelect( Sender: TObject; Section, RowNo: Integer; var CellAccessory: TTableViewCellAccessory );
var
  CustomViews: TArray<TDPFiOSBaseControl>;
begin
  // CellAccessory := TTableViewCellAccessory.tvcaCheckmark;

  CustomViews := DPFUITableView1.GetRowCustomViews( Section, RowNo );
  if length( CustomViews ) > 0 then
  begin
    DPFImageView1.SetImage( TDPFImageView( CustomViews[0] ) );
    DPFLabelTitle.Text     := TDPFLabel( CustomViews[1] ).Text;
    DPFTextViewDetail.Text := TDPFLabel( CustomViews[2] ).Text;
  end;
  DPFNavigationController1.PushViewController( DPFNavigationControllerPage2, true );
end;

procedure TFTableView.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
