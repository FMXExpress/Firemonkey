// ------------------------------------------------------------------------------
// Developed By: Pierre Moati
// ------------------------------------------------------------------------------
unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,

  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,

  Xml.xmldom, Xml.XMLIntf, Xml.XMLDoc,

  DPF.iOS.UITableView, DPF.iOS.BaseControl, DPF.iOS.UIView, DPF.iOS.common,
  DPF.iOS.UITableViewItems, DPF.iOS.UILabel, DPF.iOS.UIButton;

type
  TMainForm = class( TForm )
    DPFUIView1: TDPFUIView;
    DPFUITableView1: TDPFUITableView;
    procedure FormCreate( Sender: TObject );
    procedure DPFUITableView1GetRowHeight( Sender: TObject; Section, RowNo: Integer; var RowHeight: Single );
    procedure DPFUITableView1NeedCellIdentifier( Sender: TObject; Section, RowNo: Integer; var CellIdentifier: string );
    procedure DPFUITableView1DrawCell( Sender: TObject; Section, RowNo: Integer; TableItem: TTableItem; var Objects: TArray<DPF.iOS.BaseControl.TDPFiOSBaseControl>; var Handled: Boolean );
  private
    { Private declarations }
    function getHeightCellFromType( cellType: string ): Single;
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;
  cells   : IXMLNodeList;

implementation

{$R *.fmx}

const
  CT_LABEL  = 'label';
  CT_BUTTON = 'button';
  CT_LIST   = 'list';
  CT_VIEW   = 'view';

  MARGIN = 5;

  CT_BUTTON_H = 50;
  CT_LIST_H   = 100;
  CT_VIEW_H   = 70;
  CT_OTHER_H  = 30;

function TMainForm.getHeightCellFromType( cellType: string ): Single;
begin
  if ( cellType = CT_BUTTON ) then
    Result := CT_BUTTON_H
  else if ( cellType = CT_LIST ) then
    Result := CT_LIST_H
  else if ( cellType = CT_VIEW ) then
    Result := CT_VIEW_H
  else
    Result := CT_OTHER_H;
end;

procedure TMainForm.DPFUITableView1DrawCell( Sender: TObject; Section, RowNo: Integer; TableItem: TTableItem; var Objects: TArray<DPF.iOS.BaseControl.TDPFiOSBaseControl>; var Handled: Boolean );
var
  o              : TDPFiOSBaseControl;
  MustBeCreate   : Boolean;
  aText, cellType: string;
  cell           : IXMLNode;
  lines          : IXMLNodeList;
  i              : Cardinal;
  text           : TDPFLabel;
begin
  Handled      := true;
  MustBeCreate := length( Objects ) = 0;

  cell     := cells[RowNo];
  cellType := cell.ChildValues['type'];

  if MustBeCreate then
    SetLength( Objects, 1 );

  if ( cellType <> CT_LIST ) then
    aText := cell.ChildValues['text'];

  if ( cellType = CT_BUTTON ) then
  begin
    // Draw a button
    if MustBeCreate then
    begin
      Objects[0]                      := TDPFButton.Create( Self );
      TDPFButton( Objects[0] ).Height := CT_BUTTON_H;
      TDPFButton( Objects[0] ).Loaded;
    end;
    TDPFButton( Objects[0] ).Text := aText;
  end
  else if ( cellType = CT_LIST ) then
  begin
    // Draw a list
    if MustBeCreate then
    begin
      Objects[0]                                  := TDPFUITableView.Create( Self );
      TDPFUITableView( Objects[0] ).ClipsToBounds := true;
      TDPFUITableView( Objects[0] ).Loaded;
    end;
    TDPFUITableView( Objects[0] ).ClearAll( );
    TDPFUITableView( Objects[0] ).Sections.Add;
    lines := cell.ChildNodes['text'].ChildNodes;
    // For each line, show it on a cell
    for i := 0 to lines.Count - 1 do
      with ( TDPFUITableView( Objects[0] ).Sections[0].TableItems.Add ) do
        ItemText.Text := lines[i].Text;
    TDPFUITableView( Objects[0] ).RefreshNeeded;
  end
  else if ( cellType = CT_VIEW ) then
  begin
    // Draw a UIView
    if MustBeCreate then
    begin
      Objects[0] := TDPFUIView.Create( Self );
      TDPFUIView( Objects[0] ).Loaded;
      text        := TDPFLabel.Create( Objects[0] );
      text.Parent := Objects[0];
      text.Align  := TAlignLayout.alClient;
      text.Loaded;
    end
    else
      text    := TDPFLabel( TDPFUIView( Objects[0] ).Children[0] );
    text.Text := aText;
    text.sizeToFit;
  end
  else
  begin
    if MustBeCreate then
    begin
      Objects[0] := TDPFLabel.Create( Self );
      TDPFLabel( Objects[0] ).Loaded;
    end;
    TDPFLabel( Objects[0] ).Text := aText;
  end;

  o            := Objects[0];
  o.Position.X := MARGIN;
  o.Position.Y := MARGIN;
  o.Width      := Width - 6 * MARGIN;
  o.Height     := TableItem.Tag - 2 * MARGIN;

end;

procedure TMainForm.DPFUITableView1GetRowHeight( Sender: TObject; Section, RowNo: Integer; var RowHeight: Single );
var
  cellType: string;
begin
  // give a specific height for the type of cell
  cellType                                := cells[RowNo].ChildValues['type'];
  RowHeight                               := getHeightCellFromType( cellType );
  DPFUITableView1.Row[Section, RowNo].Tag := round( RowHeight );
end;

procedure TMainForm.DPFUITableView1NeedCellIdentifier( Sender: TObject; Section, RowNo: Integer; var CellIdentifier: string );
begin
  // Add a cell identifier. By this way, a reuse cell with different type with not be switched
  CellIdentifier := cells[RowNo].ChildValues['type'];
end;

procedure TMainForm.FormCreate( Sender: TObject );
var
  XmlDoc: TXMLDocument;
  i     : Cardinal;
begin
  // load the xml file
  XmlDoc           := TXMLDocument.Create( self );
  XmlDoc.DOMVendor := GetDOMVendor( 'ADOM XML v4' );
  XmlDoc.LoadFromFile( { GetAppFolder( ) + } 'cells.xml' );

  // Store cells in IXMLNodeList
  cells := XmlDoc.DocumentElement.ChildNodes['cells'].ChildNodes;

  DPFUITableView1.Sections.Add;
  // Insert the same quantity of tableitems than cell node in xml
  for i := 0 to cells.Count - 1 do
    DPFUITableView1.Sections[0].TableItems.Add;
end;

procedure TMainForm.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
