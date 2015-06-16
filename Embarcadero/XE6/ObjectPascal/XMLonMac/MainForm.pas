
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Dialogs,
  FMX.Types, FMX.Controls, FMX.Forms, Macapi.Foundation, Macapi.Helpers, FMX.Layouts,
  FMX.Memo, FMX.Edit, FMX.Effects, FMX.Objects, FMX.StdCtrls;

type
  TFrmXml = class(TForm)
    CreateXMLButton: TButton;
    XmlContent: TMemo;
    XMLLocation: TEdit;
    Label1: TLabel;
    Image1: TImage;
    procedure CreateXMLButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function Attribute(Name, Value: String): NSXMLNode;
    function CreateElement(Name: String; Attr: NSXMLNode)
      : NSXMLElement; overload;
    function CreateElement(Name, Value: String): NSXMLElement; overload;
  end;

var
  FrmXml: TFrmXml;

implementation

{$R *.fmx}

function TFrmXml.Attribute(Name, Value: String): NSXMLNode;
var
  Node: Pointer;
begin
  Node := TNSXMLNode.OCClass.attributeWithName(StrToNSStr(Name), StrToNSStr(Value));
  Result := TNSXMLNode.Wrap(Node);
end;

function TFrmXml.CreateElement(Name, Value: String): NSXMLElement;
begin
  Result := TNSXMLElement.Create;
  Result.setName(StrToNSStr(Name));
  Result.setStringValue(StrToNSStr(Value));
end;

function TFrmXml.CreateElement(Name: String; Attr: NSXMLNode): NSXMLElement;
begin
  Result := TNSXMLElement.Create;
  Result.initWithName(StrToNSStr(Name));

  if Assigned(Attr) then
    Result.addAttribute(Attr);

end;

procedure TFrmXml.CreateXMLButtonClick(Sender: TObject);
var
  XmlDoc: NSXMLDocument;
  Root, Book, Author, Publisher: NSXMLElement;
  Data: NSData;
begin

  // Create Xml Document
  XmlDoc := TNSXMLDocument.Create;
  XmlDoc.setVersion(StrToNSStr('1.0'));
  XmlDoc.setCharacterEncoding(StrToNSStr('UTF-8'));

  // Create the root doc element with one attributes
  Root := CreateElement('BookStore', Attribute('url', 'http://www.amazon.com'));
  XmlDoc.initWithRootElement(Root);

  // Create the first Book node
  Book := CreateElement('Book', Attribute('Name', 'Steve Jobs'));

  // Create the Author and Publisher elements
  Author := CreateElement('Author', 'Walter Isaacson');
  Publisher := CreateElement('Publisher', 'Simon Schuster (October 24, 2011)');

  // Add the elements to the XML
  Root.addChild(Book);
  Book.addChild(Author);
  Book.addChild(Publisher);

  // Create the second Book node
  Book := CreateElement('Book', Attribute('Name',
    'Clean Code: A Handbook of Agile Software Craftsmanship'));
  Author := CreateElement('Author', 'Robert C. Martin');
  Publisher := CreateElement('Publisher',
    'Prentice Hall; 1 edition (August 11, 2008)');

  // Add the elements from the second Book node to the XML
  Root.addChild(Book);
  Book.addChild(Author);
  Book.addChild(Publisher);

  // Makes the Xml output more human-readable inserting
  // carriage returns and indenting nested elements
  Data := XmlDoc.XMLDataWithOptions(NSXMLNodePrettyPrint);
  Data.writeToFile(StrToNSStr(XMLLocation.Text), true);

  XmlContent.Lines.LoadFromFile(XMLLocation.Text);

end;

end.
