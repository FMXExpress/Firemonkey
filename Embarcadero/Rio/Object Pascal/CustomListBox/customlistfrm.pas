//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit customlistfrm;

interface

uses
  System.SysUtils, System.Variants, System.Classes, System.Types, System.UITypes,
  System.Rtti, FMX.Forms, FMX.Dialogs, FMX.Types, FMX.Layouts, FMX.Styles, FMX.StdCtrls,
  FMX.ListBox, FMX.Objects, FMX.Controls, FMX.Edit, FMX.Effects, FMX.Graphics;

type
  TfrmCustomList = class(TForm)
    ListBox1: TListBox;
    Resources1: TStyleBook;
    OpenDialog1: TOpenDialog;
    InfoLabel: TLabel;
    Label1: TLabel;
    Button2: TButton;
    Button3: TButton;
    CheckBox1: TCheckBox;
    Image1: TImage;
    Image2: TImage;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
  private
    { Private declarations }
    procedure DoInfoClick(Sender: TObject);
    procedure DoVisibleChange(Sender: TObject);
  public
    { Public declarations }
  end;

var
  frmCustomList: TfrmCustomList;

implementation

{$R *.fmx}
{$R *.Windows.fmx MSWINDOWS}

function FindItemParent(Obj: TFmxObject; ParentClass: TClass): TFmxObject;
begin
  Result := nil;
  if Assigned(Obj.Parent) then
    if Obj.Parent.ClassType = ParentClass then
      Result := Obj.Parent
    else
      Result := FindItemParent(Obj.Parent, ParentClass);
end;


procedure TfrmCustomList.Button1Click(Sender: TObject);
var
  Item: TListBoxItem;
  I: Integer;
begin
  OpenDialog1.Filter := TBitmapCodecManager.GetFilterString;
  if OpenDialog1.Execute then
  begin
    // create item and save file name in the tag
    for I := 0 to OpenDialog1.Files.Count - 1 do
    begin
      Item := TListBoxItem.Create(nil);
      Item.Parent := ListBox1;
      Item.TagString := OpenDialog1.Files[I];
      Item.StyleLookup := 'CustomItem';
      Item.Text := OpenDialog1.Files[i]; // set filename
      Item.StylesData['icon'] := OpenDialog1.Files[i];
      Item.StylesData['resolution'] := '1024x768 px'; // set size
      Item.StylesData['depth'] := '32 bit';
      Item.StylesData['visible'] := true; // set Checkbox value
      Item.StylesData['visible.OnChange'] := TValue.From<TNotifyEvent>(DoVisibleChange); // set OnChange value
      Item.StylesData['info.OnClick'] := TValue.From<TNotifyEvent>(DoInfoClick); // set OnClick value
    end;
    Caption := IntToStr(ListBox1.Count) + ' items';
  end;
end;

procedure TfrmCustomList.Button2Click(Sender: TObject);
var
  Item: TListBoxItem;
begin
  // create custom item
  Item := TListBoxItem.Create(nil);
  Item.Parent := ListBox1;
  Item.StyleLookup := 'CustomItem';
  Item.Text := 'item ' + IntToStr(Item.Index); // set filename
  if Odd(Item.Index) then
    Item.ItemData.Bitmap := Image1.Bitmap // set thumbnail
  else
    Item.ItemData.Bitmap := Image2.Bitmap; // set thumbnail
  Item.StylesData['resolution'] := '1024x768 px'; // set size
  Item.StylesData['depth'] := '32 bit';
  Item.StylesData['visible'] := true; // set Checkbox value
  Item.StylesData['visible.OnChange'] := TValue.From<TNotifyEvent>(DoVisibleChange); // set OnChange value
  Item.StylesData['info.OnClick'] := TValue.From<TNotifyEvent>(DoInfoClick); // set OnClick value
end;

procedure TfrmCustomList.DoInfoClick(Sender: TObject);
var
  Item : TListBoxItem;
begin
  Item := TListBoxItem(FindItemParent(Sender as TFmxObject,TListBoxItem));
  if Assigned(Item) then
    InfoLabel.Text := 'Info Button click on ' + IntToStr(Item.Index) + ' listbox item';
end;

procedure TfrmCustomList.DoVisibleChange(Sender: TObject);
var
  Item : TListBoxItem;
begin
  Item := TListBoxItem(FindItemParent(Sender as TFmxObject,TListBoxItem));
  if Assigned(Item) then
    InfoLabel.Text := 'Checkbox changed ' + IntToStr(Item.Index) + ' listbox item to ' + BoolToStr(Item.StylesData['visible'].AsBoolean, true);
end;

procedure TfrmCustomList.Button3Click(Sender: TObject);
var
  i: integer;
begin
  ListBox1.BeginUpdate;
  for i := 1 to 1000 do
    Button2Click(Sender);
  ListBox1.EndUpdate;

  Caption := IntToStr(ListBox1.Count) + ' items';
end;

procedure TfrmCustomList.CheckBox1Change(Sender: TObject);
begin
  ListBox1.AllowDrag := CheckBox1.IsChecked;
end;

end.
