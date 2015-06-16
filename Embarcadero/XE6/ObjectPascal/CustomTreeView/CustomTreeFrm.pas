
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit CustomTreeFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Rtti,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.TreeView, FMX.Styles, FMX.StdCtrls,
  FMX.Objects;

type
  TCustomTreeViewFrm = class(TForm)
    TreeView1: TTreeView;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Image1: TImage;
    Resources1: TStyleBook;
    Image2: TImage;
    InfoLabel: TLabel;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    procedure DoApplyStyleLookup(Sender: TObject);
    procedure DoInfoClick(Sender: TObject);
    procedure DoVisibleChange(Sender: TObject);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CustomTreeViewFrm: TCustomTreeViewFrm;

implementation

{$R *.fmx}

function FindItemParent(Obj: TFmxObject; ParentClass: TClass): TFmxObject;
begin
  Result := nil;
  if Assigned(Obj.Parent) then
    if Obj.Parent.ClassType = ParentClass then
      Result := Obj.Parent
    else
      Result := FindItemParent(Obj.Parent, ParentClass);
end;

procedure TCustomTreeViewFrm.Button1Click(Sender: TObject);
var
  I: Integer;
  Item: TTreeViewItem;
begin
  TreeView1.BeginUpdate;
  for I := 1 to 100 do
  begin
    Item := TTreeViewItem.Create(nil);
    with Item do
    begin
      Parent := TreeView1;
      Text := 'Item ' + IntToStr(I);
      // this code set event - when we need to setup item
      Item.OnApplyStyleLookup := DoApplyStyleLookup;
      // this set our style to new item
      Item.StyleLookup := 'CustomItem';
    end;
  end;
  TreeView1.EndUpdate;
end;

procedure TCustomTreeViewFrm.Button2Click(Sender: TObject);
begin
  TreeView1.ExpandAll;
end;

procedure TCustomTreeViewFrm.Button3Click(Sender: TObject);
begin
  TreeView1.CollapseAll;
end;

procedure TCustomTreeViewFrm.Button4Click(Sender: TObject);
var
  I: Integer;
  Item: TTreeViewItem;
begin
  if TreeView1.Selected = nil then Exit;
  
  TreeView1.BeginUpdate;
  for I := 1 to 10 do
  begin
    Item := TTreeViewItem.Create(nil);
    with Item do
    begin  
      Parent := TreeView1.Selected;
      Height := 32;
      Text := 'Item ' + IntToStr(I);
      // this code set event - when we need to setup item
      Item.OnApplyStyleLookup := DoApplyStyleLookup;
      // this set our style to new item
      Item.StyleLookup := 'CustomChildItem';
    end;
  end;
  TreeView1.EndUpdate;
end;

procedure TCustomTreeViewFrm.DoApplyStyleLookup(Sender: TObject);
var
  Item: TTreeViewItem;
begin
  Item := TTreeViewItem(Sender);
  // create thumbnail
  Item.Text := 'item ' + IntToStr(Item.Index); // set filename
  // use this to set our child controls value - this code use BindingName in style to search
  if odd(Item.Index) then
    Item.StylesData['image'] := Image1.Bitmap // set thumbnail
  else
    Item.StylesData['image'] := Image2.Bitmap; // set thumbnail
  Item.StylesData['resolution'] := '1024x768 px'; // set size
  Item.StylesData['depth'] := '32 bit';
  Item.StylesData['visible'] := true; // set Checkbox value
  Item.StylesData['visible'] := TValue.From<TNotifyEvent>(DoVisibleChange); // set OnChange value
  Item.StylesData['info'] := TValue.From<TNotifyEvent>(DoInfoClick); // set OnClick value
end;

procedure TCustomTreeViewFrm.DoInfoClick(Sender: TObject);
var
  Item : TTreeViewItem;
begin
  Item := TTreeViewItem(FindItemParent(Sender as TFmxObject,TTreeViewItem));
  if Item <> nil then
    InfoLabel.Text := 'Info Button click on ' + IntToStr(Item.GlobalIndex) + ' treeview item';
end;

procedure TCustomTreeViewFrm.DoVisibleChange(Sender: TObject);
var
  Item : TTreeViewItem;
begin
  Item := TTreeViewItem(FindItemParent(Sender as TFmxObject,TTreeViewItem));
  if Item <> nil then
    InfoLabel.Text := 'Checkbox changed ' + IntToStr(Item.GlobalIndex) + ' treeview item to ' + BoolToStr(Item.StylesData['visible'].AsBoolean, true);
end;

end.
