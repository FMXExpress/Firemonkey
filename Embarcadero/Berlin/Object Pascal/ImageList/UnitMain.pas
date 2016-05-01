//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit UnitMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, FMX.Types, FMX.Controls, FMX.Forms,
  FMX.Graphics, FMX.Dialogs, UnitDataModule, FMX.Menus, FMX.TabControl, FMX.StdCtrls, FMX.MultiResBitmap,
  FMX.Controls.Presentation, FMX.Objects, FMX.ImgList, FMX.ActnList, System.Actions, FMX.StdActns, FMX.TreeView,
  FMX.Layouts, FMX.ListBox, FMX.ListView.Types, FMX.ListView, Data.Bind.GenData, System.Rtti, System.Bindings.Outputs,
  Fmx.Bind.Editors, Data.Bind.EngExt, Fmx.Bind.DBEngExt, Data.Bind.Components, Data.Bind.ObjectScope, System.ImageList,
  FMX.ListView.Appearances;

type
  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuBar1: TMenuBar;
    CopyMenuItem1: TMenuItem;
    CopyMenuItem2: TMenuItem;
    CopyMenuItem3: TMenuItem;
    CopyMenuItem4: TMenuItem;
    CopyMenuItem5: TMenuItem;
    CopyMenuItem6: TMenuItem;
    CopyMenuItem7: TMenuItem;
    CopyMenuItem8: TMenuItem;
    CopyMenuItem9: TMenuItem;
    CopyMenuItem10: TMenuItem;
    CopyMenuItem11: TMenuItem;
    CopyMenuItem12: TMenuItem;
    CopyMenuItem13: TMenuItem;
    CopyMenuItem14: TMenuItem;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    TabItem4: TTabItem;
    Panel1: TPanel;
    Splitter1: TSplitter;
    Button1: TButton;
    CornerButton1: TCornerButton;
    SpeedButton1: TSpeedButton;
    Button2: TButton;
    Selection1: TSelection;
    Glyph1: TGlyph;
    Rectangle1: TRectangle;
    ToolBar1: TToolBar;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    CheckBox1: TCheckBox;
    Panel2: TPanel;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    ActionList1: TActionList;
    FileExit1: TFileExit;
    FileExit2: TFileExit;
    ActnNextImage: TAction;
    ActnDormant: TAction;
    ActnUpCache: TAction;
    ActnDownCache: TAction;
    ActnClearCache: TAction;
    ActnUpdateText: TAction;
    PopupMenu1: TPopupMenu;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    Splitter2: TSplitter;
    TreeView1: TTreeView;
    TreeViewItem1: TTreeViewItem;
    TreeViewItem2: TTreeViewItem;
    TreeViewItem3: TTreeViewItem;
    TreeViewItem4: TTreeViewItem;
    TreeViewItem5: TTreeViewItem;
    TreeViewItem6: TTreeViewItem;
    TreeViewItem7: TTreeViewItem;
    TreeViewItem8: TTreeViewItem;
    TreeViewItem9: TTreeViewItem;
    TreeViewItem10: TTreeViewItem;
    TreeViewItem11: TTreeViewItem;
    ComboBox1: TComboBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    ListBoxItem4: TListBoxItem;
    ListBox1: TListBox;
    Panel3: TPanel;
    Splitter3: TSplitter;
    ActnAddSource: TAction;
    Button7: TButton;
    TreeViewItem12: TTreeViewItem;
    TreeViewItem13: TTreeViewItem;
    TreeViewItem14: TTreeViewItem;
    TreeViewItem15: TTreeViewItem;
    TreeViewItem16: TTreeViewItem;
    TreeViewItem17: TTreeViewItem;
    TreeViewItem18: TTreeViewItem;
    TreeViewItem19: TTreeViewItem;
    TreeViewItem20: TTreeViewItem;
    TreeViewItem21: TTreeViewItem;
    TreeViewItem22: TTreeViewItem;
    TreeViewItem23: TTreeViewItem;
    TreeViewItem24: TTreeViewItem;
    TreeViewItem25: TTreeViewItem;
    Splitter4: TSplitter;
    ToolBar2: TToolBar;
    Layout1: TLayout;
    ActnListViewAdd: TAction;
    Button8: TButton;
    ActnClear: TAction;
    Button9: TButton;
    ActnListAddBitmap: TAction;
    Image1: TImage;
    Button10: TButton;
    ActnAddBitmapAndImage: TAction;
    Button11: TButton;
    ListView1: TListView;
    PrototypeBindSource1: TPrototypeBindSource;
    BindingsList1: TBindingsList;
    ListView2: TListView;
    CheckBox2: TCheckBox;
    ActnImages: TAction;
    LinkFillControlToField1: TLinkFillControlToField;
    ActnInfo: TControlAction;
    ListBoxItem5: TListBoxItem;
    ListBoxItem6: TListBoxItem;
    ListBoxItem7: TListBoxItem;
    ListBoxItem8: TListBoxItem;
    ListBoxItem9: TListBoxItem;
    ActnIncIndex: TAction;
    ActnDecIndex: TAction;
    ActnShowCheckBox: TAction;
    CheckBox3: TCheckBox;
    procedure TabItem4Paint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure Glyph1Changed(Sender: TObject);

    procedure ActnUpdateTextExecute(Sender: TObject);
    procedure ActnAddSourceExecute(Sender: TObject);
    procedure ActnNextImageExecute(Sender: TObject);
    procedure ActnDormantExecute(Sender: TObject);
    procedure ActnDormantUpdate(Sender: TObject);
    procedure ActnUpCacheExecute(Sender: TObject);
    procedure ActnUpCacheUpdate(Sender: TObject);
    procedure ActnDownCacheExecute(Sender: TObject);
    procedure ActnDownCacheUpdate(Sender: TObject);
    procedure ActnClearCacheExecute(Sender: TObject);
    procedure ActnClearExecute(Sender: TObject);
    procedure ActnListViewAddExecute(Sender: TObject);
    procedure ActnListAddBitmapExecute(Sender: TObject);
    procedure ActnAddBitmapAndImageExecute(Sender: TObject);
    procedure ActnImagesExecute(Sender: TObject);
    procedure ActnImagesUpdate(Sender: TObject);
    procedure ActnInfoUpdate(Sender: TObject);
    procedure ActnIncIndexExecute(Sender: TObject);
    procedure ActnDecIndexExecute(Sender: TObject);
    procedure ActnShowCheckBoxExecute(Sender: TObject);
    procedure ActnShowCheckBoxUpdate(Sender: TObject);
  private
    { Private declarations }
    FChangeCount: Integer;
    FNumber: Integer;
    FImageLink: TImageLink;
    procedure DrawTextOnLayer(const Index: Integer; const Text: string);
    procedure AddSourceToItem(const Index: Integer);
    procedure OnImagesChange(Sender: TObject);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  FImageLink := TImageLink.Create;
  FImageLink.Images := MainDataModule.ImageList1;
  FImageLink.ImageIndex := Glyph1.ImageIndex;
  FImageLink.OnChange := OnImagesChange;
  Label2.Text := '';
  MenuBar1.Visible := TOSVersion.Platform in [pfWindows, pfMacOS, pfWinRT, pfLinux];
end;

destructor TMainForm.Destroy;
begin
  FImageLink.DisposeOf;
  inherited;
end;

procedure TMainForm.Glyph1Changed(Sender: TObject);
begin
  Inc(FChangeCount);
  Label2.Text := 'Glyph1Changed: ' + FChangeCount.ToString;
end;

procedure TMainForm.TabItem4Paint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
var
  R: TRectF;
begin
  R := TRectF.Create(ARect.Right - 22, 4, ARect.Right - 6, 20);
  MainDataModule.ImageList1.Draw(TabItem4.Canvas, R, FImageLink.ImageIndex);
end;

procedure TMainForm.DrawTextOnLayer(const Index: Integer; const Text: string);
var
  Item: TCustomBitmapItem;
  Size: TSize;
  NewSource: TCustomSourceItem;
  SourceName: string;
  LayerIndex: Integer;
  Layer: TLayer;
begin
  LayerIndex := MainDataModule.ImageList1.Destination[Index].Layers.Count - 1;
  if LayerIndex >= 0 then
  begin
    Layer := MainDataModule.ImageList1.Destination[Index].Layers[LayerIndex];
    SourceName := Layer.Name;
    // Create a new source image if earlier its was not
    if not MainDataModule.ImageList1.BitmapItemByName(SourceName, Item, Size) then
    begin
      Size.cx := Round(Layer.SourceRect.Rect.Width);
      Size.cy := Round(Layer.SourceRect.Rect.Height);
      NewSource := MainDataModule.ImageList1.Source.Add;
      NewSource.Name := SourceName;
      Item := NewSource.MultiResBitmap.ItemByScale(1, False, True);
      if Item = nil then
        Item := NewSource.MultiResBitmap.Add;
      Item.Bitmap.SetSize(Size.cx, Size.cy);
    end;
    // Output some text
    if Item <> nil then
    begin
      Item.Bitmap.Clear(TAlphaColorRec.Null);
      if Item.Bitmap.Canvas.BeginScene then
      try
        Item.Bitmap.Canvas.Font.Size := 15;
        Item.Bitmap.Canvas.Fill.Color := TAlphaColorRec.Red;
        Item.Bitmap.Canvas.Fill.Kind := TBrushKind.Solid;
        Item.Bitmap.Canvas.FillText(TRectF.Create(1, 0, Size.cx - 1, Size.cy div 2), Text, False, 1, [],
          TTextAlign.Center, TTextAlign.Center);
      finally
        Item.Bitmap.Canvas.EndScene;
      end;
    end;
  end;
end;

procedure TMainForm.AddSourceToItem(const Index: Integer);
  procedure DrawPicture(Canvas: TCanvas; R: TRectF; Scale: Single);
  var
    I: Integer;
  begin
    if Canvas.BeginScene then
    try
      Canvas.Font.Size := 6 * Scale;
      Canvas.Fill.Kind := TBrushKind.Solid;
      Canvas.Fill.Color := TAlphaColorRec.Aqua;
      Canvas.Stroke.Color := TAlphaColorRec.Green;
      Canvas.Stroke.Thickness := Scale;
      Canvas.DrawRect(R, Scale, Scale, AllCorners, 1, TCornerType.Bevel);
      Canvas.FillRect(R, Scale, Scale, AllCorners, 1, TCornerType.Bevel);
      Canvas.Stroke.Color := TAlphaColorRec.Black;
      Canvas.Stroke.Thickness := 1;
      for I := Round(R.Bottom / 2 + 2 * Scale) to Round(R.Bottom - 2 * Scale) do
        if I mod 2 = 0 then
          Canvas.DrawLine(TPointF.Create(R.Left + 2 * Scale, I + 0.5),
            TPointF.Create(R.Right - 2 * Scale, I + 0.5), 1);
      Canvas.Fill.Color := TAlphaColorRec.Darkblue;
      Canvas.FillText(R, Scale.ToString, False, 1, [], TTextAlign.Center, TTextAlign.Leading);
    finally
      Canvas.EndScene;
    end;
  end;
var
  Item: TCustomBitmapItem;
  NewSource: TCustomSourceItem;
  SourceName: string;
  Layer: TLayer;
  Size: TSize;
  I: Integer;
  S: Single;
  R: TRectF;
begin
  // Create new layer if need
  if MainDataModule.ImageList1.Destination[Index].Layers.Count = 0 then
  begin
    Layer := MainDataModule.ImageList1.Destination[Index].Layers.Add;
    Layer.SourceRect.Rect := TRectF.Create(0, 0, 16, 16);
    SourceName := '';
  end
  else
  begin
    Layer := MainDataModule.ImageList1.Destination[Index].Layers[0];
    SourceName := Layer.Name;
  end;
  // Create a new source image, if it is not present yet
  if not MainDataModule.ImageList1.BitmapItemByName(SourceName, Item, Size) then
  begin
    NewSource := MainDataModule.ImageList1.Source.Add;
    if SourceName = '' then
      Layer.Name := NewSource.Name
    else
      NewSource.Name := SourceName;
    if NewSource.MultiResBitmap.Count = 0 then
    begin
      // Create several bitmaps for different scales
      for I := 1 to 10 do
      begin
        Item := NewSource.MultiResBitmap.Add;
        S := Item.Scale;
        Item.Bitmap.BitmapScale := 1;
        Size.cx := Round(Layer.SourceRect.Width * S);
        Size.cy := Round(Layer.SourceRect.Height * S);
        Item.Bitmap.SetSize(Size.cx, Size.cy);
        Item.Bitmap.Clear(TAlphaColorRec.Null);
        R := TRectF.Create(S + 0.5, S + 0.5, Size.Width - S - 0.5, Size.Height - S - 0.5);
        DrawPicture(Item.Bitmap.Canvas, R, S);
      end;
    end;
  end;
end;

procedure TMainForm.OnImagesChange(Sender: TObject);
begin
  TabItem4.Repaint;
end;

procedure TMainForm.ActnUpdateTextExecute(Sender: TObject);
begin
  Inc(FNumber);
  DrawTextOnLayer(8, FNumber.ToString);
end;

procedure TMainForm.ActnAddSourceExecute(Sender: TObject);
begin
  AddSourceToItem(9);
  ActnAddSource.Enabled := False;
end;

procedure TMainForm.ActnNextImageExecute(Sender: TObject);
var
  Actn: TCustomAction;
  Images: TCustomImageList;
begin
  if Sender is TCustomAction then
  begin
    Actn := TCustomAction(Sender);
    if (Actn.ActionList <> nil) and (Actn.ActionList.Images is TCustomImageList) then
    begin
      Images := TCustomImageList(Actn.ActionList.Images);
      if Actn.ImageIndex < Images.Count - 1 then
        Actn.ImageIndex := Actn.ImageIndex + 1
      else
        Actn.ImageIndex := -1;
    end;
  end;
end;

procedure TMainForm.ActnDormantExecute(Sender: TObject);
begin
  MainDataModule.ImageList1.Dormant := not MainDataModule.ImageList1.Dormant;
end;

procedure TMainForm.ActnDormantUpdate(Sender: TObject);
begin
  if Sender is TCustomAction then
    TCustomAction(Sender).Checked := MainDataModule.ImageList1.Dormant;
end;

procedure TMainForm.ActnUpCacheExecute(Sender: TObject);
begin
  MainDataModule.ImageList1.CacheSize := MainDataModule.ImageList1.CacheSize + 1;
end;

procedure TMainForm.ActnUpCacheUpdate(Sender: TObject);
begin
  if Sender is TCustomAction then
    TCustomAction(Sender).Text := Format('Up Cache Size (%d)', [MainDataModule.ImageList1.CacheSize]);
end;

procedure TMainForm.ActnDownCacheExecute(Sender: TObject);
begin
  MainDataModule.ImageList1.CacheSize := MainDataModule.ImageList1.CacheSize - 1;
end;

procedure TMainForm.ActnDownCacheUpdate(Sender: TObject);
begin
  if Sender is TCustomAction then
  begin
    TCustomAction(Sender).Enabled := MainDataModule.ImageList1.CacheSize > 1;
    TCustomAction(Sender).Text := Format('Down Cache Size (%d)', [MainDataModule.ImageList1.CacheSize]);
  end;
end;

procedure TMainForm.ActnClearCacheExecute(Sender: TObject);
begin
  MainDataModule.ImageList1.ClearCache;
end;

procedure TMainForm.ActnClearExecute(Sender: TObject);
begin
  ListView2.Items.Clear;
end;

procedure TMainForm.ActnListViewAddExecute(Sender: TObject);
var
  Item: TListViewItem;
begin
  if MainDataModule.ImageList1.Count > 0 then
  begin
    ListView2.ItemAppearanceName := TAppearanceNames.ImageListItem;
    ListView2.ItemAppearanceObjects.ItemObjects.Image.Visible := True;
    Item := ListView2.Items.Add;
    if (Item.Objects.ImageObject <> nil) then
    begin
      Item.Objects.ImageObject.ImageIndex := Item.Index mod MainDataModule.ImageList1.Count;
      Item.Text := Format('Item with ImageIndex = %d', [Item.Objects.ImageObject.ImageIndex]);
    end;
  end;
end;

procedure TMainForm.ActnListAddBitmapExecute(Sender: TObject);
var
  Item: TListViewItem;
begin
  if not Image1.Bitmap.IsEmpty then
  begin
    ListView2.ItemAppearanceName := TAppearanceNames.ImageListItem;
    ListView2.ItemAppearanceObjects.ItemObjects.Image.Visible := True;
    Item := ListView2.Items.Add;
    if Item.Objects.ImageObject <> nil then
    begin
      Item.Bitmap := Image1.Bitmap;
      Item.Text := Format('Item with Bitmap ''%s''', [Image1.Name]);
    end;
  end;
end;

procedure TMainForm.ActnAddBitmapAndImageExecute(Sender: TObject);
var
  Item: TListViewItem;
begin
  if (MainDataModule.ImageList1.Count > 0) and not Image1.Bitmap.IsEmpty then
  begin
    ListView2.ItemAppearanceName := TAppearanceNames.ImageListItem;
    ListView2.ItemAppearanceObjects.ItemObjects.Image.Visible := True;
    Item := ListView2.Items.Add;
    if (Item.Objects.ImageObject <> nil) then
    begin
      Item.Bitmap := Image1.Bitmap;
      Item.Objects.ImageObject.ImageIndex := Item.Index mod MainDataModule.ImageList1.Count;
      Item.Text := Format('Item with ImageIndex = %d and Bitmap ''%s''', [Item.Objects.ImageObject.ImageIndex,
        Image1.Name]);
    end;
  end;
end;

procedure TMainForm.ActnImagesExecute(Sender: TObject);
begin
  if ListView2.Images <> nil then
    ListView2.Images := nil
  else
    ListView2.Images := MainDataModule.ImageList1;
end;

procedure TMainForm.ActnImagesUpdate(Sender: TObject);
begin
  if Sender is TCustomAction then
  begin
    TCustomAction(Sender).Checked := ListView2.Images <> nil;
  end;
end;

procedure TMainForm.ActnInfoUpdate(Sender: TObject);
var
  S: string;
begin
  if Sender is TCustomAction then
  begin
    S := 'Images: ';
    if Glyph1.Images <> nil then
    begin
      S := Format('Images: ''%s''; Count: %d', [Glyph1.Images.Name, Glyph1.Images.Count]);
      if Glyph1.Images.Dormant then
        S := S + '; Dormant';
    end
    else
      S := 'Images: nil';
    S := S + sLineBreak + Format('ImageIndex: %d; Width: %2.0f; Height: %2.0f', [Glyph1.ImageIndex, Glyph1.Width,
      Glyph1.Height]);
    if Glyph1.Visible then
      S := S + '; Visible';
    TCustomAction(Sender).Text := S;
  end;
end;

procedure TMainForm.ActnIncIndexExecute(Sender: TObject);
begin
  if Glyph1.Images <> nil then
  begin
    if Glyph1.ImageIndex < Glyph1.Images.Count - 1 then
      Glyph1.ImageIndex := Glyph1.ImageIndex + 1
    else
      Glyph1.ImageIndex := -1;
    FImageLink.ImageIndex := Glyph1.ImageIndex;
  end;
end;

procedure TMainForm.ActnDecIndexExecute(Sender: TObject);
begin
  if Glyph1.Images <> nil then
  begin
    if Glyph1.ImageIndex >= 0 then
      Glyph1.ImageIndex := Glyph1.ImageIndex - 1
    else
      Glyph1.ImageIndex := Glyph1.Images.Count - 1;
    FImageLink.ImageIndex := Glyph1.ImageIndex;
  end;
end;

procedure TMainForm.ActnShowCheckBoxExecute(Sender: TObject);
begin
  TreeView1.ShowCheckboxes := not TreeView1.ShowCheckboxes;
end;

procedure TMainForm.ActnShowCheckBoxUpdate(Sender: TObject);
begin
  if Sender is TCustomAction then
    TCustomAction(Sender).Checked := TreeView1.ShowCheckboxes;
end;

end.
