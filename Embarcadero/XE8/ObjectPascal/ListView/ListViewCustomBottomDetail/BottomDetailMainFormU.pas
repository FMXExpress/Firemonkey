//---------------------------------------------------------------------------

// This software is Copyright (c) 2013 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

// This projects modifies the "Custom" appearance to
// dispay image with text and bottom detail.
// The properties of the custom appearance item and itemedit have been
// configures in the designer.  This sample also shows how to set the
// appearance properties in code.

unit BottomDetailMainFormU;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.ListView.Types,
  FMX.StdCtrls, FMX.ListView, Data.Bind.GenData,
  Fmx.Bind.GenData, System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors,
  Data.Bind.EngExt, Fmx.Bind.DBEngExt, Data.Bind.Components,
  Data.Bind.ObjectScope, FMX.ListBox,
  FMX.TabControl, FMX.Objects, RatingsAppearanceU, FMX.MobilePreview;

type
  TForm594 = class(TForm)
    ToolBar1: TToolBar;
    ToggleEditMode: TSpeedButton;
    PrototypeBindSource1: TPrototypeBindSource;
    BindingsList1: TBindingsList;
    ListViewBottomDetail: TListView;
    ImageRatings: TImage;
    LinkFillControlToField1: TLinkFillControlToField;
    SpeedButtonLiveBindings: TSpeedButton;
    ToolBar2: TToolBar;
    SpeedButtonFill: TSpeedButton;
    ImageRAD: TImage;
    SpeedButtonSetProps: TSpeedButton;
    procedure ToggleEditModeClick(Sender: TObject);
    procedure SpeedButtonFillClick(Sender: TObject);
    procedure SpeedButtonLiveBindingsClick(Sender: TObject);
    procedure SpeedButtonSetPropsClick(Sender: TObject);
    procedure ListViewBottomDetailUpdateObjects(const Sender: TObject;
      const AItem: TListViewItem);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form594: TForm594;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

uses System.Math;

procedure TForm594.ListViewBottomDetailUpdateObjects(const Sender: TObject;
  const AItem: TListViewItem);
var
  LOuterWidth: Single;
begin
  LOuterWidth := AItem.Parent.Width - AItem.Parent.ItemSpaces.Left - AItem.Parent.ItemSpaces.Right - 8;
  if AItem.Objects.AccessoryObject.Visible then
    LOuterWidth := LOuterWidth - AItem.Objects.AccessoryObject.Width;
  // Adjust elements to width of listview so that text truncation works
  AItem.Objects.TextObject.Width :=
    Max(1.0, LOuterWidth - AItem.Objects.TextObject.PlaceOffset.X);
  AItem.Objects.DetailObject.Width := AItem.Objects.TextObject.Width;

end;

procedure TForm594.SpeedButtonFillClick(Sender: TObject);
var
  I: Integer;
  LItem: TListViewItem;
begin
  LinkFillControlToField1.Active := False;
  // Code to fill TListView
  for I := 1 to 20 do
  begin
    LItem := ListViewBottomDetail.Items.Add;
    LItem.Text := Format('Text %d', [I]);
    LItem.Detail := Format('Detail %d', [I]);
    LItem.BitmapRef := ImageRAD.Bitmap;
  end;

end;

procedure TForm594.SpeedButtonLiveBindingsClick(Sender: TObject);
begin
  LinkFillControlToField1.Active := True;
end;

// Set appearance properties in code.  This is an alternative to setting properties in the object inspector.
// Clicking the button will have no affect, if the design time properties match.
procedure TForm594.SpeedButtonSetPropsClick(Sender: TObject);
const
  cItemHeight = 80;
  cTextHeight = 70;
  cDetailHeight = 30;
  cGlyphWidth = 30;
  cImageSize = 70;
  cTextOffset = cImageSize + 5;
  cEditImageOffset = cGlyphWidth;
  cEditTextOffset = cGlyphWidth + cImageSize + 5;
  procedure SetTextProperties(const AText: TTextObjectAppearance);
  begin
    AText.RestoreDefaults; // Restore to defaults for custom
    AText.Height := cTextHeight;
    AText.VertAlign := TListItemAlign.Trailing;
    AText.TextVertAlign := TTextAlign.Leading;
    AText.PlaceOffset.X := cTextOffset;
  end;
  procedure SetDetailProperties(const ADetail: TTextObjectAppearance);
  begin
    ADetail.RestoreDefaults; // Restore to defaults for custom
    ADetail.Height := cDetailHeight;
    ADetail.VertAlign := TListItemAlign.Trailing;
    ADetail.TextVertAlign := TTextAlign.Leading;
    ADetail.PlaceOffset.X := cTextOffset;
    ADetail.Visible := True;
  end;
  procedure SetImageProperties(const AImage: TImageObjectAppearance);
  begin
    AImage.RestoreDefaults; // Restore to defaults for custom
    AImage.Height := cImageSize;
    AImage.Width := cImageSize;
    AImage.VertAlign := TListItemAlign.Center;
    AImage.Visible := True;
  end;
  procedure SetCommonProperties(const AObjects: TItemAppearanceObjects);
  begin
    SetTextProperties(AObjects.Detail);
    SetDetailProperties(AObjects.Detail);
    SetImageProperties(AObjects.Image);
  end;
  procedure SetEditItemProperties(const AObjects: TItemAppearanceObjects);
  begin
    AObjects.Image.PlaceOffset.X := cEditImageOffset;
    AObjects.Text.PlaceOffset.X := cEditTextOffset;
    AObjects.Detail.PlaceOffset.X := cEditTextOffset;
    AObjects.GlyphButton.Visible := True;
  end;
var
  LObjects: TItemAppearanceObjects;
begin
  ListViewBottomDetail.BeginUpdate;
  try
    ListViewBottomDetail.ItemAppearance.ItemHeight := cItemHeight;
    ListViewBottomDetail.ItemAppearance.ItemEditHeight := cItemHeight;
    // Set Item properties
    LObjects := ListViewBottomDetail.ItemAppearanceObjects.ItemObjects;
    LObjects.BeginUpdate;
    try
      SetCommonProperties(LObjects);
    finally
      LObjects.EndUpdate;
    end;
    // Set Edit Item properties
    LObjects := ListViewBottomDetail.ItemAppearanceObjects.ItemEditObjects;
    LObjects.BeginUpdate;
    try
      SetCommonProperties(LObjects);
      SetEditItemProperties(LObjects);
    finally
      LObjects.EndUpdate;
    end;
  finally
    ListViewBottomDetail.EndUpdate;
  end;

end;

procedure TForm594.ToggleEditModeClick(Sender: TObject);
begin
  ListViewBottomDetail.EditMode := not ListViewBottomDetail.EditMode;
end;

end.
