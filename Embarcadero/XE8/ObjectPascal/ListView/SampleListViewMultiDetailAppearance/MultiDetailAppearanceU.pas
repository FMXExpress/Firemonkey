//---------------------------------------------------------------------------

// This software is Copyright (c) 2013 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

// Implements a TListView appearance
// Install this appearance using a design time package.
// Once installed, the "MultiDetailItem" appearance can be used with a TListView component

unit MultiDetailAppearanceU;

interface

uses FMX.ListView, FMX.ListView.Types, System.Classes, System.SysUtils,
FMX.Types, System.UITypes, FMX.MobilePreview;

type

  TMultiDetailAppearanceNames = class
  public const
    ListItem = 'MultiDetailItem';
    ListItemCheck = ListItem + 'ShowCheck';
    ListItemDelete = ListItem + 'Delete';
    Detail1 = 'det1';  // Name of MultiDetail object/data
    Detail2 = 'det2';
    Detail3 = 'det3';
  end;

implementation

uses System.Math, System.Rtti;

type

  TMultiDetailItemAppearance = class(TPresetItemObjects)
  public const
    cTextMarginAccessory = 8;
    cDefaultImagePlaceOffsetX = -3;
    cDefaultImageTextPlaceOffsetX = 4;
    cDefaultHeight = 80;
    cDefaultImageWidth = 65;
    cDefaultImageHeight = 65;
  private
    FMultiDetail1: TTextObjectAppearance;
    FMultiDetail2: TTextObjectAppearance;
    FMultiDetail3: TTextObjectAppearance;
    procedure SetMultiDetail1(const Value: TTextObjectAppearance);
    procedure SetMultiDetail2(const Value: TTextObjectAppearance);
    procedure SetMultiDetail3(const Value: TTextObjectAppearance);
  protected
    function DefaultHeight: Integer; override;
    procedure UpdateSizes; override;
    function GetGroupClass: TPresetItemObjects.TGroupClass; override;
    procedure SetObjectData(const AListViewItem: TListViewItem; const AIndex: string; const AValue: TValue; var AHandled: Boolean); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property Image;
    property MultiDetail1: TTextObjectAppearance read FMultiDetail1 write SetMultiDetail1;
    property MultiDetail2: TTextObjectAppearance read FMultiDetail2 write SetMultiDetail2;
    property MultiDetail3: TTextObjectAppearance read FMultiDetail3 write SetMultiDetail3;
    property Accessory;
  end;

  TMultiDetailDeleteAppearance = class(TMultiDetailItemAppearance)
  private const
    cDefaultGlyph = TGlyphButtonType.Delete;
  public
    constructor Create; override;
  published
    property GlyphButton;
  end;

  TMultiDetailShowCheckAppearance = class(TMultiDetailItemAppearance)
  private const
    cDefaultGlyph = TGlyphButtonType.Checkbox;
  public
    constructor Create; override;
  published
    property GlyphButton;
  end;


const
  cMultiDetail1Member = 'Detail1';
  cMultiDetail2Member = 'Detail2';
  cMultiDetail3Member = 'Detail3';


constructor TMultiDetailItemAppearance.Create;
begin
  inherited;
  Accessory.DefaultValues.AccessoryType := TAccessoryType.More;
  Accessory.DefaultValues.Visible := True;
  Accessory.RestoreDefaults;
  Text.DefaultValues.VertAlign := TListItemAlign.Trailing;
  Text.DefaultValues.TextVertAlign := TTextAlign.Leading;
  Text.DefaultValues.Height := 76;  // Item will be bottom aligned, with text top aligned
  Text.DefaultValues.Visible := True;
  Text.RestoreDefaults;

  FMultiDetail1 := TTextObjectAppearance.Create;
  FMultiDetail1.Name := TMultiDetailAppearanceNames.Detail1;
  FMultiDetail1.DefaultValues.Assign(Text.DefaultValues);  // Start with same defaults as Text object
  FMultiDetail1.DefaultValues.Height := 56;  // Move text down
  FMultiDetail1.DefaultValues.IsDetailText := True; // Use detail font
  FMultiDetail1.RestoreDefaults;
  FMultiDetail1.OnChange := Self.ItemPropertyChange;
  FMultiDetail1.Owner := Self;

  FMultiDetail2 := TTextObjectAppearance.Create;
  FMultiDetail2.Name := TMultiDetailAppearanceNames.Detail2;
  FMultiDetail2.DefaultValues.Assign(FMultiDetail1.DefaultValues);  // Start with same defaults as Text object
  FMultiDetail2.DefaultValues.Height := 38; // Move text down
  FMultiDetail2.RestoreDefaults;
  FMultiDetail2.OnChange := Self.ItemPropertyChange;
  FMultiDetail2.Owner := Self;

  FMultiDetail3 := TTextObjectAppearance.Create;
  FMultiDetail3.Name := TMultiDetailAppearanceNames.Detail3;
  FMultiDetail3.DefaultValues.Assign(FMultiDetail2.DefaultValues);  // Start with same defaults as Text object
  FMultiDetail3.DefaultValues.Height := 20; // Move text down
  FMultiDetail3.RestoreDefaults;
  FMultiDetail3.OnChange := Self.ItemPropertyChange;
  FMultiDetail3.Owner := Self;

  // Define livebindings members that make up MultiDetail
  FMultiDetail1.DataMembers :=
    TObjectAppearance.TDataMembers.Create(
      TObjectAppearance.TDataMember.Create(
        cMultiDetail1Member, // Displayed by LiveBindings
        Format('Data["%s"]', [TMultiDetailAppearanceNames.Detail1])));   // Expression to access value from TListViewItem
  FMultiDetail2.DataMembers :=
    TObjectAppearance.TDataMembers.Create(
      TObjectAppearance.TDataMember.Create(
        cMultiDetail2Member, // Displayed by LiveBindings
        Format('Data["%s"]', [TMultiDetailAppearanceNames.Detail2])));   // Expression to access value from TListViewItem
  FMultiDetail3.DataMembers :=
    TObjectAppearance.TDataMembers.Create(
      TObjectAppearance.TDataMember.Create(
        cMultiDetail3Member, // Displayed by LiveBindings
        Format('Data["%s"]', [TMultiDetailAppearanceNames.Detail3])));   // Expression to access value from TListViewItem

  Image.DefaultValues.Width := cDefaultImageWidth;
  Image.DefaultValues.Height := cDefaultImageHeight;
  Image.RestoreDefaults;

  GlyphButton.DefaultValues.VertAlign := TListItemAlign.Center;
  GlyphButton.RestoreDefaults;

  // Define the appearance objects
  AddObject(Text, True);
  AddObject(MultiDetail1, True);
  AddObject(MultiDetail2, True);
  AddObject(MultiDetail3, True);
  AddObject(Image, True);
  AddObject(Accessory, True);
  AddObject(GlyphButton, IsItemEdit);  // GlyphButton is only visible when in edit mode
end;

constructor TMultiDetailDeleteAppearance.Create;
begin
  inherited;
  GlyphButton.DefaultValues.ButtonType := cDefaultGlyph;
  GlyphButton.DefaultValues.Visible := True;
  GlyphButton.RestoreDefaults;
end;

constructor TMultiDetailShowCheckAppearance.Create;
begin
  inherited;
  GlyphButton.DefaultValues.ButtonType := cDefaultGlyph;
  GlyphButton.DefaultValues.Visible := True;
  GlyphButton.RestoreDefaults;
end;

function TMultiDetailItemAppearance.DefaultHeight: Integer;
begin
  Result := cDefaultHeight;
end;

destructor TMultiDetailItemAppearance.Destroy;
begin
  FMultiDetail1.Free;
  FMultiDetail2.Free;
  FMultiDetail3.Free;
  inherited;
end;

procedure TMultiDetailItemAppearance.SetMultiDetail1(
  const Value: TTextObjectAppearance);
begin
  FMultiDetail1.Assign(Value);
end;

procedure TMultiDetailItemAppearance.SetMultiDetail2(
  const Value: TTextObjectAppearance);
begin
  FMultiDetail2.Assign(Value);
end;

procedure TMultiDetailItemAppearance.SetMultiDetail3(
  const Value: TTextObjectAppearance);
begin
  FMultiDetail3.Assign(Value);
end;

procedure TMultiDetailItemAppearance.SetObjectData(
  const AListViewItem: TListViewItem; const AIndex: string;
  const AValue: TValue; var AHandled: Boolean);
begin
  inherited;

end;

function TMultiDetailItemAppearance.GetGroupClass: TPresetItemObjects.TGroupClass;
begin
  Result := TMultiDetailItemAppearance;
end;

procedure TMultiDetailItemAppearance.UpdateSizes;
var
  LOuterHeight: Single;
  LOuterWidth: Single;
  LInternalWidth: Single;
  LImagePlaceOffset: Single;
  LImageTextPlaceOffset: Single;
begin
  BeginUpdate;
  try
    inherited;

    // Update the widths and positions of renderening objects within a TListViewItem
    LOuterHeight := Height - Owner.ItemSpaces.Top - Owner.ItemSpaces.Bottom;
    LOuterWidth := Owner.Width - Owner.ItemSpaces.Left - Owner.ItemSpaces.Right;
    if Image.ActualWidth = 0 then
    begin
      LImagePlaceOffset := 0;
      LImageTextPlaceOffset := 0;
    end
    else
    begin
      LImagePlaceOffset := cDefaultImagePlaceOffsetX;
      LImageTextPlaceOffset := cDefaultImageTextPlaceOffsetX;
    end;
    Image.InternalPlaceOffset.X := GlyphButton.ActualWidth + LImagePlaceOffset;
    if Image.ActualWidth > 0 then
      Text.InternalPlaceOffset.X :=
        Image.ActualPlaceOffset.X +  Image.ActualWidth + LImageTextPlaceOffset
    else
      Text.InternalPlaceOffset.X :=
        0 + GlyphButton.ActualWidth;
    MultiDetail1.InternalPlaceOffset.X := Text.InternalPlaceOffset.X;
    MultiDetail2.InternalPlaceOffset.X := Text.InternalPlaceOffset.X;
    MultiDetail3.InternalPlaceOffset.X := Text.InternalPlaceOffset.X;
    LInternalWidth := LOuterWidth - Text.ActualPlaceOffset.X - Accessory.ActualWidth;
    if Accessory.ActualWidth > 0 then
      LInternalWidth := LInternalWidth - cTextMarginAccessory;
    Text.InternalWidth := Max(1, LInternalWidth);
    MultiDetail1.InternalWidth := Text.InternalWidth;
    MultiDetail2.InternalWidth := Text.InternalWidth;
    MultiDetail3.InternalWidth := Text.InternalWidth;
  finally
    EndUpdate;
  end;

end;

type
  TOption = TCustomListView.TRegisterAppearanceOption;
const
  sThisUnit = 'MultiDetailAppearanceU';     // Will be added to the uses list when appearance is used
initialization
  // MultiDetailItem group
  TCustomListView.RegisterAppearance(
    TMultiDetailItemAppearance, TMultiDetailAppearanceNames.ListItem,
    [TOption.Item], sThisUnit);
  TCustomListView.RegisterAppearance(
    TMultiDetailDeleteAppearance, TMultiDetailAppearanceNames.ListItemDelete,
    [TOption.ItemEdit], sThisUnit);
  TCustomListView.RegisterAppearance(
    TMultiDetailShowCheckAppearance, TMultiDetailAppearanceNames.ListItemCheck,
    [TOption.ItemEdit], sThisUnit);
finalization
  TCustomListView.UnregisterAppearances(
    TArray<TCustomListView.TItemAppearanceObjectsClass>.Create(
      TMultiDetailItemAppearance, TMultiDetailDeleteAppearance,
      TMultiDetailShowCheckAppearance));
end.
