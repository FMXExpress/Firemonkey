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
// Once installed, the "RatingsListItem" appearance can be used with a TListView component
unit RatingsAppearanceU;

interface

uses FMX.ListView, FMX.ListView.Types, System.Classes, System.SysUtils,
FMX.Types, System.UITypes, FMX.Objects, FMX.MobilePreview;

type

  TRatingsListItemAppearanceNames = class
  public const
    ListItem = 'RatingListItem';
    ListItemCheck = ListItem + 'ShowCheck';
    ListItemDelete = ListItem + 'Delete';
    Text2Name = 'Text2';  // Use with TListViewItem.Objects.FindObject
    Text3Name = 'Text3';  // Use with TListViewItem.Objects.FindObject
    RatingsImageName = 'RImage';  // Use with TListViewItem.Objects.FindObject
  end;

  TRatingsImageObjectAppearance = class(TImageObjectAppearance)
  private type
    TNotify = class(TComponent)
    private
      FOwner: TRatingsImageObjectAppearance;
    protected
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    end;
  private
    FRatingsImage: TImage;
    FNotify: TNotify;
    FRatingsCount: Integer;
    procedure SetRatingsImage(const Value: TImage);
    procedure SetRatingsCount(const Value: Integer);
  public
    destructor Destroy; override;
    procedure CreateObject(const AListViewItem: TListViewItem); override;
    procedure ResetObject(const AListViewItem: TListViewItem); override;
  published
    // Image for ratings
    // Note that the following two properties must be set for both Item and EditItem appearances so that the
    // images are the same when transitioning between edit mode
    property RatingsImage: TImage read FRatingsImage write SetRatingsImage;
    // Number of ratings (the image will be divided into this number of sub image rows)
    property RatingsCount: Integer read FRatingsCount write SetRatingsCount;
  end;


implementation

uses System.Math, System.Rtti, System.Types;

type

  TRatingsListItemAppearance = class(TPresetItemObjects)
  public const
    cDefaultHeight = 80;
  private
    FText2: TTextObjectAppearance;
    FRatingsImage: TRatingsImageObjectAppearance;
    FText3: TTextObjectAppearance;
    procedure SetText2(const Value: TTextObjectAppearance);
    procedure SetRatingsImage(const Value: TRatingsImageObjectAppearance);
    procedure SetText3(const Value: TTextObjectAppearance);
    procedure UpdateImageSrcRect(const AListViewItem: TListViewItem);
  protected
    function DefaultHeight: Integer; override;
    procedure UpdateSizes; override;
    function GetGroupClass: TPresetItemObjects.TGroupClass; override;
    procedure SetObjectData(const AListViewItem: TListViewItem; const AIndex: string; const AValue: TValue; var AHandled: Boolean); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property Text;
    property Image;
    property Text2: TTextObjectAppearance read FText2 write SetText2;
    property Text3: TTextObjectAppearance read FText3 write SetText3;
    property RatingsImage: TRatingsImageObjectAppearance read FRatingsImage write SetRatingsImage;
    property Accessory;
  end;

  TRatingsListItemDeleteAppearance = class(TRatingsListItemAppearance)
  private const
    cDefaultGlyph = TGlyphButtonType.Delete;
  public
    constructor Create; override;
  published
    property GlyphButton;
  end;

  TRatingsListItemShowCheckAppearance = class(TRatingsListItemAppearance)
  private const
    cDefaultGlyph = TGlyphButtonType.Checkbox;
  public
    constructor Create; override;
  published
    property GlyphButton;
  end;


const
  cText2MemberName = 'Text2';
  cText3MemberName = 'Text3';
  cRatingMemberName = 'Rating';

constructor TRatingsListItemAppearance.Create;
begin
  inherited;
  Accessory.DefaultValues.AccessoryType := TAccessoryType.More;
  Accessory.DefaultValues.Visible := True;
  Accessory.RestoreDefaults;

  Text.DefaultValues.Visible := True;
  Text.DefaultValues.VertAlign := TListItemAlign.Trailing;
  Text.DefaultValues.TextVertAlign := TTextAlign.Leading;
  Text.DefaultValues.Height := 74;
  Text.RestoreDefaults;


  FText2 := TTextObjectAppearance.Create;
  FText2.Name := TRatingsListItemAppearanceNames.Text2Name;
  FText2.DefaultValues.Assign(Text.DefaultValues);
  FText2.DefaultValues.IsDetailText := True;
  FText2.DefaultValues.Height := 56;
  FText2.RestoreDefaults;
  FText2.OnChange := Self.ItemPropertyChange;
  FText2.Owner := Self;

  Text2.DataMembers :=
    TObjectAppearance.TDataMembers.Create(
      TObjectAppearance.TDataMember.Create(
        cText2MemberName, // Displayed by LiveBindings
        Format('Data["%s"]', [TRatingsListItemAppearanceNames.Text2Name])));   // Expression to access value from TListViewItem

  FText3 := TTextObjectAppearance.Create;
  FText3.Name := TRatingsListItemAppearanceNames.Text3Name;
  FText3.DefaultValues.Assign(Text2.DefaultValues);
  FText3.DefaultValues.Height := 42;
  FText3.RestoreDefaults;
  FText3.OnChange := Self.ItemPropertyChange;
  FText3.Owner := Self;

  Text3.DataMembers :=
    TObjectAppearance.TDataMembers.Create(
      TObjectAppearance.TDataMember.Create(
        cText3MemberName, // Displayed by LiveBindings
        Format('Data["%s"]', [TRatingsListItemAppearanceNames.Text3Name])));   // Expression to access value from TListViewItem

  Image.DefaultValues.VertAlign := TListItemAlign.Center;
  Image.DefaultValues.Height := 70;
  Image.DefaultValues.Width := 70;
  Image.DefaultValues.Visible := True;
  Image.RestoreDefaults;

  FRatingsImage := TRatingsImageObjectAppearance.Create;
  FRatingsImage.Name := TRatingsListItemAppearanceNames.RatingsImageName;
  FRatingsImage.DefaultValues.Visible := True;
  FRatingsImage.DefaultValues.VertAlign := TListItemAlign.Trailing;
  FRatingsImage.DefaultValues.Height := 25;
  FRatingsImage.RestoreDefaults;
  FRatingsImage.OnChange := Self.ItemPropertyChange;
  FRatingsImage.Owner := Self;

  FRatingsImage.DataMembers :=
    TObjectAppearance.TDataMembers.Create(
      TObjectAppearance.TDataMember.Create(
        cRatingMemberName, // Displayed by LiveBindings
        Format('Data["%s"]', [TRatingsListItemAppearanceNames.RatingsImageName])));   // Expression to access value from TListViewItem

  AddObject(Text, True);
  AddObject(Text2, True);
  AddObject(Text3, True);
  AddObject(Image, True);
  AddObject(RatingsImage, True);
  AddObject(Accessory, True);
  AddObject(GlyphButton, IsItemEdit);
end;

constructor TRatingsListItemDeleteAppearance.Create;
begin
  inherited;
  GlyphButton.DefaultValues.ButtonType := cDefaultGlyph;
  GlyphButton.DefaultValues.Visible := True;
  GlyphButton.RestoreDefaults;
end;

constructor TRatingsListItemShowCheckAppearance.Create;
begin
  inherited;
  GlyphButton.DefaultValues.ButtonType := cDefaultGlyph;
  GlyphButton.DefaultValues.Visible := True;
  GlyphButton.RestoreDefaults;
end;


function TRatingsListItemAppearance.DefaultHeight: Integer;
begin
  Result := cDefaultHeight;
end;

destructor TRatingsListItemAppearance.Destroy;
begin
  FText2.Free;
  FText3.Free;
  FRatingsImage.Free;
  inherited;
end;

procedure TRatingsListItemAppearance.SetText2(
  const Value: TTextObjectAppearance);
begin
  FText2.Assign(Value);
end;

procedure TRatingsListItemAppearance.SetText3(
  const Value: TTextObjectAppearance);
begin
  FText3.Assign(Value);
end;

procedure TRatingsListItemAppearance.SetObjectData(
  const AListViewItem: TListViewItem; const AIndex: string;
  const AValue: TValue; var AHandled: Boolean);
begin
  if AIndex = TRatingsListItemAppearanceNames.RatingsImageName then
  begin
    AHandled := True;
    UpdateImageSrcRect(AListViewItem);
  end;
end;

procedure TRatingsListItemAppearance.UpdateImageSrcRect(
  const AListViewItem: TListViewItem);
var
  LObject: TListItemImage;
  LValue: TValue;
  LRating: Cardinal;
  S: string;
  I: Integer;
  C: Cardinal;
  B: Boolean;
  LImageHeight: Single;
  LImageRowHeight: Single;
begin
  LObject := AListViewItem.Objects.FindObject(TRatingsListItemAppearanceNames.RatingsImageName) as TListItemImage;
  Assert(LObject <> nil);
  LValue := AListViewItem.Data[TRatingsListItemAppearanceNames.RatingsImageName];
  LRating := 0;
  if not LValue.IsEmpty then
  begin
    if LValue.TryAsType<Cardinal>(C) then
    begin
      LRating := C;
    end
    else if LValue.TryAsType<Boolean>(B) then
    begin
      if B then
        LRating := 1
      else
        LRating := 0;
    end
    else
    begin
      S := LValue.ToString;
      if TryStrToInt(S, I) then
        LRating := Abs(I);
    end;
  end;
  LRating := Min(LRating, RatingsImage.RatingsCount - 1);
  if (RatingsImage.RatingsCount > 0) and (RatingsImage.RatingsImage <> nil) and (RatingsImage.RatingsImage.Bitmap <> nil) then
  begin
    LImageHeight := RatingsImage.RatingsImage.Bitmap.Height;
    LImageRowHeight := LImageHeight / RatingsImage.RatingsCount;
    LObject.SrcRect := TRectF.Create(0, LImageRowHeight * LRating, 0, LImageRowHeight * (LRating + 1));
  end;
end;

procedure TRatingsListItemAppearance.SetRatingsImage(
  const Value: TRatingsImageObjectAppearance);
begin
  FRatingsImage.Assign(Value);
end;

function TRatingsListItemAppearance.GetGroupClass: TPresetItemObjects.TGroupClass;
begin
  Result := TRatingsListItemAppearance;
end;

procedure TRatingsListItemAppearance.UpdateSizes;
var
  LOuterWidth: Single;
  LOuterHeight: Single;
  LTextOffset: Single;
begin
  BeginUpdate;
  try
    inherited;
    Assert(Owner <> nil);
    if Owner <> nil then
    begin
      LOuterHeight := Height - Owner.ItemSpaces.Top - Owner.ItemSpaces.Bottom;
      LOuterWidth := Owner.Width - Owner.ItemSpaces.Left - Owner.ItemSpaces.Right;
      Image.InternalPlaceOffset.X := GlyphButton.ActualWidth;
      LTextOffset := GlyphButton.ActualWidth + Image.ActualWidth;
      if Image.ActualWidth > 0 then
        LTextOffset := LTextOffset + 5;
      Text.InternalPlaceOffset.X := LTextOffset;
      Text2.InternalPlaceOffset.X := LTextOffset;
      Text3.InternalPlaceOffset.X := LTextOffset;
      RatingsImage.InternalPlaceOffset.X := LTextOffset;
      RatingsImage.InternalWidth := LOuterWidth;
    end;
  finally
    EndUpdate;
  end;

end;

  { TRatingsImageObjectAppearance }

procedure TRatingsImageObjectAppearance.CreateObject(
  const AListViewItem: TListViewItem);
var
  LObject: TListItemImage;
begin
  inherited;
  LObject := AListViewItem.Objects.FindObject(TRatingsListItemAppearanceNames.RatingsImageName) as TListItemImage;
  if Assigned(Self.RatingsImage) then
  begin
    LObject.Bitmap := Self.RatingsImage.Bitmap;
    (Owner as TRatingsListItemAppearance).UpdateImageSrcRect(AListViewItem);
  end;
end;

destructor TRatingsImageObjectAppearance.Destroy;
begin
  FNotify.Free;
  inherited;
end;

procedure TRatingsImageObjectAppearance.ResetObject(
  const AListViewItem: TListViewItem);
begin
  inherited;
  (Owner as TRatingsListItemAppearance).UpdateImageSrcRect(AListViewItem);
end;

procedure TRatingsImageObjectAppearance.SetRatingsImage(const Value: TImage);
var
  LListView: TCustomListView;
  LItem: TListViewItem;
  LObject: TListItemImage;
begin

  if FRatingsImage <> Value then
  begin
    if FNotify = nil then
    begin
      FNotify := TNotify.Create(nil);
      FNotify.FOwner := Self;
    end;
    if FRatingsImage <> nil then
      FRatingsImage.RemoveFreeNotification(FNotify);

    LListView := Owner.Owner;
    if Assigned(LListView) and not (csDestroying in LListView.ComponentState) then
    begin
      for LItem in LListView.Items do
      begin
        LObject := LItem.Objects.FindObject(TRatingsListItemAppearanceNames.RatingsImageName) as TListItemImage;
        if (LObject <> nil)  then
          if (FRatingsImage <> nil) and (LObject.Bitmap = FRatingsImage.Bitmap) then
          begin
            if Value <> nil then
              LObject.Bitmap := Value.Bitmap
            else
              LObject.Bitmap := nil;
          end
          else if Value <> nil then
            LObject.Bitmap := Value.Bitmap;
          (Owner as TRatingsListItemAppearance).UpdateImageSrcRect(LItem);
      end;
    end;
    FRatingsImage := Value;
    if FRatingsImage <> nil then
      FRatingsImage.FreeNotification(FNotify);
  end;
end;

procedure TRatingsImageObjectAppearance.SetRatingsCount(const Value: Integer);
var
  LListView: TCustomListView;
  LItem: TListViewItem;
begin
  FRatingsCount := Value;
  LListView := Owner.Owner;
  if Assigned(LListView) and not (csDestroying in LListView.ComponentState) then
    for LItem in LListView.Items do
      (Owner as TRatingsListItemAppearance).UpdateImageSrcRect(LItem);
end;

{ TRatingsImageObjectAppearance.TNotify }

procedure TRatingsImageObjectAppearance.TNotify.Notification(
  AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = TOperation.opRemove then
    if AComponent = FOwner.RatingsImage then
      FOwner.RatingsImage := nil;
end;

type
  TOption = TCustomListView.TRegisterAppearanceOption;
const
  sThisUnit = 'RatingsAppearanceU';     // Will be added to the uses list when appearance is used


initialization
  // RatingsListItem group
  TCustomListView.RegisterAppearance(
    TRatingsListItemAppearance, TRatingsListItemAppearanceNames.ListItem,
    [TOption.Item], sThisUnit);
  TCustomListView.RegisterAppearance(
    TRatingsListItemDeleteAppearance, TRatingsListItemAppearanceNames.ListItemDelete,
    [TOption.ItemEdit], sThisUnit);
  TCustomListView.RegisterAppearance(
    TRatingsListItemShowCheckAppearance, TRatingsListItemAppearanceNames.ListItemCheck,
    [TOption.ItemEdit], sThisUnit);
finalization
  TCustomListView.UnregisterAppearances(
    TArray<TCustomListView.TItemAppearanceObjectsClass>.Create(
      TRatingsListItemAppearance, TRatingsListItemDeleteAppearance,
      TRatingsListItemShowCheckAppearance));
end.

