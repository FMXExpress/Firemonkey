//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit unitSearchMenuHelper;

// Opacity of the Layer = .5 (partly hidden)
// 1 x Glow Effect = Partial Selection
// 2 x Glow Effect = Brighter

interface

uses
  FMX.Effects, FMX.Layouts, FMX.Objects, System.Classes, System.UITypes, FMX.Types,
  FMX.Controls, Generics.Collections, Math, System.SysUtils, FMX.StdCtrls, FMX.Graphics;

type
  ///	<summary>
  ///	 Use TSearchState to read the current state of the Search Item after SearchText
  ///	</summary>
  ///  <remarks>
  ///  ssNone = No Search,
  ///  ssNoMatch = No Match on the current Search
  ///  ssPartial = Part Match on the current search
  ///  ssFull = Full match in the current search
  ///  </remarks>
  TSearchState = (ssNone, ssNoMatch, ssPartial, ssFull);

  ///	<summary>
  ///	 Use TSearchItems to display the icons in the Search Menu.
  ///	</summary>
  ///	<remarks>
  /// An TSearchItem consists of a TLayer, Rectangle (to hide border) Circle to highlight a ssFull state and an TImage
  ///	</remarks>
  TSearchItem = class(TLayout)
  strict private
    BackGroundRec : TRectangle;
    Image  : TImage;
    GlowPartial : TGlowEffect;
    FooterLabel : Tlabel;
    BackGroundGlow: TCircle;
    FSearchText: TStringList;
    FOnDblClick: TNotifyEvent;
    procedure InternalOnDblClick(Sender : TObject);
    procedure SetSearchText(const Value: string);
    function GetSearchText: string;
    function GetText: string;
  private
    FSearchState: TSearchState;
    procedure SetSearchState(const Value: TSearchState);
  public
    procedure TextSearch(Value : string);
    constructor Create(AOwner: TComponent; BackGroundColor : TAlphaColor; GlowColor : TAlphaColor; SourceImage : TImage; aText : string); reintroduce;
    destructor Destroy; override;
  published
    property SearchText : string read GetSearchText write SetSearchText;
    property Text : string read GetText;
    property OnDblClick : TNotifyEvent read FOnDblClick write FOnDblClick;
    property State : TSearchState read FSearchState write SetSearchState;
  end;


  ///	<summary>
  ///	  A Search Band has a head and a grid of Search Items
  ///	</summary>
  ///	<remarks>
  ///	  Use a Search Band manager to control a list of Search items
  ///	</remarks>
  TSearchBand = class(TLayout)
  strict private
    FItemGrid : TGridLayout;
    FSearchItems : TObjectList<TSearchItem>;
    FBandTitle: TLabel;
    function GetItemHeight: Single;
    function GetItemWidth: Single;
    procedure SetItemHeight(const Value: Single);
    procedure SetItemWidth(const Value: Single);
    function GetCount: Integer;
    procedure InternalResize(Sender: TObject);
    function GetText: string;
  public
    ///	<summary>
    ///	  Creates a new Search Band.
    ///	  If OwnsObjects = True, the SearchBand will Free the added items when
    ///	</summary>
    constructor Create(AOwner : TComponent; aOwnsObjects : Boolean; BackGroundColor : TAlphaColor; aText : string; aItemHeight, aItemWidth : Single); reintroduce;
    destructor Destroy; override;

    ///	<summary>
    ///	  Pass in Delimited Text for a ANY match = Full brightness
    ///	</summary>
    ///	<remarks>
    ///	  TextSearch is used to set the value of the Glow Effects and Opacity
    ///	  to identify matching searches
    ///	</remarks>
    procedure TextSearch(Value : string);
    procedure Add(Item : TSearchItem);
    property Count : Integer read GetCount;
  published
    property ItemHeight : Single read GetItemHeight write SetItemHeight;
    property ItemWidth : Single read GetItemWidth write SetItemWidth;
    property Text : string read GetText;
 end;

  TSearchBandManager = class(TLayout) // TVerticalScrollBox would be good for making this a component.
  strict private
    FSearchBands : TObjectList<TSearchBand>;
  private
    function GetSBItem(Index: Integer): TSearchBand;
    function GetCount: Integer;
  public
    constructor Create(AOwner : TComponent; aOwnsObjects : Boolean); reintroduce;
    destructor Destroy; override;
    procedure Add(SearchBand : TSearchBand);
    procedure TextSearch(Value : string);
    procedure Clear;
    function BandByName(aName : string) : TSearchBand;
    property Items[Index: Integer]: TSearchBand read GetSBItem;
    property Count : Integer read GetCount;
  end;

implementation

{ TSearchItem }

constructor TSearchItem.Create(AOwner: TComponent; BackGroundColor : TAlphaColor; GlowColor : TAlphaColor; SourceImage : TImage; aText : string);
var
  Stream : TMemoryStream;

  function CreateEffect: TGlowEffect;
  begin
    Result := TGlowEffect.Create(Self);
    Result.Parent := Image;
    Result.Opacity := 1;
    Result.Softness := 1;
    //Result.ShadowColor := GlowColor;
    Result.GlowColor := GlowColor;
    Result.Enabled := False;
  end;

begin
  inherited Create(AOwner);
  FSearchText := TStringList.Create;

  /// Make the background Blend in
  BackGroundRec := TRectangle.Create(Self);
  BackGroundRec.Parent := Self;
  BackGroundRec.Align := TAlignLayout.Client;
  BackGroundRec.Fill.Color := BackGroundColor;
  BackGroundRec.Fill.Kind := TBrushKind.Solid;
  BackGroundRec.Stroke.Color := BackGroundColor; //TAlphaColorRec.Red;//
  BackGroundRec.Visible := True;
  BackGroundRec.HitTest := True;
  BackGroundRec.OnDblClick := InternalOnDblClick;

  BackGroundGlow := TCircle.Create(Self);
  BackGroundGlow.Parent := BackGroundRec;
  BackGroundGlow.HitTest := False;
  BackGroundGlow.Stroke.Color :=  BackGroundColor;
  BackGroundGlow.Stroke.Kind := TBrushKind.Solid;
  BackGroundGlow.Opacity := 0.85;
  BackGroundGlow.Fill.Color := BackGroundColor;
  BackGroundGlow.Fill.Kind := TBrushKind.Solid;
  BackGroundGlow.Align := TAlignLayout.Client;
  BackGroundGlow.Width := BackGroundRec.Width;
  BackGroundGlow.Height := BackGroundRec.Height;
  BackGroundGlow.Align := TAlignLayout.Client;

  // Add the image
  Image  := TImage.Create(Self);
  Image.Parent := BackGroundRec; //BackGroundGlow;//
  Image.Width := BackGroundRec.Width;
  Image.Height := BackGroundRec.Height;
  Image.Padding.Left := 20;
  Image.Padding.Right := 20;
  Image.Padding.Top := 20;
  Image.Padding.Bottom := 20;
  Image.Align := TAlignLayout.Center;
  Image.HitTest := False;

  Stream := TMemoryStream.Create;
  try
    SourceImage.Bitmap.SaveToStream(Stream);
    Stream.Position := 0;
    Image.Bitmap.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
  Image.Visible := True;

  // Add the two glow effects to the image
  GlowPartial := CreateEffect;

  FooterLabel := Tlabel.Create(Self);
  FooterLabel.Parent := Self;
  FooterLabel.Align := TAlignLayout.Bottom;
  FooterLabel.TextAlign := TTextAlign.Center;
  FooterLabel.Text := aText;
  FooterLabel.Font.Size := 10;
  FooterLabel.Height := 20;
  FooterLabel.Visible := True;
  FooterLabel.HitTest := False;
end;

destructor TSearchItem.Destroy;
begin
  GlowPartial.Free;
  Image.Free;
  BackGroundGlow.Free;
  BackGroundRec.Free;
  FooterLabel.Free;
  inherited;
end;

function TSearchItem.GetSearchText: string;
begin
  Result := FSearchText.Text;
end;

function TSearchItem.GetText: string;
begin
  Result := Self.FooterLabel.Text;
end;

procedure TSearchItem.InternalOnDblClick(Sender: TObject);
begin
  if Assigned(FOnDblClick) then
    FOnDblClick(Self);
end;

procedure TSearchItem.SetSearchState(const Value: TSearchState);
begin
  FSearchState := Value;
  case Value of
    ssNone    : begin
                  Self.Opacity := 1;
                  GlowPartial.Enabled := False;
                  BackGroundGlow.Fill.Color := BackGroundRec.Fill.Color;
                end;
    ssNoMatch : begin
                  Self.Opacity := 0.5;
                  GlowPartial.Enabled := False;
                  BackGroundGlow.Fill.Color := BackGroundRec.Fill.Color;
                end;
    ssPartial : begin
                  Self.Opacity := 1;
                  GlowPartial.Enabled := True;
                  BackGroundGlow.Fill.Color := BackGroundRec.Fill.Color;
                end;
    ssFull    : begin
                  Self.Opacity := 1;
                  GlowPartial.Enabled := True;
                  BackGroundGlow.Fill.Color := GlowPartial.GlowColor;
                end;
  end;

end;

procedure TSearchItem.SetSearchText(const Value: string);
begin
  FSearchText.DelimitedText := Lowercase(Value);
end;

procedure TSearchItem.TextSearch(Value: string);
begin
  if Value = '' then begin
    Self.State := ssNone;
    Exit;
  end;

  Value := LowerCase(Value);

  if Pos(Value,SearchText) = 0 then begin
    State := ssNoMatch;
    Exit;
  end;

  if FSearchText.IndexOf(Value) > -1 then
    State := ssFull
  else
    State := ssPartial;
end;

{ TSearchBand }

procedure TSearchBand.Add(Item: TSearchItem);
begin
  if Item = nil then
    Exit;

  // Add New Items Only
  if FSearchItems.IndexOf(Item) > -1 then
    Exit;

  Self.FItemGrid.AddObject(Item);
  Self.FSearchItems.Add(Item);

  InternalResize(Self.FItemGrid);
end;

constructor TSearchBand.Create(AOwner: TComponent; aOwnsObjects : Boolean; BackGroundColor : TAlphaColor; aText : string; aItemHeight, aItemWidth : Single);
begin
  inherited Create(AOwner);
  Self.FSearchItems := TObjectList<TSearchItem>.Create(aOwnsObjects);
  Self.Align := TAlignLayout.Top;

  FBandTitle := TLabel.Create(Self);
  FBandTitle.Parent := Self;
  FBandTitle.Text := aText;
  FBandTitle.Visible := True;
  FBandTitle.Align := TAlignLayout.Top;
  FBandTitle.Font.Size := 16;
  FBandTitle.Font.Style := [TFontStyle.fsBold];
  FBandTitle.Padding.Top := 10;
  FBandTitle.Padding.Bottom := 10;

  Self.FItemGrid := TGridLayout.Create(AOwner);
  Self.FItemGrid.Parent := Self;
  Self.FItemGrid.ItemHeight := aItemHeight;
  Self.FItemGrid.ItemWidth := aItemWidth;
  Self.FItemGrid.Align := TAlignLayout.Client;

  Self.FItemGrid.OnResize := InternalResize;
  InternalResize(Self.FItemGrid);
end;

destructor TSearchBand.Destroy;
begin
  Self.FBandTitle.Free;
  Self.FSearchItems.Free;
  Self.FItemGrid.Free;
  inherited;
end;

function TSearchBand.GetCount: Integer;
begin
  Result := Self.FSearchItems.Count;
end;

function TSearchBand.GetItemHeight: Single;
begin
  Result := Self.FItemGrid.ItemHeight;
end;

function TSearchBand.GetItemWidth: Single;
begin
  Result := Self.FItemGrid.ItemWidth;
end;

function TSearchBand.GetText: string;
begin
  Result := Self.FBandTitle.Text;
end;

procedure TSearchBand.InternalResize(Sender: TObject);
var
  Ratio : Single;
begin
  Self.Height := Self.FItemGrid.ItemHeight;
  //Self.FItemGrid.Height;
  Ratio := (ItemWidth * Count) / Self.Width;
  Self.Height := Self.FBandTitle.Height + (ItemHeight * Ceil(Ratio)) + 30; //Bottom Padding
end;

procedure TSearchBand.SetItemHeight(const Value: Single);
begin
  Self.FItemGrid.ItemHeight := Value;
end;

procedure TSearchBand.SetItemWidth(const Value: Single);
begin
  Self.FItemGrid.ItemWidth := Value;
end;

procedure TSearchBand.TextSearch(Value: string);
var
  SI : TSearchItem;
begin
  for SI in Self.FSearchItems do
    SI.TextSearch(Value);
end;

{ TSearchBandManager }

procedure TSearchBandManager.Add(SearchBand: TSearchBand);
var
  Band: TSearchBand;
begin
  if SearchBand = nil then
    Exit;

  if Self.FSearchBands.IndexOf(SearchBand) > -1 then
    Exit;


  for Band in Self.FSearchBands do
    Band.Align := TAlignLayout.MostTop;

  Self.FSearchBands.Add(SearchBand);
  SearchBand.Parent := Self;
  SearchBand.Align := TAlignLayout.Top;
  SearchBand.Visible := True;

  if FSearchBands.Count > 0 then
    SearchBand.Top := FSearchBands[Count-1].Top + FSearchBands[Count-1].Height
  else
    SearchBand.Top := 0;
end;

function TSearchBandManager.BandByName(aName: string): TSearchBand;
var
  Band: TSearchBand;
begin
  Result := nil;
  for Band in Self.FSearchBands do
    if Band.Text = aName then
      Exit(Band);
end;

procedure TSearchBandManager.Clear;
begin
  Self.FSearchBands.Clear;
end;

constructor TSearchBandManager.Create(AOwner: TComponent; aOwnsObjects : Boolean);
begin
  inherited Create(AOwner);
  Self.FSearchBands := TObjectList<TSearchBand>.Create(aOwnsObjects);
end;

destructor TSearchBandManager.Destroy;
begin
  Self.FSearchBands.Free;
  inherited;
end;

function TSearchBandManager.GetCount: Integer;
begin
  Result := Self.FSearchBands.Count;
end;

function TSearchBandManager.GetSBItem(Index: Integer): TSearchBand;
begin
  Result := FSearchBands.Items[Index];
end;

procedure TSearchBandManager.TextSearch(Value: string);
var
  SB : TSearchBand;
begin
  if (FSearchBands = nil) or (FSearchBands.Count = 0) then
    Exit;

  for SB in Self.FSearchBands do begin
    SB.TextSearch(Value);
    SB.Repaint;
  end;
end;

end.
