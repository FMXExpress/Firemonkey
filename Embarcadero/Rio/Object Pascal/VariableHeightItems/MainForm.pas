//---------------------------------------------------------------------------
// Copyright (c) 2016 Embarcadero Technologies, Inc. All rights reserved.  
//
// This software is the copyrighted property of Embarcadero Technologies, Inc. 
// ("Embarcadero") and its licensors. You may only use this software if you 
// are an authorized licensee of Delphi, C++Builder or RAD Studio 
// (the "Embarcadero Products").  This software is subject to Embarcadero's 
// standard software license and support agreement that accompanied your 
// purchase of the Embarcadero Products and is considered a Redistributable, 
// as such term is defined thereunder. Your use of this software constitutes 
// your acknowledgement of your agreement to the foregoing software license 
// and support agreement. 
//---------------------------------------------------------------------------
unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.ListView.Types,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView,
  Markov;

type
  TVariableHeight = class(TForm)
    Button1: TButton;
    ListView1: TListView;
    ToolBar1: TToolBar;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure ListView1UpdateObjects(const Sender: TObject;
      const AItem: TListViewItem);
  private
    FChain: TChain;
    FBitmaps: TDictionary<Integer, TBitmap>;
    FText: TArray<string>;
    procedure ReadText;
    function GetDimensionBitmap(const Width, Height: Single): TBitmap;
  public
    function GetTextHeight(const D: TListItemText; const Width: Single; const Text: string): Integer;
    destructor Destroy; override;
  end;

var
  VariableHeight: TVariableHeight;

implementation

uses
  System.IOUtils, FMX.TextLayout;

{$R *.fmx}

// Create a new item with random text of random length
procedure TVariableHeight.Button1Click(Sender: TObject);
begin
  ReadText;
  ListView1.Items.Add.Data['txtMain'] := FText[Random(Length(FText))];
  // with classic appearances, use Text property
  //ListView1.Items.Add.Text := FChain.Generate(Random(100) + 5);
end;

function TVariableHeight.GetDimensionBitmap(const Width, Height: Single): TBitmap;
  procedure Arrow(C: TCanvas; P: array of TPointF);
  begin
    C.DrawLine(P[0], P[1], 1.0);
    C.DrawLine(P[0], P[2], 1.0);
    C.DrawLine(P[0], P[3], 1.0);
  end;

var
  EndP1, EndP2: TPointF;
  TextBitmap: TBitmap;
  IntHeight: Integer;
begin
  IntHeight := Trunc(Height);

  if FBitmaps = nil then
    FBitmaps := TDictionary<Integer, TBitmap>.Create;
  if not FBitmaps.TryGetValue(IntHeight, Result) then
  begin
    Result := TBitmap.Create(Trunc(Width), IntHeight);
    FBitmaps.Add(IntHeight, Result);
    if Result.Canvas.BeginScene then
    begin
      Result.Canvas.Clear(TAlphaColorRec.Null);
      Result.Canvas.Stroke.Color := TAlphaColorRec.Darkgray;

      // Draw the arrows
      EndP1 := TPointF.Create(Width/2, 0);
      EndP2 := TPointF.Create(Width/2, Height);
      Arrow(Result.Canvas,
        [EndP1, TPointF.Create(Width/2, Height/2 - Width/2),
         EndP1 + TPointF.Create(-2, 5), EndP1 + TPointF.Create(2, 5)]);
      Arrow(Result.Canvas,
        [EndP2, TPointF.Create(Width/2, Height/2 + Width/2),
         EndP2 + TPointF.Create(-2, -5), EndP2 + TPointF.Create(2, -5)]);

      // Draw the dimension text
      TextBitmap := TBitmap.Create(Trunc(Width), Trunc(Width));
      try
        if TextBitmap.Canvas.BeginScene then
          with TextBitmap.Canvas do
          begin
            Clear(TAlphaColorRec.Null);
            Fill.Color := TAlphaColorRec.Darkgray;
            FillText(TextBitmap.BoundsF, ''.Format('%d', [IntHeight]), False, 1,
              [], TTextAlign.Center, TTextAlign.Center);
            EndScene;
          end;
        TextBitmap.Rotate(90);
        Result.Canvas.DrawBitmap(TextBitmap, TextBitmap.BoundsF,
          TextBitmap.BoundsF.CenterAt(Result.BoundsF), 1);
      finally
        TextBitmap.Free;
      end;

      Result.Canvas.EndScene;
    end;
  end;
end;

destructor TVariableHeight.Destroy;
var
  Key: Integer;
begin
  FChain.Free;
  if FBitmaps <> nil then
    for Key in FBitmaps.Keys do
      FBitmaps[Key].Free;
  FBitmaps.Free;
  inherited;
end;

// Calculate height for text drawable D
function TVariableHeight.GetTextHeight(const D: TListItemText; const Width: single; const Text: string): Integer;
var
  Layout: TTextLayout;
begin
  // Create a TTextLayout to measure text dimensions
  Layout := TTextLayoutManager.DefaultTextLayout.Create;
  try
    Layout.BeginUpdate;
    try
      // Initialize layout parameters with those of the drawable
      Layout.Font.Assign(D.Font);
      Layout.VerticalAlign := D.TextVertAlign;
      Layout.HorizontalAlign := D.TextAlign;
      Layout.WordWrap := D.WordWrap;
      Layout.Trimming := D.Trimming;
      Layout.MaxSize := TPointF.Create(Width, TTextLayout.MaxLayoutSize.Y);
      Layout.Text := Text;
    finally
      Layout.EndUpdate;
    end;
    // Get layout height
    Result := Round(Layout.Height);
    // Add one em to the height
    Layout.Text := 'm';
    Result := Result + Round(Layout.Height);
  finally
    Layout.Free;
  end;
end;

procedure TVariableHeight.ListView1UpdateObjects(const Sender: TObject;
  const AItem: TListViewItem);
var
  Drawable: TListItemText;
  SizeImg: TListItemImage;
  Text: string;
  AvailableWidth: Single;
begin
  SizeImg := TListItemImage(AItem.View.FindDrawable('imgSize'));
  AvailableWidth := TListView(Sender).Width - TListView(Sender).ItemSpaces.Left
    - TListView(Sender).ItemSpaces.Right - SizeImg.Width;

  // Find the text drawable which is used to calcualte item size.
  // For dynamic appearance, use item name.
  // For classic appearances use TListViewItem.TObjectNames.Text
  // Drawable := TListItemText(AItem.View.FindDrawable(TListViewItem.TObjectNames.Text));
  Drawable := TListItemText(AItem.View.FindDrawable('txtMain'));
  Text := Drawable.Text;

  // Randomize the font when updating for the first time
  if Drawable.TagFloat = 0 then
  begin
    Drawable.Font.Size := 1; // Ensure that default font sizes do not play against us
    Drawable.Font.Size := 10 + Random(4) * 4;

    Drawable.TagFloat := Drawable.Font.Size;
    if Text.Length < 100 then
      Drawable.Font.Style := [TFontStyle.fsBold];
  end;

  // Calculate item height based on text in the drawable
  AItem.Height := GetTextHeight(Drawable, AvailableWidth, Text);
  Drawable.Height := AItem.Height;
  Drawable.Width := AvailableWidth;

  SizeImg.OwnsBitmap := False;
  SizeImg.Bitmap := GetDimensionBitmap(SizeImg.Width, AItem.Height);
end;

procedure TVariableHeight.ReadText;
const
  Delimiters: array of char = [#10, #13];
var
  Reader: TStreamReader;
  Stream: TResourceStream;
begin
  if Length(FText) = 0 then
  begin
    Stream := TResourceStream.Create(HInstance, 'Blabla', RT_RCDATA);
    Reader := TStreamReader.Create(Stream);
    try
      FText := Reader.ReadToEnd.Split(Delimiters, TStringSplitOptions.ExcludeEmpty)
    finally
      Reader.Close;
      Reader.Free;
      Stream.Free;
    end;
  end;
end;

end.
