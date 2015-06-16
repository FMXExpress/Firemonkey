
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit FlagUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.ListBox, FMX.Layouts, FMX.StdCtrls,
  FMX.ExtCtrls, FMX.Printer, System.UIConsts, FMX.Graphics;

type
  TFlagKind = (flHorz, flVert, flCircle);

  TForm1 = class(TForm)
    FlgViewer: TImageViewer;
    FlagsCombo: TComboBox;
    CountryName: TLabel;
    PrintBtn: TButton;
    PrintDialog1: TPrintDialog;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    ListBoxItem4: TListBoxItem;
    ListBoxItem5: TListBoxItem;
    ListBoxItem6: TListBoxItem;
    ListBoxItem7: TListBoxItem;
    procedure FlagsComboClick(Sender: TObject);
    procedure PrintBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FlagsComboChange(Sender: TObject);
  private
    { Private declarations }
    procedure DrawFlag(col1, col2, col3: TColor; flgKind: TFlagKind = flVert);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  bw,bh: Single;

implementation

{$R *.fmx}

//Draw Flag (horizontal or vertical bands or circle in the center).
procedure TForm1.DrawFlag(col1, col2, col3: TColor; flgKind: TFlagKind = flVert);
var
    Bitmap:  TBitmap;
    l, t, r, b: integer;
begin
  Bitmap := TBitmap.Create(Round(FlgViewer.Width), Round(FlgViewer.Height));
  Bitmap.Canvas.BeginScene();

  case flgKind of
    flVert:
    begin
      Bitmap.Canvas.Fill.Color := col1;
      Bitmap.Canvas.Fill.Kind := TBrushKind.Solid;
      l := 0;
      t := Bitmap.Height;
      r := Round(Bitmap.Width*33/100);
      b := 0;

      Bitmap.Canvas.FillRect(TRectF.Create(l,t,r,b), 0, 100, AllCorners, 1);

      Bitmap.Canvas.Fill.Color := col2;
      l := Round(Bitmap.Width*33/100);
      t := Bitmap.Height;
      r := Round(Bitmap.Width*66/100);
      b := 0;

      Bitmap.Canvas.FillRect(TRectF.Create(l,t,r,b), 0, 100, AllCorners, 1);

      Bitmap.Canvas.Fill.Color := col3;
      l := Round(Bitmap.Width*66/100);
      t := Bitmap.Height;
      r := Bitmap.Width;
      b := 0;

      Bitmap.Canvas.FillRect(TRectF.Create(l,t,r,b), 0, 100, AllCorners, 1);

      //Draw outline rectangle
      Bitmap.Canvas.Fill.Color := claBlack;
      l := 0;
      t := Bitmap.Height;
      r := Bitmap.Width;
      b := 0;
      Bitmap.Canvas.DrawRect(TRectF.Create(l,t,r,b), 0, 100, AllCorners, 1);
    end;
    flHorz:
    begin
      Bitmap.Canvas.Fill.Color := col1;
      Bitmap.Canvas.Fill.Kind := TBrushKind.Solid;
      l := 0;
      t := Round(Bitmap.Height*33/100);
      r := Bitmap.Width;
      b := 0;

      Bitmap.Canvas.FillRect(TRectF.Create(l,t,r,b), 0, 100, AllCorners, 1);

      Bitmap.Canvas.Fill.Color := col2;
      l := 0;
      t := Round(Bitmap.Height*66/100);
      r := Bitmap.Width;
      b := Round(Bitmap.Height*33/100);

      Bitmap.Canvas.FillRect(TRectF.Create(l,t,r,b), 0, 100, AllCorners, 1);

      Bitmap.Canvas.Fill.Color := col3;
      l := 0;
      t := Bitmap.Height;
      r := Bitmap.Width;
      b := Round(Bitmap.Height*66/100);

      Bitmap.Canvas.FillRect(TRectF.Create(l,t,r,b), 0, 100, AllCorners, 1);

      //Draw outline rectangle
      Bitmap.Canvas.Fill.Color := claBlack;
      l := 0;
      t := Bitmap.Height;
      r := Bitmap.Width;
      b := 0;
      Bitmap.Canvas.DrawRect(TRectF.Create(l,t,r,b), 0, 100, AllCorners, 1);
    end;
    flCircle:
    begin
      //Draw background
      Bitmap.Canvas.Fill.Color := claWhite;
      Bitmap.Canvas.Fill.Kind := TBrushKind.Solid;
      Bitmap.Canvas.FillRect(TRectF.Create(0,Bitmap.Height,Bitmap.Width,0), 0, 100, AllCorners, 1);

      Bitmap.Canvas.Fill.Color := col1;
      Bitmap.Canvas.Fill.Kind := TBrushKind.Solid;
      l := Round(Width*29/100);
      t := Round(Height*14/100);
      r := Round(Width*58/100);
      b := Round(Height*47/100);
      Bitmap.Canvas.FillEllipse(TRectF.Create(l,t,r,b), 1);

      //Draw outline rectangle
      Bitmap.Canvas.Fill.Color := claBlack;
      l := 0;
      t := Bitmap.Height;
      r := Bitmap.Width;
      b := 0;
      Bitmap.Canvas.DrawRect(TRectF.Create(l,t,r,b), 0, 100, AllCorners, 1);
    end;
  end;

  Bitmap.Canvas.EndScene;
  FlgViewer.Bitmap := Bitmap;
end;

//Get base path to executable - Windows only.
function GetExePath: String;
var
  FullPath: String;
begin
  FullPath := GetCurrentDir;
  if Pos ('Win', FullPath) > 0 then
    FullPath := Copy(FullPath,0,Pos('Win', FullPath)-1)
  else
    FullPath := '';
  Result := FullPath;
end;

procedure TForm1.FlagsComboChange(Sender: TObject);
begin
  FlagsComboClick(Sender);
end;

procedure TForm1.FlagsComboClick(Sender: TObject);
var
  countryName: String;
begin
  countryName := FlagsCombo.ListBox.Items[FlagsCombo.ItemIndex];
  if countryName = 'United States' then begin
    FlgViewer.Bitmap.LoadFromFile(GetExePath + 'us-flag.gif');
    FlgViewer.Width := bw;
    FlgViewer.Height := bh;
  end else if countryName = 'Canada' then begin
    FlgViewer.Bitmap.LoadFromFile(GetExePath + 'canada-flag.gif');
    FlgViewer.Width := bw;
    FlgViewer.Height := bh;
  end else if countryName = 'Spain' then begin
    FlgViewer.Bitmap.LoadFromFile(GetExePath + 'spain-flag.gif');
    FlgViewer.Width := bw;
    FlgViewer.Height := bh;
  end else if countryName = 'Romania' then begin
    DrawFlag(claBlue, claYellow, claRed, flVert);
  end else if countryName = 'Italy' then begin
    DrawFlag(claGreen, claWhite, claRed, flVert);
  end else if countryName = 'Russia' then begin
    DrawFlag(claWhite, claBlue, claRed, flHorz);
  end else if countryName = 'Japan' then begin
    DrawFlag(claRed, claNull, claNull, flCircle);
  end;
  FlgViewer.Repaint;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  //Default to US flag
  FlgViewer.Bitmap.LoadFromFile(GetExePath + 'us-flag.gif');
  FlgViewer.Width := FlgViewer.Bitmap.Width;
  FlgViewer.Height := FlgViewer.Bitmap.Height;
  //default width and height
  bw := FlgViewer.Width;
  bh := FlgViewer.Height;
end;

procedure TForm1.PrintBtnClick(Sender: TObject);
var
  l,t,r,b: Integer;
  s: String;
  source, dest : TRectF;
begin
  if PrintDialog1.Execute then begin
    try
      //Set default DPI for the printer. The SelectDPI routine defaults
      //to the closest available resolution as reported by the driver.
      Printer.ActivePrinter.SelectDPI(1200, 1200);
      Printer.BeginDoc;

      //Print country name on top.
      Printer.Canvas.Font.Size := 15;
      Printer.Canvas.Font.Family   := 'Arial';
      Printer.Canvas.Font.Style  := [TFontStyle.fsbold];
      Printer.Canvas.Fill.Color  := claBlack;
      Printer.Canvas.Fill.Kind := TBrushKind.Solid;

      s := FlagsCombo.Items[FlagsCombo.ItemIndex];
      l := Round((Printer.PageWidth - Printer.Canvas.TextWidth(s)) / 99);
      t := Round(Height*3/100);
      r := l + Round(Printer.Canvas.TextWidth(s));
      b := t + Round(Printer.Canvas.TextHeight(s));

      Printer.Canvas.FillText(TRectF.Create(l, t, r, b), s, false, 1, [TFillTextFlag.RightToLeft], TTextAlign.Leading, TTextAlign.Leading);

      //print flag from bitmap.
      source := TRectF.Create(0,0,FlgViewer.Bitmap.Width,FlgViewer.Bitmap.Height);
      t := Round(Printer.PageHeight*4/100);
      r := l + Round(Printer.PageWidth*66/100);
      b := t + Round(Printer.PageHeight/3);
      dest := TRectF.Create(l,t,r,b);
      Printer.Canvas.DrawBitmap(FlgViewer.Bitmap, source, dest, 1);
      Printer.EndDoc;
    except
      on Exception do
      begin
        //Abort if an error occurs while printing.
        Printer.Abort;

        if Printer.Printing then
           Printer.EndDoc;

        Raise;
      end;
    end;
  end;
end;

end.
