unit MyPaintBox;

interface

uses
  System.SysUtils,FMX.Surfaces,FMX.Colors,System.UITypes,System.Types, System.Classes,FMX.Controls,FMX.Graphics, FMX.Types, FMX.Objects;

type
  TFunctionDraw=(fdNone,fdPen,fdLine,fdRectangle,fdEllipse,fdFillBgr,fdBitmapStamp,fdPolyLine);
  TMyPaintBox = class(TPaintBox)
  private
    {$IFDEF POSIX}
    ffillBrush : TStrokeBrush;
    {$ENDIF}
    fDrawing:boolean;
    fbmpstamp:TBitmap;
    ffDraw:TFunctionDraw;
    fdrawbmp:TBitmap;
    fdrawbmprect:TRectF;
    fdrawrect:TRectF;//Paint box size
    pFrom,pTo:TPointF;
    fThickness:Single;
    ffgColor:TAlphaColor;
    fbgColor:TAlphaColor;
    fnofill:boolean;
    fcbrush:TBrush;//Current drawing Brush
    fcstroke:TStrokeBrush;//Current drawing stroke
    procedure SetForegroundColor(v:TAlphaColor);
    procedure SetBackgroundColor(v:TAlphaColor);
    procedure SetThickness(v:Single);
    procedure SetNoFill(v:boolean);
    procedure SetBitmapStamp(v:TBitmap);
  private
    procedure StartDrawing(startP:TPointF);
    procedure EndDrawing(startP:TPointF);
    procedure DoDraw(vCanvas: TCanvas;const drawall:boolean=true);
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ForegroundColor:TAlphaColor read ffgColor write SetForegroundColor;
    property BackgroundColor:TAlphaColor read fbgColor write SetBackgroundColor;
    property Thickness:Single read fThickness write SetThickness;
    property FuncDraw:TFunctionDraw read ffDraw write ffDraw;
    property NoFill:Boolean read fnofill write SetNoFill;
    property BitmapStamp:TBitmap read fbmpstamp write SetBitmapStamp;
  public
    procedure FillColor(color:TAlphaColor);
    procedure SaveToJPEGStream(Stream: TStream);
    {$IFDEF POSIX}
    procedure FFillerMod;
    {$ENDIF}
  published
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MyComponent', [TMyPaintBox]);
end;

{ TMyPaintBox }

constructor TMyPaintBox.Create(AOwner: TComponent);
begin
  inherited;
  fbmpstamp:=nil;
  Parent:=TFmxObject(AOwner);
  Align:= TAlignLayout.alClient;
  ffDraw:=TFunctionDraw.fdNone;
  fnofill:=false;
  fDrawing:=false;
  fThickness:=1;
  pFrom := PointF(-1, -1);
  pTo := PointF(-1, -1);

  fdrawrect:=RectF(0,0,self.Width, self.Height);
  fdrawbmprect:=RectF(0,0,self.Width, self.Height);
  fdrawbmp := TBitmap.Create(Round(fdrawbmprect.Width),Round(fdrawbmprect.Height));

  SetBackgroundColor(TAlphaColorRec.White);
  SetForegroundColor(TAlphaColorRec.Black);
  FillColor(fbgColor);

  {$IFDEF POSIX}
  FFillerMod;
  {$ENDIF}

end;

destructor TMyPaintBox.Destroy;
begin

  if (assigned(fcbrush)) then
    fcbrush.Free;
  if (assigned(fcstroke)) then
    fcstroke.Free;
  if (assigned(fdrawbmp)) then
    fdrawbmp.Free;
  if assigned(fbmpstamp) then
    fbmpstamp.Free;

  {$IFDEF POSIX}
  if assigned ( ffillBrush ) then
    FreeAndNil ( ffillBrush );
  {$ENDIF}
  inherited;
end;

procedure TMyPaintBox.DoDraw(vCanvas: TCanvas; const drawall: boolean);
var
r,rd:TRectF;
begin
  if (drawall) then  self.Canvas.DrawBitmap(fdrawbmp,fdrawrect,fdrawrect,1);
  if (ffdraw=TFunctionDraw.fdNone) or(not fdrawing) then exit;

  r:=TRectF.Create(pFrom,pTo);
  with vCanvas do
  begin
  BeginScene();
  case ffdraw of
  {$IFDEF MSWINDOWS}
  TFunctionDraw.fdPen:begin
      DrawLine(pFrom,pTo,1,fcstroke);
  end;
  {$ENDIF}
  TFunctionDraw.fdLine:begin
      DrawLine(pFrom,pTo,1,fcstroke);
  end;
  TFunctionDraw.fdRectangle:begin
      if not fnofill then
        FillRect(r,0,0,[TCorner.crTopLeft],1,fcbrush);
      DrawRect(r,0,0,[TCorner.crTopLeft],1,fcstroke);
  end;
  TFunctionDraw.fdEllipse:begin
    if not fnofill then
         FillEllipse(r,1,fcbrush);
      DrawEllipse(r,1,fcstroke);
  end;
  TFunctionDraw.fdFillBgr:begin
      Clear(fbgColor);
  end;
  TFunctionDraw.fdBitmapStamp:if (assigned(fbmpstamp)) then begin
    r:=TRectF.Create(PointF(0,0),fbmpstamp.Width,fbmpstamp.Height);
    rd:=TRectF.Create(PointF(pTo.X,pTo.Y),fbmpstamp.Width,fbmpstamp.Height);
    DrawBitmap(fbmpstamp,r,rd,1);
  end;
  end;
  EndScene;
  end;
end;

procedure TMyPaintBox.EndDrawing(startP: TPointF);
begin
  if (not fdrawing) then exit;
  pTo := PointF(startP.X,startP.Y);
  DoDraw(fdrawbmp.Canvas,false);

  fdrawing:=false;
  pFrom := PointF(-1, -1);
  pTo := PointF(-1, -1);
end;

procedure TMyPaintBox.FillColor(color: TAlphaColor);
begin
  with fdrawbmp.Canvas do
  begin
    BeginScene();
    Clear(color);
    EndScene;
  end;
end;

procedure TMyPaintBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;
  if (not fdrawing) then
  begin
    StartDrawing(PointF(X, Y));
  end;
end;

procedure TMyPaintBox.MouseMove(Shift: TShiftState; X, Y: Single);
{$IFDEF POSIX}
var
Radius       : Single;
xDir, yDir   : Single;
Dx, Dy       : Single;
Ratio        : Single;
MoveX, MoveY : Single;
{$ENDIF}
begin
  inherited;
  if (not fdrawing) then exit;

{$IFDEF POSIX}
  Radius := fThickness / 2;
{$ENDIF}

  pTo := PointF(X, Y);
  InvalidateRect(fdrawrect);
  case ffdraw of
    TFunctionDraw.fdPen:
    begin
    {$IFDEF POSIX}
      if ( pFrom.Round <> pTo.Round ) then
        begin
         { Direction detection from pFrom to pTo }
         { to adjust start center                }

         IF pTo.Y >= pFrom.Y THEN yDir := -1 ELSE yDir := 1;
         IF pTo.X >= pFrom.X THEN xDir := -1 ELSE xDir := 1;

         { Quantify movement }

         Dx := ABS ( pTo.X - pFrom.X );
         Dy := ABS ( pTo.Y - pFrom.Y );

         IF ABS ( Dy ) > ABS ( Dx ) THEN
           Begin
                Ratio   := ABS ( Radius / Dy * Dx );
                MoveY   := Radius  * yDir;
                pFrom.Y := pFrom.Y + MoveY;
                MoveX   := Ratio   * xDir;
                pFrom.X := pFrom.X + MoveX;
           End
         ELSE
           Begin
                Ratio   := ABS ( Radius / Dx * Dy );
                MoveX   := Radius  * xDir;
                pFrom.X := pFrom.X + MoveX;
                MoveY   := Ratio   * yDir;
                pFrom.Y := pFrom.Y + MoveY;
           End;

         fdrawbmp.Canvas.BeginScene ();
            fdrawbmp.Canvas.DrawLine ( pFrom, pTo, 1, ffillBrush );
         fdrawbmp.Canvas.EndScene;

         { Direction detection end of line }
         { to adjust end of line center    }

         IF pTo.Y >= pFrom.Y THEN yDir := -1 ELSE yDir := 1;
         IF pTo.X >= pFrom.X THEN xDir := -1 ELSE xDir := 1;

         { Quantify movement }

         Dx := ABS ( pTo.X - pFrom.X );
         Dy := ABS ( pTo.Y - pFrom.Y );

         IF ABS ( Dy ) > ABS ( Dx ) THEN
           Begin
                Ratio   := ABS ( Radius / Dy * Dx );
                MoveY   := Radius * yDir;
                pFrom.Y := pTo.Y  + MoveY;
                MoveX   := Ratio  * xDir;
                pFrom.X := pTo.X  + MoveX;
           End
         ELSE
           Begin
                Ratio   := ABS ( Radius / Dx * Dy );
                MoveX   := Radius * xDir;
                pFrom.X := pTo.X  + MoveX;
                MoveY   := Ratio  * yDir;
                pFrom.Y := pTo.Y  + MoveY;
           End;
        end;
      {$ENDIF}
      {$IFDEF MSWINDOWS}
      if (pFrom<>pTo) then
      begin
      DoDraw(fdrawbmp.Canvas,false);
        pFrom:=pTo;
      end;
      {$ENDIF}

    end;
  end;
end;

procedure TMyPaintBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;
  EndDrawing(PointF(X, Y));
  InvalidateRect(fdrawrect);
end;

procedure TMyPaintBox.Paint;
begin
  inherited;
  if (csDesigning in ComponentState) then exit;
  DoDraw(self.Canvas);
end;

procedure TMyPaintBox.SaveToJPEGStream(Stream: TStream);
var
  Surf: TBitmapSurface;
  saveParams : TBitmapCodecSaveParams;
begin
  Surf := TBitmapSurface.Create;
  try
    Surf.Assign(fdrawbmp);
    saveparams.Quality:=100; // <-- always stops here with an AV error
    TBitmapCodecManager.SaveToStream(Stream, Surf, '.jpg',@saveParams);
  finally
    Surf.Free;
  end;
end;

procedure TMyPaintBox.SetBackgroundColor(v: TAlphaColor);
begin
  if (v=fbgColor) then exit;
  if (assigned(fcbrush)) then
    fcbrush.Free;
  fbgColor:=v;
  fcbrush:=TBrush.Create(TBrushKind.bkSolid,fbgColor);
end;

procedure TMyPaintBox.SetBitmapStamp(v: TBitmap);
begin
  if not assigned(v) then exit;
  if assigned(fbmpstamp) then
    fbmpstamp.Free;
  fbmpstamp:=TBitmap.Create(0,0);
  fbmpstamp.Assign(v);
end;

procedure TMyPaintBox.SetForegroundColor(v: TAlphaColor);
begin
  if (v=ffgColor) then exit;
  if (assigned(fcstroke)) then
    fcstroke.Free;
  ffgColor:=v;

  fcstroke:=TStrokeBrush.Create(TBrushKind.bkSolid,ffgColor);
  fcstroke.DefaultColor:=ffgColor;
  fcstroke.Thickness:=fThickness;

  {$IFDEF POSIX}
  ffillermod;
  {$ENDIF}

end;

procedure TMyPaintBox.SetNoFill(v: boolean);
begin
  if fnofill<>v then
    fnofill:=v;

end;

procedure TMyPaintBox.SetThickness(v: Single);
begin
  if (v=fThickness) then exit;
  if (assigned(fcstroke)) then
    fcstroke.Free;
  fThickness:=v;

  fcstroke:=TStrokeBrush.Create(TBrushKind.bkSolid,ffgColor);
  fcstroke.DefaultColor:=ffgColor;
  fcstroke.Thickness:=fThickness;

  {$IFDEF POSIX}
  ffillermod;
  {$ENDIF}

end;

procedure TMyPaintBox.StartDrawing(startP: TPointF);
begin
  if (csDesigning in ComponentState) then exit;
  if (fDrawing) or (ffDraw=TFunctionDraw.fdNone) then exit;

  pFrom := PointF(startP.X, startP.Y);
  pTo := PointF(startP.X, startP.Y);
  fDrawing:=true;
end;

{$IFDEF POSIX}
procedure TMyPaintBox.FFillerMod;
Begin
     IF NOT Assigned ( ffillBrush ) THEN ffillBrush := TStrokeBrush.Create ( TBrushKind.bkSolid, ffgcolor );

     ffillBrush.Thickness := fThickness;
     ffillBrush.Cap       := TStrokeCap.scRound;
     ffillBrush.Join      := TStrokeJoin.sjRound;
     ffillBrush.Color     := ffgcolor;
End;
{$ENDIF}


end.
