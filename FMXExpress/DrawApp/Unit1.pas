unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Colors, FMX.ExtCtrls, FMX.ListBox;

type
  Tmainfrm = class(TForm)
    recBoard: TPaintBox;
    ToolBar1: TToolBar;
    pbdraw: TPopupBox;
    cbfg: TComboColorBox;
    cbbg: TComboColorBox;
    SpeedButton1: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure recBoardGesture(Sender: TObject;
      const EventInfo: TGestureEventInfo; var Handled: Boolean);
    procedure recBoardMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure recBoardMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure recBoardMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure recBoardPaint(Sender: TObject; Canvas: TCanvas);
    procedure pbdrawChange(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    idraw:integer;
    drawbmp:TBitmap;
    drawbmprect:TRectF;
    drawrect:TRectF;
    pFrom,pTo:TPointF;
    bdrawing:boolean;
    procedure StartDrawing(startP:TPointF);
    procedure EndDrawing(startP:TPointF);
    procedure DoDraw(Canvas: TCanvas;const drawall:boolean=true);
  public
    { Public declarations }
    procedure FillColor(color:TAlphaColor);
  end;

var
  mainfrm: Tmainfrm;

implementation

{$R *.fmx}

procedure Tmainfrm.DoDraw(Canvas: TCanvas;const drawall:boolean);
var
r:TRectF;
br:TBrush;
str:TStrokeBrush;
begin
  if (drawall) then  recBoard.Canvas.DrawBitmap(drawbmp,drawrect,drawrect,1);
  if (idraw<=0) or(not bdrawing) then exit;

  br:=TBrush.Create(TBrushKind.bkSolid,cbbg.Color);
  str:=TStrokeBrush.Create(TBrushKind.bkSolid,cbfg.Color);
  str.DefaultColor:=cbfg.Color;
  str.Thickness:=1;
  r:=TRectF.Create(pFrom,pTo);
  Canvas.BeginScene();
  case idraw of
  1:begin
      Canvas.DrawLine(pFrom,pTo,1,str);
  end;
  2:begin
      Canvas.FillRect(r,0,0,[TCorner.crTopLeft],1,br);
      Canvas.DrawRect(r,0,0,[TCorner.crTopLeft],1,str);
  end;
  3:begin
      Canvas.FillEllipse(r,1,br);
      Canvas.DrawEllipse(r,1,str);
  end;
  4:begin
      Canvas.Clear(cbbg.Color);
  end;
  end;
  Canvas.EndScene;
  br.Free;
  str.Free;
end;

procedure Tmainfrm.FillColor(color: TAlphaColor);
begin
  with drawbmp.Canvas do
  begin
    BeginScene();
    Clear(color);
    EndScene;
  end;
end;

procedure Tmainfrm.FormCreate(Sender: TObject);
begin
  bdrawing:=false;
  idraw:=0;
  pFrom := PointF(-1, -1);
  drawrect:=RectF(0,0,recBoard.Width, recBoard.Height);
  drawbmprect:=RectF(0,0,recBoard.Width, recBoard.Height);
  drawbmp := TBitmap.Create(Round(drawbmprect.Width),Round(drawbmprect.Height));
  FillColor(cbbg.Color);

end;

procedure Tmainfrm.FormDestroy(Sender: TObject);
begin
  drawbmp.Free;
end;

procedure Tmainfrm.pbdrawChange(Sender: TObject);
begin
  if (idraw=pbdraw.ItemIndex) then exit;
  if (pbdraw.ItemIndex>0) then
  begin
    //save context
  end
  else
  begin
  end;

  idraw:=pbdraw.ItemIndex;
end;

procedure Tmainfrm.recBoardGesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
{
 if ((pFrom.X <> -1) and (pFrom.Y <> -1)) then
 if (idraw<=0) then exit;
 if (not bdrawing) then
 begin
  if TInteractiveGestureFlag.gfBegin in EventInfo.Flags then
    StartDrawing(PointF(EventInfo.Location.X, EventInfo.Location.Y));
 end
 else
 begin

   if TInteractiveGestureFlag.gfEnd in EventInfo.Flags then
   begin
     EndDrawing(PointF(EventInfo.Location.X, EventInfo.Location.Y));
   end
   else
   begin
     pTo := PointF(EventInfo.Location.X, EventInfo.Location.Y);
     recBoard.InvalidateRect(drawrect);
   end;

 end;
}
end;

procedure Tmainfrm.recBoardMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if (not bdrawing) then
  begin
    StartDrawing(PointF(X, Y));
  end;
end;


procedure Tmainfrm.recBoardMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  if (not bdrawing) then exit;
  pTo := PointF(X, Y);
  recBoard.InvalidateRect(drawrect);
end;

procedure Tmainfrm.recBoardMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  EndDrawing(PointF(X, Y));
end;



procedure Tmainfrm.recBoardPaint(Sender: TObject; Canvas: TCanvas);
begin
  DoDraw(Canvas);
end;

procedure Tmainfrm.SpeedButton1Click(Sender: TObject);
begin
//drawbmp.SaveToFile();
end;

procedure Tmainfrm.StartDrawing(startP: TPointF);
begin
  if (bdrawing) or (idraw<=0) then exit;

  pFrom := PointF(startP.X, startP.Y);
  pTo := PointF(startP.X, startP.Y);
  bdrawing:=true;
end;

procedure Tmainfrm.EndDrawing(startP: TPointF);
begin
  if (not bdrawing) then exit;
  pTo := PointF(startP.X,startP.Y);
  DoDraw(drawbmp.Canvas,false);

  bdrawing:=false;
  pFrom := PointF(-1, -1);
  pTo := PointF(-1, -1);
end;
end.
