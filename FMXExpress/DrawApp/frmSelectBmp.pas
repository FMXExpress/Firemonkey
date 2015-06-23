unit frmSelectBmp;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.StdCtrls, System.Actions, FMX.ActnList, FMX.StdActns,
  FMX.MediaLibrary.Actions;

type
  TSelectBmpfrm = class(TForm)
    ToolBar1: TToolBar;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Img: TImage;
    actall: TActionList;
    ActopenImage: TTakePhotoFromLibraryAction;
    Button1: TButton;
    btopen: TButton;
    actOpenCustom: TAction;
    procedure Button1Click(Sender: TObject);
    procedure ActopenImageDidFinishTaking(Image: TBitmap);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ImgPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure ImgMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure ImgMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure ImgMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure actOpenCustomExecute(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  private
    { Private declarations }
    startX,startY,ScaleFactor: Single;
    ftypemoving:integer;//1: Moving
    frectP1:TPointF;
    frectP2:TPointF;
    fcstroke:TStrokeBrush;
    procedure repairRePaint;
  public
    fbmp:TBitmap;
    { Public declarations }
{$IF Defined(MSWINDOWS) or Defined(MACOS)}
    function OpenImage:boolean;
{$ENDIF}
    function DropImage:boolean;
  end;

var
  SelectBmpfrm: TSelectBmpfrm;

implementation

{$R *.fmx}

{ TForm2 }

procedure TSelectBmpfrm.actOpenCustomExecute(Sender: TObject);
begin
{$IF Defined(MSWINDOWS) or Defined(MACOS)}
  OpenImage;
{$ENDIF}
end;

procedure TSelectBmpfrm.ActopenImageDidFinishTaking(Image: TBitmap);

begin
  fbmp.Assign(Image);
  repairRePaint;
end;

procedure TSelectBmpfrm.Button1Click(Sender: TObject);
begin
  DropImage;
end;

function TSelectBmpfrm.DropImage: boolean;
var
pcimg,pbmp:TPointF;
p1,p2:TPoint;
dx,dy:Single;
bmp:TBItmap;
r:TRect;
scale:Single;
begin
  result:=false;

  if not assigned(fbmp) then exit;
  pcimg:=PointF(img.Width/2,img.Height/2);
  pbmp:=PointF(fbmp.Width/2,fbmp.Height/2);
  dx:=frectP1.X -pcimg.X;
  dy:=frectP1.Y -pcimg.Y;
  //scale:=ScaleFactor;
  scale:=1;
  p1.X:=round(pbmp.X+dx*scale);
  if (p1.X<0) then
    p1.X:=0;
  if (p1.X>fbmp.Width-1) then
    p1.X:=fbmp.Width-1;
  p1.Y:=round(pbmp.Y+dy*scale);
  if (p1.Y<0) then
    p1.Y:=0;
  if (p1.Y>fbmp.Height-1) then
    p1.Y:=fbmp.Height-1;

  dx:=frectP2.X -pcimg.X;
  dy:=frectP2.Y -pcimg.Y;
  p2.X:=round(pbmp.X+dx*scale);
  if (p2.X<0) then
    p2.X:=0;
  if (p2.X>fbmp.Width-1) then
    p2.X:=fbmp.Width-1;
  p2.Y:=round(pbmp.Y+dy*scale);
  if (p2.Y<0) then
    p2.Y:=0;
  if (p2.Y>fbmp.Height-1) then
    p2.Y:=fbmp.Height-1;
  r:=TRect.Create(p1,p2);
  bmp:=TBItmap.Create(round(r.Width),round(r.Height));
  bmp.CopyFromBitmap(fbmp,r,0,0);
  fbmp.Assign(bmp);
  bmp.Free;
  repairRePaint;
end;

procedure TSelectBmpfrm.FormCreate(Sender: TObject);
begin
  ftypemoving:=0;
  fbmp:= TBitmap.Create(0, 0);
  frectP1:=PointF(img.Width/2-img.Width/4,img.Height/2-img.Height/4);
  frectP2:=PointF(img.Width/2+img.Width/4,img.Height/2+img.Height/4);
  fcstroke:=TStrokeBrush.Create(TBrushKind.bkSolid,TAlphaColorRec.Blue);
  fcstroke.DefaultColor:=TAlphaColorRec.Blue;
  fcstroke.Thickness:=1;
{$IF Defined(MSWINDOWS) or Defined(MACOS)}
  btopen.Action:=actOpenCustom;
{$ELSE}
  btopen.Action:=ActopenImage;
{$ENDIF}
end;

procedure TSelectBmpfrm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fbmp);
end;

procedure TSelectBmpfrm.ImgMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  d,ty,tx:Single;
begin
//
  startX:=X;startY:=Y;
  tx:=abs(frectP2.X-frectP1.X)/4;
  ty:=abs(frectP2.Y-frectP1.Y)/4;
  if (frectP1.X+tx<X) and (frectP2.X-tx>X) and (frectP1.Y+ty<Y) and (frectP2.Y-ty>Y) then
  begin
    //moving
    ftypemoving:=1;
  end
  else
  begin
    d:=abs(Y-frectP1.Y);
    ftypemoving:=2;
    if (d>abs(X-frectP2.X)) then
    begin
      ftypemoving:=3;
      d:=abs(X-frectP2.X)
    end;
    if (d>abs(Y-frectP2.Y)) then
    begin
      ftypemoving:=4;
      d:=abs(Y-frectP2.Y)
    end;
    if (d>abs(X-frectP1.X)) then
    begin
      ftypemoving:=5;
      d:=abs(X-frectP1.X)
    end;

  end;
end;

procedure TSelectBmpfrm.ImgMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
var
  dy,dx:Single;
begin
  if (ftypemoving=0) then exit;
  dx:=X-startX;
  dy:=Y-startY;
  if (ftypemoving=1) then
  begin
  if (frectP1.X+dx<img.Width)  and (frectP1.X+dx>=0) and (frectP2.X+dx<img.Width)  and (frectP2.X+dx>=0) then
  begin
      frectP1.X:=frectP1.X+dx;
      frectP2.X:=frectP2.X+dx;
  end;
  if (frectP1.Y+dy<img.Height)  and (frectP1.Y+dy>=0) and (frectP2.Y+dy<img.Height)  and (frectP2.Y+dy>=0)then
  begin
      frectP1.Y:=frectP1.Y+dy;
      frectP2.Y:=frectP2.Y+dy;
  end;
  end
  else if (ftypemoving=2) then
  begin
    if (frectP1.Y+dy<frectP2.Y) then
      frectP1.Y:=frectP1.Y+dy;
  end
  else if (ftypemoving=3) then
  begin
    if (frectP2.X+dx>frectP1.X) then
      frectP2.X:=frectP2.X+dx;
  end
  else if (ftypemoving=4) then
  begin
    if (frectP2.Y+dy>frectP1.Y) then
      frectP2.Y:=frectP2.Y+dy;
  end
  else if (ftypemoving=5) then
  begin
    if (frectP1.X+dx<frectP2.X) then
      frectP1.X:=frectP1.X+dx;
  end;
  img.Repaint;
  startX:=X;
  startY:=Y;

end;

procedure TSelectBmpfrm.ImgMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  ftypemoving:=0;
end;

procedure TSelectBmpfrm.ImgPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
var
r:TRectF;

begin
  inherited;
  r:=TRectF.Create(frectP1,frectP2);
  Canvas.BeginScene();
  Canvas.DrawRect(r,0,0,[TCorner.crTopLeft],1,fcstroke);
  Canvas.EndScene;

end;
{$IF Defined(MSWINDOWS) or Defined(MACOS)}
function TSelectBmpfrm.OpenImage:boolean;
var
dlg:TOpenDialog;
begin
 result:=false;
  frectP1:=PointF(img.Width/2-img.Width/4,img.Height/2-img.Height/4);
  frectP2:=PointF(img.Width/2+img.Width/4,img.Height/2+img.Height/4);

  dlg:=TOpenDialog.Create(Application);
  if (dlg.Execute) then
  begin

    try
      fbmp:=TBitmap.CreateFromFile(dlg.FileName);
      repairRePaint;
      result:=true;
    except
      ShowMessage('Error: Open Bitmap');
      fbmp:=nil;
    end;
  end;
  dlg.Free;
end;
{$ENDIF}
procedure TSelectBmpfrm.repairRePaint;
begin
  if not assigned(fbmp) then exit;
  ScaleFactor :=1.0;
  if fbmp.Width > img.Width then
  begin
    ScaleFactor := fbmp.Width / img.Width;
    fbmp.Resize(Round(fbmp.Width / ScaleFactor), Round(fbmp.Height / ScaleFactor));
  end
  else
  begin
    frectP1:=PointF(img.Width/2-fbmp.Width/4,img.Height/2-fbmp.Height/4);
    frectP2:=PointF(img.Width/2+fbmp.Width/4,img.Height/2+fbmp.Height/4);
  end;
  img.Bitmap.Assign(fbmp);
end;

procedure TSelectBmpfrm.SpeedButton1Click(Sender: TObject);
begin
self.ModalResult:=mrOK;
self.Hide();
end;

procedure TSelectBmpfrm.SpeedButton2Click(Sender: TObject);
begin
self.ModalResult:=mrCancel;
self.Hide();
end;

end.



