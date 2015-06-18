unit UnitTest;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Objects, FMX.Controls.Presentation,
  FMX.Edit;

type
  TForm1 = class(TForm)
    PaintBox1: TPaintBox;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormTouch(Sender: TObject; const Touches: TTouches; const Action: TTouchAction);
    procedure PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
    procedure PaintBox1Tap(Sender: TObject; const Point: TPointF);
  private
    { Private declarations }
    FTouches: TTouches;
    FBitmap: TBitmap;
    procedure DoPaint;
    procedure DrawPoint(APoint: TPointF; ASize: Integer = 2);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  FBitmap.SetSize(Width, Height);
  FBitmap.Clear(TAlphaColors.White);
  PaintBox1.Repaint;
end;

procedure TForm1.DoPaint;
var
  I: Integer;
begin
  PaintBox1.Canvas.BeginScene;
  FBitmap.Canvas.Stroke.Color := TAlphaColors.Black;
  for I := 0 to High(FTouches) do
  begin
    DrawPoint(FTouches[I].Location);
  end;
  PaintBox1.Canvas.EndScene;
end;

procedure TForm1.DrawPoint(APoint: TPointF; ASize: Integer);
var
  R: TRectF;
  locOffset: TPointF;
begin
  if FBitmap <> nil then
  begin
    FBitmap.Canvas.BeginScene;
    try
      locOffset := TPointF.Create(ASize, ASize);
      R := TRectF.Create(APoint - locOffset, APoint + locOffset);
      FBitmap.Canvas.DrawRect(R, 0, 0, [], 1);
    finally
      FBitmap.Canvas.EndScene;
    end;
    PaintBox1.Repaint;
  end;
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  FBitmap.SetSize(Width, Height);
  FBitmap.Clear(TAlphaColors.White);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FBitmap := TBitmap.Create;
  FBitmap.SetSize(Width, Height);
  FBitmap.Clear(TAlphaColors.White);
end;

procedure TForm1.FormTouch(Sender: TObject; const Touches: TTouches; const Action: TTouchAction);
begin
  FTouches := Touches;
  DoPaint;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
var
  R: TRectF;
begin
  R := TRectF.Create(0, 0, FBitmap.Width, FBitmap.Height);
  PaintBox1.Canvas.DrawBitmap(FBitmap, R, R, 1, True);
end;

procedure TForm1.PaintBox1Tap(Sender: TObject; const Point: TPointF);
begin
  PaintBox1.Canvas.BeginScene;
  FBitmap.Canvas.Stroke.Color := TAlphaColors.Red;
  DrawPoint(Point, 5);
  PaintBox1.Canvas.EndScene;
end;

end.
