unit uDebugDrawerOpenGL;

interface

{$I ..\..\Physics2D\Physics2D.inc}

uses
  System.UITypes,
  UDebugDrawer,
  UPhysics2D,
  UPhysics2DTypes,
  UOpenGLCanvas;

type
  TDebugDrawerOpenGL = class(TDebugDrawer)
  public
    Canvas: TGLCanvas;

    // uPhysics2D.Tb2Draw overrides
    procedure DrawPolygon(const vertices: Tb2PolyVertices; vertexCount: Int32; const color: RGBA); override;
    procedure DrawPolygon4(const vertices: TVectorArray4; vertexCount: Int32; const color: RGBA); override;
    procedure DrawSolidPolygon(const vertices: Tb2PolyVertices; vertexCount: Int32; const color: RGBA); override;
    procedure DrawCircle(const center: TVector2; radius: PhysicsFloat; const color: RGBA); override;
    procedure DrawSolidCircle(const center, axis: TVector2; radius: PhysicsFloat; const color: RGBA); override;
    procedure DrawSegment(const p1, p2: TVector2; const color: RGBA); override;
    procedure DrawTransform(const xf: Tb2Transform); override;

    // uDebugDrawer.TDebugDrawer override
    procedure DrawPoint(const p: TVector2; size: PhysicsFloat; const color: RGBA); override;
    procedure DrawAABB(const aabb: Tb2AABB; const color: RGBA); override;
    procedure SetDefaultFontColor(const Value: TColor); override;
    procedure SetCanvasTranslation(x, y: PhysicsFloat); override;
    procedure SetCanvasTranslationOffset(dx, dy: PhysicsFloat); override;
    procedure TextOutASCII(text: string; x,y: integer); override;
  end;

implementation

uses
  Winapi.OpenGL;

{ TDrawerGL }

procedure TDebugDrawerOpenGL.DrawPolygon(const vertices: Tb2PolyVertices;
  vertexCount: Int32; const color: RGBA);
{$IFNDEF SINGLE_PRECISION}
var
   i: Integer;
{$ENDIF}
begin
   {$IFDEF SINGLE_PRECISION}
   Canvas.SetPenColor(TColorVector(color)).Polygon(TGLPointsF(@vertices[0]), vertexCount);
   {$ELSE}
   Canvas.SetPenColor(TColorVector(color));
   glBegin(GL_LINE_LOOP);
   for i := 0 to vertexCount - 1 do
      glVertex2d(vertices[i].X, vertices[i].Y);
   glEnd;
   {$ENDIF}
end;

procedure TDebugDrawerOpenGL.DrawPolygon4(const vertices: TVectorArray4;
   vertexCount: Int32; const color: RGBA);
{$IFNDEF SINGLE_PRECISION}
var
   i: Integer;
{$ENDIF}
begin
   {$IFDEF SINGLE_PRECISION}
   Canvas.SetPenColor(TColorVector(color)).Polygon(TGLPointsF(@vertices[0]), 4);
   {$ELSE}
   Canvas.SetPenColor(TColorVector(color));
   glBegin(GL_LINE_LOOP);
   for i := 0 to 3 do
      glVertex2d(vertices[i].X, vertices[i].Y);
   glEnd;
   {$ENDIF}
end;

procedure TDebugDrawerOpenGL.DrawSolidPolygon(const vertices: Tb2PolyVertices;
   vertexCount: Int32; const color: RGBA);
var
   tmp: TColorVector;
{$IFNDEF SINGLE_PRECISION}
   i: Integer;
{$ENDIF}
begin
   with TRGBA(color) do
   begin
      tmp[0] := red / 2;
      tmp[1] := green / 2;
      tmp[2] := blue / 2;
      tmp[3] := alpha / 2;
   end;
   {$IFDEF SINGLE_PRECISION}
   Canvas.SetPenColor(TColorVector(color)).SetBrushColor(tmp).
      FillPolygon(TGLPointsF(@vertices[0]), vertexCount, True);
   {$ELSE}
   Canvas.SetPenColor(tmp);
   glBegin(GL_POLYGON);
   for i := 0 to vertexCount - 1 do
      glVertex2d(vertices[i].X, vertices[i].Y);
   glEnd;
   Canvas.SetPenColor(TColorVector(color));
   glBegin(GL_LINE_LOOP);
   for i := 0 to vertexCount - 1 do
      glVertex2d(vertices[i].X, vertices[i].Y);
   glEnd;
   {$ENDIF}
end;

procedure TDebugDrawerOpenGL.DrawCircle(const center: TVector2; radius: PhysicsFloat; const color: RGBA);
begin
  Canvas.SetPenColor(TColorVector(color)).Ellipse(center.x, center.y, radius, radius);
end;

procedure TDebugDrawerOpenGL.DrawSolidCircle(const center, axis: TVector2; radius: PhysicsFloat; const color: RGBA);
var
   tmp: TColorVector;
   p: TVector2;
begin
   Canvas.SetPenColor(TColorVector(color));
   with TRGBA(color) do
   begin
      tmp[0] := red / 2;
      tmp[1] := green / 2;
      tmp[2] := blue / 2;
      tmp[3] := alpha / 2;
   end;
   Canvas.SetBrushColor(tmp).FillEllipse(center.x, center.y, radius, radius, True);

   {$IFDEF OP_OVERLOAD}
   p := center + radius * axis;
   {$ELSE}
   p := Add(center, Multiply(axis, radius));
   {$ENDIF}
   Canvas.Line(center.x, center.y, p.x, p.y);
end;

procedure TDebugDrawerOpenGL.DrawSegment(const p1, p2: TVector2; const color: RGBA);
begin
   Canvas.SetPenColor(TColorVector(color)).Line(p1.x, p1.y, p2.x, p2.y);
end;

procedure TDebugDrawerOpenGL.DrawTransform(const xf: Tb2Transform);
const
   k_axisScale = 0.4;
   clRed: TColorVector = (1.0, 0.0, 0.0, 1.0);
   clGreen: TColorVector = (0.0, 1.0, 0.0, 1.0);
var
   p2: TVector2;
   xAxis, yAxis: TVector2;
begin
   with xf do
   begin
      {$IFDEF OP_OVERLOAD}
      xAxis := q.GetXAxis;
      yAxis := q.GetYAxis;
      {$ELSE}
      xAxis := GetXAxis(q);
      yAxis := GetYAxis(q);
      {$ENDIF}
      p2.x := p.x + k_axisScale * xAxis.x;
      p2.y := p.y + k_axisScale * xAxis.y;
      Canvas.SetPenColor(clRed).Line(p.x, p.y, p2.x, p2.y);

      p2.x := p.x + k_axisScale * yAxis.x;
      p2.y := p.y + k_axisScale * yAxis.y;
      Canvas.SetPenColor(clGreen).Line(p.x, p.y, p2.x, p2.y);
   end;
end;

procedure TDebugDrawerOpenGL.SetCanvasTranslation(x, y: PhysicsFloat);
begin
  Canvas.BeginUpdateTransformation;
  Canvas.SetTranslateX(x);
  Canvas.SetTranslateX(y);
  Canvas.EndUpdateTransformation;
end;

procedure TDebugDrawerOpenGL.SetCanvasTranslationOffset(dx, dy: PhysicsFloat);
begin
  Canvas.BeginUpdateTransformation;
  Canvas.TranslateX := Canvas.TranslateX + dx;
  Canvas.TranslateY := Canvas.TranslateY + dy;
  Canvas.EndUpdateTransformation;
end;

procedure TDebugDrawerOpenGL.SetDefaultFontColor(const Value: TColor);
begin
  Canvas.DefaultFont.WinColor := Value;
end;

procedure TDebugDrawerOpenGL.TextOutASCII(text: string; x, y: integer);
begin
  Canvas.TextOutASCII(text, x, y);
end;

procedure TDebugDrawerOpenGL.DrawPoint(const p: TVector2; size: PhysicsFloat; const color: RGBA);
begin
   glPointSize(size);
   glColor3f(color[0], color[1], color[2]);
   glBegin(GL_POINTS);
   glVertex2f(p.x, p.y);
   glEnd;
   glPointSize(1.0);
end;

procedure TDebugDrawerOpenGL.DrawAABB(const aabb: Tb2AABB; const color: RGBA);
begin
   glColor3f(color[0], color[1], color[2]);
   glBegin(GL_LINE_LOOP);
   with aabb do
   begin
      glVertex2f(lowerBound.x, lowerBound.y);
      glVertex2f(upperBound.x, lowerBound.y);
      glVertex2f(upperBound.x, upperBound.y);
      glVertex2f(lowerBound.x, upperBound.y);
   end;
   glEnd;
end;

end.
