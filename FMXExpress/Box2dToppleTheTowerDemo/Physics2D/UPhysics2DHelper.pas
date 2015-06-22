unit UPhysics2DHelper;

{ box2D 2.3.0 translation

  ###  This unit is written based on Box2D maintained by Erin Catto (http://www.box2d.org)
  All type names follow the Delphi custom Txxx and xxx means the corresponding
  type in cpp source. This rule doesn't apply to the following types b2Vec2,
  b2Vec3, b2Mat22 and b2Mat33. Because I think these types are so primary that you
  may want to reuse them in other projects.

  ###  box2D 2.1.0 version is so different from 2.0.1 version that Erin nearly
  overwrote all codes. For more information I recommend reading the following page
  http://box2dflash.boristhebrave.com/docs/2.1a/updating

  ###  I translated version 2.0.1 without considering much of class encapsulation so
  you can find many m_xxx class memebers left out in public field. In this version
  I fixed all things and you should use Set/Get procedures to access these members.
  But for some members owning both Set and Get procedures I changed them to
  properties. For example I simplify SetUserData and GetUserData to such code
     Property UserData: Pointer read m_userData write m_userData;
  Another thing is that I changed some reference declaration(delphi doesn't support)
  to pointer to avoid unnecessary memory copy. You can check Tb2Body.GetTransform
  and Tb2DynamicTree.GetFatAABB for more details.

  ###  Another difference from version 2.0.1 is that I changed Tb2Contact
  declaration from class to record for effciency. Because class is created on
  heap and Tb2Contact is so frequently created and destroyed.

  ###  Because versions before Delphi 2006 don't support operator overloading, so
  I write two versions of all math operations for vector and matrix, etc. But
  later I found that the version without operator overloading runs faster.
  So if you want a better performance, DEFINE BETTER_PERFORMANCE in Physics2D.inc
  which will UNDEFINE OP_OVERLOAD even if you are using Delphi 2010.

  ###  This library supports three kinds of floats, Single(32bit), Double(64bit) and
  Extended(80bit). Double precision is always recommended because I will test all
  codes and demos using double precision. Using other float systems may cause some
  unpredictable exceptions.
       flags        EXTENDED_PRECISION        DOUBLE_PRECISION
     Extended               ON                     whatever
      Double(default)       OFF                       ON
      Single                OFF                       OFF
  There is also a flag SINGLE_PRECISION in the include file but it doesn't affect
  Float type definition.

  ###  Controllers are added as enhancement and can be flagged by CONTROLLERS.
  If you don't need them, please unflag to reduce code size.

  ###  If you want to do benchmark or something else, please flag COMPUTE_PHYSICS_TIME.
  Time consumed by each step is updated and stored in Tb2World.GetPhysicsTime.

  ###  All assertions are ignored.

  Translator: Qianyuan Wang(ÍõÇ¬Ôª)
  Contact me: http://hi.baidu.com/wqyfavor
              wqyfavor@qq.com
              QQ: 466798985
              Tweet: t.qq.com/wqyfavor
}

interface
{$I Physics2D.inc}

uses
   UPhysics2D, UPhysics2DTypes, Math, Classes;

// Change curve_tension to make the box a little round
function BuildStaticBoundBox(width, height, centerX, centerY: TPointFloat;
   world: Tb2World; curve_tension: Single = 0.0): Tb2Body;

// If bd is a dynamic body, @shape_density shouldn't be zero.
procedure BuildEdgeShapeCurve(pts: PPointF; cnt: Int32; bd: Tb2Body;
   precision: Single = 0.5; shape_density: Single = 0.0; curve_tension: Single = 0.5);

{ Build rope about a given curve.
  @body1 and @body2 are bodies linked to ends of the rope.
  If @bodies is not nil then it contains all rope element bodies created.
  @precision determins how smooth is the curve and @max_segment determins the max
  length each segment. The lower of both values, the smoother the rope is. }
procedure BuildRope(pts: PPointF; cnt: Int32; world: Tb2World; bodies: TList = nil;
   body1: Tb2Body = nil; body2: Tb2Body = nil; precision: Single = 0.5;
   max_segment: Single = 5; shape_density: Single = 1.0;
   shape_friction: Single = 0.0; curve_tension: Single = 0.5); overload;

// Build rope using two end points
procedure BuildRope(const p1, p2: TPointF; world: Tb2World; bodies: TList = nil;
   body1: Tb2Body = nil; body2: Tb2Body = nil; max_segment: Single = 5;
   shape_density: Single = 1.0; shape_friction: Single = 0.0); overload;

// radius is of the circumcircle of the polygon.
// edge must not be larger than b2_maxPolygonVertices
function BuildPentagonShape(radius: Single): Tb2PolygonShape; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function BuildHexagonShape(radius: Single): Tb2PolygonShape; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function BuildPolygonShape(radius: Single; edge: Integer): Tb2PolygonShape;

var
   LastGeneratedPoints: TPointsF;
   LastGeneratedPointCount: Integer;

implementation
var
   BezierPrecision: Single;

function BuildStaticBoundBox(width, height, centerX, centerY: TPointFloat;
   world: Tb2World; curve_tension: Single = 0.0): Tb2Body;
var
   // Wall feature points
   Walls: array[0..4] of TPointF;
   bd: Tb2BodyDef;
begin
   Walls[0].x := centerX - width / 2;
   Walls[0].y := centerY - height / 2;
   Walls[1].x := centerX - width / 2;
   Walls[1].y := centerY + height / 2;
   Walls[2].x := centerX + width / 2;
   Walls[2].y := centerY + height / 2;
   Walls[3].x := centerX + width / 2;
   Walls[3].y := centerY - height / 2;
   Walls[4].x := centerX - width / 2;
   Walls[4].y := centerY - height / 2;

   bd := Tb2BodyDef.Create;
   Result := world.CreateBody(bd);
   BuildEdgeShapeCurve(@Walls[0], 5, Result, 0.1, 0, curve_tension); // Change tension to make the box rectangle
end;

{ The following methods are translated into Pascal from ReactOS source.
    calc_curve_bezier_endp
    calc_curve_bezier
    BEZIERMIDDLE
    BezierCheck
    GDI_InternalBezier
    GDI_Bezier
    GenCurvePoints  }
// Calculates Bezier points from cardinal spline endpoints.
procedure calc_curve_bezier_endp(xend, yend, xadj, yadj, tension: Single;
   var x, y: Single);
begin
   // tangent at endpoints is the line from the endpoint to the adjacent point
   x := tension * (xadj - xend) + xend;
   y := tension * (yadj - yend) + yend;
end;

// Calculates Bezier points from cardinal spline points.
procedure calc_curve_bezier(const pts: TPointsF; tension: Single;
   var x1, y1, x2, y2: Single);
var
   xdiff, ydiff: Single;
begin
   // calculate tangent
   xdiff := pts[2].X - pts[0].X;
   ydiff := pts[2].Y - pts[0].Y;

   // apply tangent to get control points
   x1 := pts[1].X - tension * xdiff;
   y1 := pts[1].Y - tension * ydiff;
   x2 := pts[1].X + tension * xdiff;
   y2 := pts[1].Y + tension * ydiff;
end;

procedure BEZIERMIDDLE(var Mid: TPointF; const P1, P2: TPointF);
begin
   Mid.x := (P1.x + P2.x) / 2;
   Mid.y := (P1.y + P2.y) / 2;
end;

type
   TGDIBezierPoints = array[0..3] of TPointF;

{
* BezierCheck helper function to check
* that recursion can be terminated
*       Points[0] and Points[3] are begin and endpoint
*       Points[1] and Points[2] are control points
*       level is the recursion depth
*       returns true if the recusion can be terminated
}
function BezierCheck(level: Int32; const Points: TGDIBezierPoints): Boolean;
var
   dx, dy: Single;
begin
   dx := Points[3].x - Points[0].x;
   dy := Points[3].y - Points[0].y;
   if Abs(dy) <= Abs(dx) then // shallow line
   begin
      // check that control points are between begin and end
      if Points[1].x < Points[0].x then
      begin
         if Points[1].x < Points[3].x then
         begin
            Result := False;
            Exit;
         end;
      end
      else if Points[1].x > Points[3].x then
      begin
         Result := False;
         Exit;
      end;

      if Points[2].x < Points[0].x then
      begin
         if Points[2].x < Points[3].x then
         begin
            Result := False;
            Exit;
         end;
      end
      else if Points[2].x > Points[3].x then
      begin
         Result := False;
         Exit;
      end;

      if IsZero(dx) then
      begin
         Result := True;
         Exit;
      end;

      if (Abs(Points[1].y - Points[0].y - (dy / dx) * (Points[1].x - Points[0].x)) > BezierPrecision) or
         (Abs(Points[2].y - Points[0].y - (dy / dx) * (Points[2].x - Points[0].x)) > BezierPrecision) then
      begin
         Result := False;
         Exit;
      end
      else
      begin
         Result := True;
         Exit;
      end;
   end
   else
   begin // steep line
      // check that control points are between begin and end
      if Points[1].y < Points[0].y then
      begin
         if Points[1].y < Points[3].y then
         begin
            Result := False;
            Exit;
         end;
      end
      else if Points[1].y > Points[3].y then
      begin
         Result := False;
         Exit;
      end;

      if Points[2].y < Points[0].y then
      begin
         if Points[2].y < Points[3].y then
         begin
            Result := False;
            Exit;
         end;
      end
      else if Points[2].y > Points[3].y then
      begin
         Result := False;
         Exit;
      end;

      if IsZero(dy) then
      begin
         Result := True;
         Exit;
      end;

      if (Abs(Points[1].x - Points[0].x - (dx / dy) * (Points[1].y - Points[0].y)) > BezierPrecision) or
        (Abs(Points[2].x - Points[0].x - (dx / dy) * (Points[2].y - Points[0].y)) > BezierPrecision) then
      begin
         Result := False;
         Exit;
      end
      else
      begin
         Result := True;
         Exit;
      end;
   end;
end;

procedure GDI_InternalBezier(var Points: TGDIBezierPoints; var PtsOut: TPointsF;
   var dwOut, nPtsOut: Int32; level: Int32);
var
   Points2: TGDIBezierPoints; // for the second recursive call
begin
  if nPtsOut = dwOut then
  begin
     dwOut := dwOut * 2;
     SetLength(PtsOut, dwOut);
  end;

  if (level = 0) or BezierCheck(level, Points) then // Recursion can be terminated
  begin
     if nPtsOut = 0 then
     begin
        PtsOut[0] := Points[0];
        nPtsOut := 1;
     end;
     PtsOut[nPtsOut] := Points[3];
     Inc(nPtsOut);
  end
  else
  begin
     Points2[3] := Points[3];
     BEZIERMIDDLE(Points2[2], Points[2], Points[3]);
     BEZIERMIDDLE(Points2[0], Points[1], Points[2]);
     BEZIERMIDDLE(Points2[1],Points2[0],Points2[2]);

     BEZIERMIDDLE(Points[1], Points[0],  Points[1]);
     BEZIERMIDDLE(Points[2], Points[1], Points2[0]);
     BEZIERMIDDLE(Points[3], Points[2], Points2[1]);

     Points2[0] := Points[3];

     // do the two halves
     GDI_InternalBezier(Points, PtsOut, dwOut, nPtsOut, level - 1);
     GDI_InternalBezier(Points2, PtsOut, dwOut, nPtsOut, level - 1);
  end;
end;

procedure GDI_Bezier(const Points: TPointsF; count: Int32;
   var PtsOut: TPointsF; var nPtsOut:Int32);
var
   Bezier, dwOut: Int32;
   ptBuf: TGDIBezierPoints;
begin
   dwOut := 150;
   nPtsOut := 0;

   if (count - 1) mod 3 <> 0 then
      Exit;

   SetLength(PtsOut, dwOut);
   for Bezier := 0 to (count - 1) div 3 - 1 do
   begin
      Move(Points[Bezier * 3], ptBuf[0], SizeOf(ptBuf));
      GDI_InternalBezier(ptBuf, PtsOut, dwOut, nPtsOut, 8);
   end;
end;

procedure GenCurvePoints(const points: TPointsF; count: Int32; tension: Single = 0.5);
var
   i, len_pt: Int32;
   x1, x2, y1, y2: Single;
   pt: TPointsF;
begin
   LastGeneratedPointCount := 0;
   if count <= 1 then
      Exit;

   // PolyBezier expects count*3-2 points.
   len_pt := count * 3 - 2;
   SetLength(pt, len_pt);
   tension := tension * 0.3;

   calc_curve_bezier_endp(points[0].X, points[0].Y, points[1].X, points[1].Y,
      tension, x1, y1);

   pt[0] := points[0];
   pt[1].X := x1;
   pt[1].Y := y1;

   for i := 0 to count - 3 do
   begin
      calc_curve_bezier(TPointsF(@(points[i])), tension, x1, y1, x2, y2);
      pt[3 * i + 2].X := x1;
      pt[3 * i + 2].Y := y1;
      pt[3 * i + 3] := points[i + 1];
      pt[3 * i + 4].X := x2;
      pt[3 * i + 4].Y := y2;
   end;

   calc_curve_bezier_endp(points[count - 1].X, points[count - 1].Y,
       points[count - 2].X, points[count - 2].Y, tension, x1, y1);
   pt[len_pt - 2].X := x1;
   pt[len_pt - 2].Y := y1;
   pt[len_pt - 1] := points[count - 1];

   GDI_Bezier(pt, len_pt, LastGeneratedPoints, LastGeneratedPointCount);
end;

procedure BuildEdgeShapeCurve(pts: PPointF; cnt: Int32; bd: Tb2Body;
   precision: Single = 0.5; shape_density: Single = 0.0; curve_tension: Single = 0.5);
var
   i: Integer;
   x1, y1, x2, y2: Single;
   shape: Tb2EdgeShape;
begin
   if cnt <= 1 then
      Exit;

   BezierPrecision := precision;
   GenCurvePoints(TPointsF(pts), cnt, curve_tension);

   x1 := LastGeneratedPoints[0].x;
   y1 := LastGeneratedPoints[0].y;
   shape := Tb2EdgeShape.Create;
   for i := 1 to LastGeneratedPointCount - 1 do
   begin
      x2 := LastGeneratedPoints[i].x;
      y2 := LastGeneratedPoints[i].y;

      shape.SetVertices(MakeVector(x1, y1), MakeVector(x2, y2));
      bd.CreateFixture(shape, shape_density, False);

      x1 := x2;
      y1 := y2;
   end;
   shape.Free;
end;

procedure BuildRope(pts: PPointF; cnt: Int32; world: Tb2World; bodies: TList = nil;
   body1: Tb2Body = nil; body2: Tb2Body = nil; precision: Single = 0.5;
   max_segment: Single = 5; shape_density: Single = 1.0;
   shape_friction: Single = 0.0; curve_tension: Single = 0.5);

var
   bd: Tb2BodyDef;
   jd: Tb2RevoluteJointDef;
   fd: Tb2FixtureDef;
   body, prevBody: Tb2Body;
   anchor: TVector2;
   shape: Tb2EdgeShape;

   procedure Sub(var x1, y1: Single; const x2, y2: Single);
   begin
      SetValue(bd.position, (x1 + x2) / 2, (y1 + y2) / 2);
      shape.SetVertices(MakeVector((x1 - x2) / 2, (y1 - y2) / 2), MakeVector((x2 - x1) / 2, (y2 - y1) / 2));

      body := world.CreateBody(bd, False);
      body.CreateFixture(fd, False, False);

      if Assigned(bodies) then
         bodies.Add(body);

      if Assigned(prevBody) then
      begin
         SetValue(anchor, x1, y1);
         jd.Initialize(prevBody, body, anchor);
         world.CreateJoint(jd, False);
      end;

      prevBody := body;
      x1 := x2;
      y1 := y2;
   end;
var
   i, j, partition: Integer;
   x1, y1, x2, y2, l, dx, dy: Single;
begin
   if (not Assigned(world)) or (cnt <= 1) then
      Exit;

   BezierPrecision := precision;
   GenCurvePoints(TPointsF(pts), cnt, curve_tension);
   if LastGeneratedPointCount < 2 then
      Exit;

   shape := Tb2EdgeShape.Create;
   shape.m_edgeShapeMassed := True;
   fd := Tb2FixtureDef.Create;
   fd.shape := shape;
   fd.density := shape_density;
   fd.friction := shape_friction;

   jd := Tb2RevoluteJointDef.Create;
   jd.collideConnected := False;

   bd := Tb2BodyDef.Create;
   bd.bodyType := b2_dynamicBody;

   x1 := LastGeneratedPoints[0].x;
   y1 := LastGeneratedPoints[0].y;
   prevBody := body1;
   for i := 1 to LastGeneratedPointCount - 1 do
   begin
      x2 := LastGeneratedPoints[i].x;
      y2 := LastGeneratedPoints[i].y;

      { Though this segment can be treated as a line in view, but it needs further
        partition to make the rope more realistic. }
      l := Sqrt(Sqr(x1 - x2) + Sqr(y1 - y2)); // length of this segment
      if l > max_segment then
      begin
         partition := Floor(l / max_segment) + 1;
         dx := (x2 - x1) / partition;
         dy := (y2 - y1) / partition;
         for j := 1 to partition do
            Sub(x1, y1, x1 + dx, y1 + dy);
      end
      else
         Sub(x1, y1, x2, y2);
   end;

   if Assigned(body2) then
   begin
      SetValue(anchor, x1, y1);
      jd.Initialize(prevBody, body2, anchor);
      world.CreateJoint(jd, False);
   end;

   shape.Free;
   fd.Free;
   bd.Free;
   jd.Free;
end;

procedure BuildRope(const p1, p2: TPointF; world: Tb2World; bodies: TList = nil;
   body1: Tb2Body = nil; body2: Tb2Body = nil; max_segment: Single = 5;
   shape_density: Single = 1.0; shape_friction: Single = 0.0);
var
   pts: array[0..1] of TPointF;
begin
   pts[0] := p1;
   pts[1] := p2;
   BuildRope(@pts[0], 2, world, bodies, body1, body2, 1, max_segment,
      shape_density, shape_friction);
end;

function BuildPentagonShape(radius: Single): Tb2PolygonShape;
begin
   Result := BuildPolygonShape(radius, 5);
end;

function BuildHexagonShape(radius: Single): Tb2PolygonShape;
begin
   Result := BuildPolygonShape(radius, 6);
end;

function BuildPolygonShape(radius: Single; edge: Integer): Tb2PolygonShape;
var
   i: Integer;
   angle, delta: Single;
   vertices: Tb2PolyVertices;
begin
   Result := nil;
   if (edge >= 3) and (edge <= b2_maxPolygonVertices) then
   begin
      angle := 0.0;
      delta := 2 * Pi / edge;
      for i := 0 to edge - 1 do
      begin
         SetValue(vertices[i], radius * Cos(angle), radius * Sin(angle));
         angle := angle + delta;
      end;

      Result := Tb2PolygonShape.Create;
      Result.SetVertices(@vertices[0], edge);
   end;
end;

end.
