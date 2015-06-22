unit UPhysics2DPolygonTool;

{
 * Copyright (c) 2007 Eric Jordan
 *
 * This software is provided 'as-is', without any express or implied
 * warranty.  In no event will the authors be held liable for any damages
 * arising from the use of this software.
 * Permission is granted to anyone to use this software for any purpose,
 * including commercial applications, and to alter it and redistribute it
 * freely, subject to the following restrictions:
 * 1. The origin of this software must not be misrepresented; you must not
 * claim that you wrote the original software. If you use this software
 * in a product, an acknowledgment in the product documentation would be
 * appreciated but is not required.
 * 2. Altered source versions must be plainly marked as such, and must not be
 * misrepresented as being the original software.
 * 3. This notice may not be removed or altered from any source distribution.
}

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
   UPhysics2DTypes, UPhysics2D, SysUtils, Classes, Math;

const
   maxVerticesPerPolygon = b2_maxPolygonVertices;

type
   TTriangle = record
      x: TPointFloatArray;
      y: TPointFloatArray;

      {$IFDEF OP_OVERLOAD}
      procedure SetPoints(x1, y1, x2, y2, x3, y3: TPointFloat);
      function IsInside(_x, _y: TPointFloat): Boolean;
      {$ENDIF}
   end;
   TTriangleArray = array of TTriangle;

   TPolygon = class(TObject)
   public
      nVertices: Int32;
      x: TPointFloatArray;
      y: TPointFloatArray;

      area: Single;
      areaIsSet: Boolean;

      constructor Create; overload;
      constructor Create(const _x, _y: TPointFloatArray; _nVert: Int32); overload;
      constructor Create(const vs: TPointsF; _nVert: Int32); overload;
      constructor Create(const vs: TVectorArray; _nVert: Int32); overload;
      constructor Create(poly: TPolygon); overload;
      constructor Create(const tri: TTriangle); overload;

      procedure SetPoly(poly: TPolygon);

      function IsCCW: Boolean; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
      function IsUsable: Boolean;
      function IsConvex: Boolean;
      function IsSimple: Boolean;

      function GetArea: Single;
      function Add(tri: TTriangle): TPolygon;

      function Translate(dx, dy: TPointFloat): TPolygon;
      function GetVertices(var output: TPointsF): Int32; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

      procedure AddTo(fd: Tb2FixtureDef); // Remember to free fd.shape
      procedure MergeParallelEdges(tolerance: Single);
      procedure ReversePolygon; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
   end;

   TPolygonArray = array of TPolygon;

{$IFNDEF OP_OVERLOAD}
procedure SetPoints(var tri: TTriangle; x1, y1, x2, y2, x3, y3: TPointFloat);
function IsInside(const tri: TTriangle; _x, _y: TPointFloat): Boolean;
{$ENDIF}

procedure xyArrayToPointArray(const xv, yv: TPointFloatArray; vNum: Int32; var output: TPointsF);
procedure PointArrayToXYArray(const pArray: TPointsF; vNum: Int32; var xv, yv: TPointFloatArray);
procedure CopyFloatArray(const source: TPointFloatArray; var dest: TPointFloatArray; n: Int32);

function TriangulatePolygon(const xv, yv: TPointFloatArray; vNum: Int32; var output: TTriangleArray): Int32; overload;
function TriangulatePolygon(const v: TPointsF; vNum: Int32; var output: TTriangleArray): Int32; overload;

// Remember to free polys and note that Result is the real number polygons created. Result may differ from Length(polys)}
function PolygonizeTriangles(const triangulated: TTriangleArray;
   triangulatedLength: Int32; var polys: TPolygonArray): Int32;
function DecomposeConvex(p: TPolygon; var results: TPolygonArray): Int32; // Remember to free afterwards

function ConvexHull(const v: TPointsF; nVert: Int32): TPolygon; overload; // Remember to free afterwards
function ConvexHull(const cloudX, cloudY: TPointFloatArray; nVert: Int32): TPolygon; overload; // Remember to free afterwards

procedure ReversePolygon(var x, y: TPointFloatArray; n: Int32); overload;
procedure ReversePolygon(var v: TPointsF; n: Int32); overload;
function IsPolygonClockWise(const x, y: TPointFloatArray; n: Int32): Boolean; overload;
function IsPolygonClockWise(const v: TPointsF; n: Int32): Boolean; overload;
function IsPolygonConvex(const x, y: TPointFloatArray; n: Int32): Boolean; overload;
function IsPolygonConvex(const v: TPointsF; n: Int32): Boolean; overload;

implementation

//If you're using 1.4.3, b2_toiSlop won't exist, so set this equal to 0
const
   toiSlop = 0.0;
   MAX_CONNECTED = 32;
   COLLAPSE_DIST_SQR = FLT_EPSILON * FLT_EPSILON;//0.1f; //1000*FLT_EPSILON*1000*FLT_EPSILON;

/// Perform the cross product on two vectors. In 2D this produces a scalar.
function b2Cross(const a, b: TPointF): Double; overload;
begin
	 Result := a.x * b.y - a.y * b.x;
end;

/// Perform the cross product on a vector and a scalar. In 2D this produces a vector.
function b2Cross(const a: TPointF; s: Single): TPointF; overload;
begin
   Result.x := s * a.y;
   Result.y := -s * a.x;
end;

/// Perform the cross product on a scalar and a vector. In 2D this produces a vector.
function b2Cross(s: Single; const a: TPointF): TPointF; overload;
begin
   Result.x := -s * a.y;
   Result.y := s * a.x;
end;

/// Peform the dot product on two vectors.
function b2Dot(const a, b: TPointF): Double;
begin
   Result := a.x * b.x + a.y * b.y;
end;

function PolyCentroid(const vs: TPointsF; count: Int32): TPointF;
const
   inv3 = 1 / 3;
var
   i: Integer;
   pRef, p1, p2, p3, e1, e2: TPointF;
   area, triangleArea: Double;
begin
   area := 0.0;
   if count = 2 then
   begin
      Result.x := (vs[0].x + vs[1].x) / 2;
      Result.y := (vs[0].y + vs[1].y) / 2;
      Exit;
   end;

   Result.x := 0;
   Result.y := 0;
   pRef.x := 0;
   pRef.y := 0;

   for i := 0 to count - 1 do
   begin
      // Triangle vertices.
      p1 := pRef;
      p2 := vs[i];
      if i + 1 < count then
         p3 := vs[i + 1]
      else
         p3 := vs[0];

      e1.x := p2.x - p1.x;
      e1.y := p2.y - p1.y;
      e2.x := p3.x - p1.x;
      e2.y := p3.y - p1.y;

      triangleArea := 0.5 * b2Cross(e1, e2);
      area := area + triangleArea;

      // Area weighted centroid
      Result.x := Result.x + (p1.x + p2.x + p3.x) * triangleArea * inv3;
      Result.y := Result.y + (p1.y + p2.y + p3.y) * triangleArea * inv3;
   end;

   // Centroid
   Result.x := Result.x / area;
   Result.y := Result.y / area;
end;

{
 * Check if the lines a0->a1 and b0->b1 cross.
 * If they do, intersectionPoint will be filled
 * with the point of crossing.
 *
 * Grazing lines should not return true.
}
function intersect(const a0, a1, b0, b1: TPointF; var intersectionPoint: TPointF): Boolean; overload;
var
   x1, y1, x2, y2, x3, y3, x4, y4: Single;
   ua, ub, denom: Double;
begin
   if (Int64(a0) = Int64(b0)) or (Int64(a0) = Int64(b1)) or
      (Int64(a1) = Int64(b0)) or (Int64(a1) = Int64(b1)) then
   begin // Int64 equals TPointF in size
      Result := False;
      Exit;
   end;

   x1 := a0.x; y1 := a0.y;
   x2 := a1.x; y2 := a1.y;
   x3 := b0.x; y3 := b0.y;
   x4 := b1.x; y4 := b1.y;

   //AABB early exit
   if (b2Max(x1, x2) < b2Min(x3, x4)) or (b2Max(x3, x4) < b2Min(x1, x2)) or
      (b2Max(y1, y2) < b2Min(y3, y4)) or (b2Max(y3, y4) < b2Min(y1, y2)) then
   begin
      Result := False;
      Exit;
   end;

   ua := ((x4 - x3) * (y1 - y3) - (y4 - y3) * (x1 - x3));
   ub := ((x2 - x1) * (y1 - y3) - (y2 - y1) * (x1 - x3));
   denom := (y4 - y3) * (x2 - x1) - (x4 - x3) * (y2 - y1);
   if Abs(denom) < FLT_EPSILON then //Lines are too close to parallel to call
   begin
      Result := False;
      Exit;
   end;

   ua := ua / denom;
   ub := ub / denom;

   if (0 < ua) and (ua < 1) and (0 < ub) and (ub < 1) then
   begin
      intersectionPoint.x := (x1 + ua * (x2 - x1));
      intersectionPoint.y := (y1 + ua * (y2 - y1));
      Result := True;
   end
   else
      Result := False;
end;

// True if line from a0->a1 intersects b0->b1
function intersect(const a0, a1, b0, b1: TPointF): Boolean; overload;
var
   myVec: TPointF;
begin
   Result := intersect(a0, a1, b0, b1, myVec);
end;

procedure CopyFloatArray(const source: TPointFloatArray; var dest: TPointFloatArray; n: Int32);
begin
   SetLength(dest, n);
   Move(source[0], dest[0], SizeOf(TPointFloat) * n);
end;

procedure xyArrayToPointArray(const xv, yv: TPointFloatArray; vNum: Int32; var output: TPointsF);
var
   i: Integer;
begin
   SetLength(output, vNum);
   for i := 0 to vNum - 1 do
   begin
      output[i].x := xv[i];
      output[i].y := yv[i];
   end;
end;

procedure PointArrayToXYArray(const pArray: TPointsF; vNum: Int32; var xv, yv: TPointFloatArray);
var
   i: Integer;
begin
   SetLength(xv, vNum);
   SetLength(yv, vNum);
   for i := 0 to vNum - 1 do
   begin
      xv[i] := pArray[i].x;
      yv[i] := pArray[i].y;
   end;
end;

//Fix for obnoxious behavior for the % operator for negative numbers...
function remainder(x, modulus: Int32): Int32;
begin
   Result := x mod modulus;
   while Result < 0 do
      Result := Result + modulus;
end;

function IsEar(i: Int32; const xv, yv: TPointFloatArray; xvLength: Int32): Boolean;
var
   dx0, dy0, dx1, dy1: TPointFloat;
   upper, lower: Integer;
   cross: Double;
   myTri: TTriangle;
   j: Integer;
begin
   if (i >= xvLength) or (i < 0) or (xvLength < 3) then
   begin
      Result := False;
      Exit;
   end;

   upper := i + 1;
   lower := i - 1;

   if i = 0 then
   begin
      dx0 := xv[0] - xv[xvLength - 1];
      dy0 := yv[0] - yv[xvLength - 1];
      dx1 := xv[1] - xv[0];
      dy1 := yv[1] - yv[0];
      lower := xvLength - 1;
   end
   else
      if i = xvLength - 1 then
      begin
         dx0 := xv[i] - xv[i - 1];
         dy0 := yv[i] - yv[i - 1];
         dx1 := xv[0] - xv[i];
         dy1 := yv[0] - yv[i];
         upper := 0;
      end
      else
      begin
         dx0 := xv[i] - xv[i - 1];
         dy0 := yv[i] - yv[i - 1];
         dx1 := xv[i + 1] - xv[i];
         dy1 := yv[i + 1] - yv[i];
      end;

   cross := (dx0 * dy1) - (dx1 * dy0);
   if cross > 0 then
   begin
      Result := False;
      Exit;
   end;

   {$IFDEF OP_OVERLOAD}
   myTri.SetPoints(xv[i], yv[i], xv[upper], yv[upper], xv[lower], yv[lower]);
   {$ELSE}
   SetPoints(myTri, xv[i], yv[i], xv[upper], yv[upper], xv[lower], yv[lower]);
   {$ENDIF}

   for j := 0 to xvLength - 1 do
      if not ((j = i) or (j = lower) or (j = upper)) then
         {$IFDEF OP_OVERLOAD}
         if myTri.IsInside(xv[j], yv[j]) then
         {$ELSE}
         if IsInside(myTri, xv[j], yv[j]) then
         {$ENDIF}
         begin
            Result := False;
            Exit;
         end;

   Result := True;
end;

function TriangulatePolygon(const v: TPointsF; vNum: Int32;
   var output: TTriangleArray): Int32; overload;
var
   xA, yA: TPointFloatArray;
begin
   PointArrayToXYArray(v, vNum, xA, yA);
   Result := TriangulatePolygon(xA, yA, vNum, output);
end;

{
  * Triangulates a polygon using simple ear-clipping algorithm. Returns
  * size of Triangle array unless the polygon can't be triangulated.
  * This should only happen if the polygon self-intersects,
  * though it will not _always_ return null for a bad polygon - it is the
  * caller's responsibility to check for self-intersection, and if it
  * doesn't, it should at least check that the return value is non-null
  * before using. You're warned!
*
* Triangles may be degenerate, especially if you have identical points
* in the input to the algorithm.  Check this before you use them.
  *
  * This is totally unoptimized, so for large polygons it should not be part
  * of the simulation loop.
  *
  * Returns:
  * -1 if algorithm fails (self-intersection most likely)
  * 0 if there are not enough vertices to triangulate anything.
  * Number of triangles if triangulation was successful.
  *
  * results will be filled with results - ear clipping always creates vNum - 2
  * or fewer (due to pinch point polygon snipping), so allocate an array of
* this size.
}

function TriangulatePolygon(const xv, yv: TPointFloatArray; vNum: Int32;
   var output: TTriangleArray): Int32; overload;
var
   i: Integer;
   buffer: TTriangleArray;
   bufferSize, itemCount: Integer;
   xrem, yrem: TPointFloatArray;
   earIndex: Integer;
   newx, newy: TPointFloatArray;
   currDest: Integer;
   under, over: Integer;
   toAdd, toAddMore: TTriangle;
begin
   if vNum < 3 then
   begin
      Result := 0;
      Exit;
   end;

   itemCount := 0;
   bufferSize := 0;
   CopyFloatArray(xv, xrem, vNum);
   CopyFloatArray(yv, yrem, vNum);

   while vNum > 3 do
   begin
      earindex := -1;
      for i := 0 to vNum - 1 do
         if IsEar(i, xrem, yrem, Length(xrem)) then
         begin
            earIndex := i;
            Break;
         end;

      if earIndex = -1 then
      begin
         Result := 0;
         Exit;
      end;

      Dec(vNum);
      currDest := 0;
      SetLength(newx, vNum);
      SetLength(newy, vNum);
      for i := 0 to vNum - 1 do
      begin
         if currDest = earIndex then
            Inc(currDest);
         newx[i] := xrem[currDest];
         newy[i] := yrem[currDest];
         Inc(currDest);
      end;

      if earIndex = 0 then
         under := Length(xrem) - 1
      else
         under := earIndex - 1;
      if earindex = Length(xrem) - 1 then
         over := 0
      else
         over := earIndex + 1;

      {$IFDEF OP_OVERLOAD}
      toAdd.SetPoints(xrem[earIndex], yrem[earIndex], xrem[over],
         yrem[over], xrem[under], yrem[under]);
      {$ELSE}
      SetPoints(toAdd, xrem[earIndex], yrem[earIndex], xrem[over],
         yrem[over], xrem[under], yrem[under]);
      {$ENDIF}

      Inc(itemCount);
      if bufferSize < itemCount then
      begin
         Inc(bufferSize, 5);
         SetLength(buffer, bufferSize);
      end;
      buffer[itemCount - 1] := toAdd;

      xrem := newx;
      yrem := newy;
   end;

   {$IFDEF OP_OVERLOAD}
   toAddMore.SetPoints(xrem[1], yrem[1], xrem[2], yrem[2], xrem[0], yrem[0]);
   {$ELSE}
   SetPoints(toAddMore, xrem[1], yrem[1], xrem[2], yrem[2], xrem[0], yrem[0]);
   {$ENDIF}
   if Length(output) < itemCount + 1 then
      SetLength(output, itemCount + 1); // else do not change output size
   for i := 0 to itemCount - 1 do
      output[i] := buffer[i];
   output[itemCount] := toAddMore;

   Result := itemCount + 1;
end;

{
* Turns a list of triangles into a list of convex polygons. Very simple
  * method - start with a seed triangle, keep adding triangles to it until
  * you can't add any more without making the polygon non-convex.
  *
  * Returns an integer telling how many polygons were created.  Will fill
  * polys array up to polysLength entries, which may be smaller or larger
  * than the return value.
  *
  * Takes O(N*P) where P is the number of resultant polygons, N is triangle
  * count.
  *
  * The final polygon list will not necessarily be minimal, though in
  * practice it works fairly well.
}

function PolygonizeTriangles(const triangulated: TTriangleArray;
   triangulatedLength: Int32; var polys: TPolygonArray): Int32;
var
   i, polyIndex, bufSize: Integer;
   newP, poly: TPolygon;
   covered: array of Boolean;
   currTri: Integer;
   notDone: Boolean;
begin
   if not Assigned(triangulated) then
   begin
      Result := 0;
      Exit;
   end;

   polyIndex := 0;
   bufSize := 0;
   SetLength(covered, triangulatedLength);
   for i := 0 to triangulatedLength - 1 do
   begin
      covered[i] := False;
      //Check here for degenerate triangles
      if ((triangulated[i].x[0] = triangulated[i].x[1]) and (triangulated[i].y[0] = triangulated[i].y[1]))
         or ((triangulated[i].x[1] = triangulated[i].x[2]) and (triangulated[i].y[1] = triangulated[i].y[2]))
         or ((triangulated[i].x[0] = triangulated[i].x[2]) and (triangulated[i].y[0] = triangulated[i].y[2])) then
         covered[i] := True;
   end;
   notDone := True;
   while notDone do
   begin
      currTri := -1;
      for i := 0 to triangulatedLength - 1 do
      begin
         if covered[i] then
            Continue;
         currTri := i;
         Break;
      end;

      if currTri = -1 then
         notDone := false
      else
      begin
         poly := TPolygon.Create(triangulated[currTri]);
         covered[currTri] := true;
         for i := 0 to triangulatedLength - 1 do
         begin
            if covered[i] then
               continue;
            newP := poly.add(triangulated[i]);
              // Box2D 2.01
              // haXe version is > 7 for some reason. Box2D's limit is 8
             //   if not assigned(newP) or (system.length(newP.x) > 8) then continue;

             // No limit in Box2D 2.1 on number of polygons, but will stick to 8
             // for the time being
            if not assigned(newP) then
               Continue;

            if newP.nVertices > maxVerticesPerPolygon then
            begin
               newP.Free;
               Continue;
            end;

            if newP.isConvex then
            begin
               poly.Free;
               poly := newP;
               covered[i] := true;
            end
            else
               newP.Free;
         end;

         Inc(polyIndex);
         if bufSize < polyIndex then
         begin
            Inc(bufSize, 5);
            SetLength(polys, bufSize);
         end;
         polys[polyIndex - 1] := poly;
      end;
   end;
   Result := polyIndex;
end;

{
* Decomposes a non-convex polygon into a number of convex polygons, up
  * to maxPolys (remaining pieces are thrown out, but the total number
* is returned, so the return value can be greater than maxPolys).
  *
  * Each resulting polygon will have no more than maxVerticesPerPolygon
  * vertices (set to b2MaxPolyVertices by default, though you can change
* this).
  *
  * Returns -1 if operation fails (usually due to self-intersection of
* polygon).
}
function DecomposeConvex(p: TPolygon; var results: TPolygonArray): Int32;
var
   nTri: Int32;
   triangulated: TTriangleArray;
   tempP: TPolygon;
begin
   if p.nVertices < 3 then
   begin
      Result := 0;
      Exit;
   end;

   if p.IsCCW then
   begin
      tempP := TPolygon.Create;
      tempP.SetPoly(p);
      ReversePolygon(tempP.x, tempP.y, tempP.nVertices);
      nTri := TriangulatePolygon(tempP.x, tempP.y, tempP.nVertices, triangulated);
      // ReversePolygon(p.x, p.y, p.nVertices); //reset orientation
   end
   else
      nTri := TriangulatePolygon(p.x, p.y, p.nVertices, triangulated);

   if nTri < 1 then
   begin
      Result := -1;
      Exit;
   end;

   Result := PolygonizeTriangles(triangulated, nTri, results);
end;

{
* Find the convex hull of a point cloud using "Gift-wrap" algorithm - start
  * with an extremal point, and walk around the outside edge by testing
  * angles.
  *
  * Runs in O(N*S) time where S is number of sides of resulting polygon.
  * Worst case: point cloud is all vertices of convex polygon . O(N^2).
  *
  * There may be faster algorithms to do this, should you need one -
  * this is just the simplest. You can get O(N log N) expected time if you
  * try, I think, and O(N) if you restrict inputs to simple polygons.
  *
  * Returns null if number of vertices passed is less than 3.
  *
* Results should be passed through convex decomposition afterwards
* to ensure that each shape has few enough points to be used in Box2d.
*
  * FIXME?: May be buggy with colinear points on hull. Couldn't find a test
  * case that resulted in wrong behavior. If one turns up, the solution is to
  * supplement angle check with an equality resolver that always picks the
  * longer edge. I think the current solution is working, though it sometimes
  * creates an extra edge along a line.
}
function ConvexHull(const v: TPointsF; nVert: Int32): TPolygon;
var
   cloudX, cloudY: TPointFloatArray;
begin
   PointArrayToXYArray(v, nVert, cloudX, cloudY);
   Result := ConvexHull(cloudX, cloudY, nVert);
end;

function ConvexHull(const cloudX, cloudY: TPointFloatArray; nVert: Int32): TPolygon;
var
   i: Integer;
   edgeList: array of Int32;
   numEdges, minYIndex, startIndex, winIndex: Int32;
   minY, dx, dy, newdx, newdy, maxDot, newDot, nrm: Double;
   xres, yres: TPointFloatArray;
begin
   if nVert <= 2 then
   begin
      Result := nil;
      Exit;
   end;

   SetLength(edgeList, nVert);
   numEdges := 0;

   minY := Infinity;
   minYIndex := nVert;
   for i := 0 to nVert - 1 do
   begin
      if cloudY[i] < minY then
      begin
         minY := cloudY[i];
         minYIndex := i;
      end;
   end;

   startIndex := minYIndex;
   winIndex := -1;
   dx := -1.0;
   dy := 0.0;
   while winIndex <> minYIndex do
   begin
      maxDot := -2.0;
      for i := 0 to nVert - 1 do
      begin
         if i = startIndex then
            Continue;

         newdx := cloudX[i] - cloudX[startIndex];
         newdy := cloudY[i] - cloudY[startIndex];
         nrm := Sqrt(newdx * newdx + newdy * newdy);
         if nrm = 0.0 then
            nrm := 1.0;
         newdx := newdx / nrm;
         newdy := newdy / nrm;

         //Cross and dot products act as proxy for angle
         //without requiring inverse trig.
         //FIXED: don't need cross test
         //float32 newCross := newdx * dy - newdy * dx;
         newDot := newdx * dx + newdy * dy;
         if newDot > maxDot then //newCross >= 0.0f && newDot > maxDot)
         begin
             maxDot := newDot;
             winIndex := i;
         end;
      end;
      edgeList[numEdges] := winIndex;
      Inc(numEdges);
      dx := cloudX[winIndex] - cloudX[startIndex];
      dy := cloudY[winIndex] - cloudY[startIndex];
      nrm := Sqrt(dx * dx + dy * dy);
      if nrm = 0.0 then
         nrm := 1.0;
      dx := dx / nrm;
      dy := dy / nrm;
      startIndex := winIndex;
   end;

   SetLength(xres, numEdges);
   SetLength(yres, numEdges);
   for i := 0 to numEdges - 1 do
   begin
       xres[i] := cloudX[edgeList[i]];
       yres[i] := cloudY[edgeList[i]];
   end;

   Result := TPolygon.Create(xres, yres, numEdges);
   Result.MergeParallelEdges(b2_angularSlop);
end;

procedure ReversePolygon(var x, y: TPointFloatArray; n: Int32);
var
   low, high: Integer;
   buffer: Single;
begin
   if n = 1 then
      Exit;

   low := 0;
   high := n - 1;
   while (low < high) do
   begin
      buffer := x[low];
      x[low] := x[high];
      x[high] := buffer;
      buffer := y[low];
      y[low] := y[high];
      y[high] := buffer;
      Inc(low);
      Dec(high);
   end;
end;

procedure ReversePolygon(var v: TPointsF; n: Int32);
var
   low, high: Integer;
   buffer: TPointF;
begin
   if n = 1 then
      Exit;
   low := 0;
   high := n - 1;
   while (low < high) do
   begin
      buffer := v[low];
      v[low] := v[high];
      v[high] := buffer;
      Inc(low);
      Dec(high);
   end;
end;

function IsPolygonClockWise(const x, y: TPointFloatArray; n: Int32): Boolean;
var
   p: TPolygon;
begin
   p := TPolygon.Create(x, y, n);
   Result := not p.IsCCW;
   p.Free;
end;

function IsPolygonClockWise(const v: TPointsF; n: Int32): Boolean;
var
   p: TPolygon;
begin
   p := TPolygon.Create(v, n);
   Result := not p.IsCCW;
   p.Free;
end;

function IsPolygonConvex(const x, y: TPointFloatArray; n: Int32): Boolean;
var
   p: TPolygon;
begin
   p := TPolygon.Create(x, y, n);
   Result := p.IsConvex;
   p.Free;
end;

function IsPolygonConvex(const v: TPointsF; n: Int32): Boolean;
var
   p: TPolygon;
begin
   p := TPolygon.Create(v, n);
   Result := p.IsConvex;
   p.Free;
end;

{ TTriangle }

{$IFDEF OP_OVERLOAD}
procedure TTriangle.SetPoints(x1, y1, x2, y2, x3, y3: TPointFloat);
var
   dx1, dx2, dy1, dy2: TPointFloat;
begin
   SetLength(x, 3);
   SetLength(y, 3);
   dx1 := x2 - x1;
   dx2 := x3 - x1;
   dy1 := y2 - y1;
   dy2 := y3 - y1;
   if (dx1 * dy2) - (dx2 * dy1) > 0 then // ccw
   begin
      x[0] := x1;
      x[1] := x2;
      x[2] := x3;
      y[0] := y1;
      y[1] := y2;
      y[2] := y3;
   end
   else
   begin
      x[0] := x1;
      x[1] := x3;
      x[2] := x2;
      y[0] := y1;
      y[1] := y3;
      y[2] := y2;
   end;
end;

function TTriangle.IsInside(_x, _y: TPointFloat): Boolean;
var
   vx0, vx1, vx2: TPointFloat;
   vy0, vy1, vy2: TPointFloat;
   dot00, dot01, dot02, dot11, dot12: TPointFloat;
   invDenom: TPointFloat;
   u, v: TPointFloat;
begin
   if ((_x < x[0]) and (_x < x[1]) and (_x < x[2])) or
      ((_x > x[0]) and (_x > x[1]) and (_x > x[2])) or
      ((_y < y[0]) and (_y < y[1]) and (_y < y[2])) or
      ((_y > y[0]) and (_y > y[1]) and (_y > y[2])) then
   begin
      Result := False;
      Exit;
   end;

   vx2 := _x - x[0];
   vy2 := _y - y[0];
   vx1 := x[1] - x[0];
   vy1 := y[1] - y[0];
   vx0 := x[2] - x[0];
   vy0 := y[2] - y[0];
   dot00 := vx0 * vx0 + vy0 * vy0;
   dot01 := vx0 * vx1 + vy0 * vy1;
   dot02 := vx0 * vx2 + vy0 * vy2;
   dot11 := vx1 * vx1 + vy1 * vy1;
   dot12 := vx1 * vx2 + vy1 * vy2;
   if Abs(dot00 * dot11 - dot01 * dot01) < 0.001 then
      invDenom := 0.00
   else
      invDenom := 1.0 / (dot00 * dot11 - dot01 * dot01);
   u := (dot11 * dot02 - dot01 * dot12) * invDenom;
   v := (dot00 * dot12 - dot01 * dot02) * invDenom;
   Result := ((u > 0) and (v > 0) and (u + v < 1));
end;
{$ELSE}
procedure SetPoints(var tri: TTriangle; x1, y1, x2, y2, x3, y3: TPointFloat);
var
   dx1, dx2, dy1, dy2: TPointFloat;
begin
   with tri do
   begin
      SetLength(x, 3);
      SetLength(y, 3);
      dx1 := x2 - x1;
      dx2 := x3 - x1;
      dy1 := y2 - y1;
      dy2 := y3 - y1;
      if (dx1 * dy2) - (dx2 * dy1) > 0 then // ccw
      begin
         x[0] := x1;
         x[1] := x2;
         x[2] := x3;
         y[0] := y1;
         y[1] := y2;
         y[2] := y3;
      end
      else
      begin
         x[0] := x1;
         x[1] := x3;
         x[2] := x2;
         y[0] := y1;
         y[1] := y3;
         y[2] := y2;
      end;
   end;
end;

function IsInside(const tri: TTriangle; _x, _y: TPointFloat): Boolean;
var
   vx0, vx1, vx2: TPointFloat;
   vy0, vy1, vy2: TPointFloat;
   dot00, dot01, dot02, dot11, dot12: TPointFloat;
   invDenom: TPointFloat;
   u, v: TPointFloat;
begin
   with tri do
   begin
      if ((_x < x[0]) and (_x < x[1]) and (_x < x[2])) or
         ((_x > x[0]) and (_x > x[1]) and (_x > x[2])) or
         ((_y < y[0]) and (_y < y[1]) and (_y < y[2])) or
         ((_y > y[0]) and (_y > y[1]) and (_y > y[2])) then
      begin
         Result := False;
         Exit;
      end;

      vx2 := _x - x[0];
      vy2 := _y - y[0];
      vx1 := x[1] - x[0];
      vy1 := y[1] - y[0];
      vx0 := x[2] - x[0];
      vy0 := y[2] - y[0];
      dot00 := vx0 * vx0 + vy0 * vy0;
      dot01 := vx0 * vx1 + vy0 * vy1;
      dot02 := vx0 * vx2 + vy0 * vy2;
      dot11 := vx1 * vx1 + vy1 * vy1;
      dot12 := vx1 * vx2 + vy1 * vy2;
      if Abs(dot00 * dot11 - dot01 * dot01) < 0.001 then
         invDenom := 0.00
      else
         invDenom := 1.0 / (dot00 * dot11 - dot01 * dot01);
      u := (dot11 * dot02 - dot01 * dot12) * invDenom;
      v := (dot00 * dot12 - dot01 * dot02) * invDenom;
      Result := ((u > 0) and (v > 0) and (u + v < 1));
   end;
end;
{$ENDIF}

{ TPolygon }

constructor TPolygon.Create;
begin
   nVertices := 0;
   areaIsSet := False;
end;

constructor TPolygon.Create(const _x, _y: TPointFloatArray; _nVert: Int32);
begin
   areaIsSet := False;
   nVertices := _nVert;
   CopyFloatArray(_x, x, nVertices);
   CopyFloatArray(_y, y, nVertices);
end;

constructor TPolygon.Create(const vs: TPointsF; _nVert: Int32);
begin
   areaIsSet := False;
   nVertices := _nVert;
   PointArrayToXYArray(vs, _nVert, x, y);
end;

constructor TPolygon.Create(const vs: TVectorArray; _nVert: Int32);
var
   i: Integer;
begin
   areaIsSet := False;

   nVertices := _nVert;
   SetLength(x, nVertices);
   SetLength(y, nVertices);

   for i := 0 to nVertices - 1 do
   begin
      x[i] := vs[i].x;
      y[i] := vs[i].y;
   end;
end;

constructor TPolygon.Create(poly: TPolygon);
begin
   SetPoly(poly);
end;

constructor TPolygon.Create(const tri: TTriangle);
var
   i: Integer;
begin
   areaIsSet := False;

   nVertices := 3;
   SetLength(x, nVertices);
   SetLength(y, nVertices);

   for i := 0 to nVertices - 1 do
   begin
      x[i] := tri.x[i];
      y[i] := tri.y[i];
   end;
end;

procedure TPolygon.SetPoly(poly: TPolygon);
begin
   nVertices := poly.nVertices;
   CopyFloatArray(poly.x, x, nVertices);
   CopyFloatArray(poly.y, y, nVertices);
   area := poly.area;
   areaIsSet := poly.areaIsSet;
end;

function TPolygon.IsCCW: Boolean;
begin
   Result := GetArea() > 0.0;
end;

{
 * Checks if polygon is valid for use in Box2d engine.
 * Last ditch effort to ensure no invalid polygons are
 * added to world geometry.
 *
 * Performs a full check, for simplicity, convexity,
 * orientation, minimum angle, and volume.  This won't
 * be very efficient, and a lot of it is redundant when
 * other tools in this section are used.
}
function TPolygon.IsUsable: Boolean;
var
   i, j, i2, iminus: Integer;
   error: Int32;
   noError: Boolean;
   centroid, n1, n2, v: TPointF;
   normals, vertices: TPointsF;
   edge, tmpV, d: TVector2;
   cross: PhysicsFloat;
begin
   error := -1;
   noError := True;
   if (nVertices < 3) or (nVertices > b2_maxPolygonVertices) then
   begin
      noError := False;
      error := 0;
   end;

   if not IsConvex then
   begin
      noError := False;
      error := 1;
   end;

   if not IsSimple then
   begin
      noError := False;
      error := 2;
   end;

   if GetArea() < FLT_EPSILON then
   begin
      noError := False;
      error := 3;
   end;

   //Compute normals
   SetLength(normals, nVertices);
   SetLength(vertices, nVertices);

   for i := 0 to nVertices - 1 do
   begin
      vertices[i].x := x[i];
      vertices[i].y := y[i];

      if i + 1 < nVertices then
         i2 := i + 1
      else
         i2 := 0;
      edge.x := x[i2] - x[i];
      edge.y := y[i2] - y[i];
      tmpV := b2Cross(edge, 1.0);
      {$IFDEF OP_OVERLOAD}
      tmpV.Normalize;
      {$ELSE}
      Normalize(tmpV);
      {$ENDIF}
      normals[i].x := tmpV.x;
      normals[i].y := tmpV.y;
   end;

   //Required side checks
   for i := 0 to nVertices - 1 do
   begin
      if i = 0 then
         iminus := nVertices - 1
      else
         iminus := i - 1;

      //int32 iplus := (i==nVertices-1)?0:i+1;

      //Parallel sides check
      cross := b2Cross(normals[iminus], normals[i]);
      cross := b2Clamp(cross, -1.0, 1.0);
      if ArcSin(cross) <= b2_angularSlop then
      begin
         noError := False;
         error := 4;
         Break;
      end;

      //Too skinny check
      for j := 0 to nVertices - 1 do
      begin
         if (j = i) or (j = (i + 1) mod nVertices) then
            Continue;

         v.x := vertices[j].x - vertices[i].x;
         v.y := vertices[j].y - vertices[i].y;
         if b2Dot(normals[i], v) >= -b2_linearSlop then
         begin
            noError := False;
            error := 5;
         end;
      end;

      centroid := PolyCentroid(vertices, nVertices);
      n1 := normals[iminus];
      n2 := normals[i];
      v.x := vertices[i].x - centroid.x;
      v.y := vertices[i].y - centroid.y;

      d.x := b2Dot(n1, v) - toiSlop;
      d.y := b2Dot(n2, v) - toiSlop;

      // Shifting the edge inward by b2_toiSlop should
      // not cause the plane to pass the centroid.
      if (d.x < 0.0) or (d.y < 0.0) then
      begin
         noError := False;
         error := 6;
      end;
   end;

  (* if (not noError) && printErrors){
     printf("Found invalid polygon, ");
     switch(error){
       case 0:
         printf("must have between 3 and %d vertices.\n",b2_maxPolygonVertices);
         Break;
       case 1:
         printf("must be convex.\n");
         Break;
       case 2:
         printf("must be simple (cannot intersect itself).\n");
         Break;
       case 3:
         printf("area is too small.\n");
         Break;
       case 4:
         printf("sides are too close to parallel.\n");
         Break;
       case 5:
         printf("polygon is too thin.\n");
         Break;
       case 6:
         printf("core shape generation would move edge past centroid (too thin).\n");
         Break;
       default:
         printf("don't know why.\n");
     }
   }       *)
   Result := noError;
end;

function TPolygon.IsConvex: Boolean;
var
   isPositive: Boolean;
   i: Integer;
   lower, middle, upper: Integer;
   dx0, dy0, dx1, dy1: TPointFloat;
   cross: TPointFloat;
   newIsP: Boolean;
begin
   isPositive := False;
   for i := 0 to nVertices - 1 do
   begin
      if i = 0 then
         lower := nVertices - 1
      else
         lower := i - 1;
      middle := i;
      if i = nVertices - 1 then
         upper := 0
      else
         upper := i + 1;

      dx0 := x[middle] - x[lower];
      dy0 := y[middle] - y[lower];
      dx1 := x[upper] - x[middle];
      dy1 := y[upper] - y[middle];
      cross := dx0 * dy1 - dx1 * dy0;
      // Cross product should have same sign
      // for each vertex if poly is convex.
      if cross > 0 then
         newIsP := True
      else
         newIsP := False;
      if i = 0 then
         isPositive := newIsP
      else
         if (isPositive <> newIsP) then
         begin
            Result := False;
            Exit;
         end;
   end;
   Result := True;
end;

//Check for edge crossings
function TPolygon.IsSimple: Boolean;
var
   i, j, iplus, jplus: Integer;
   a1, a2, b1, b2: TPointF;
begin
   for i := 0 to nVertices - 1 do
   begin
      if i + 1 > nVertices - 1 then
         iplus := 0
      else
         iplus := i + 1;

      a1.x := x[i];
      a1.y := y[i];
      a2.x := x[iplus];
      a2.y := y[iplus];

      for j := i + 1 to nVertices - 1 do
      begin
         if j + 1 > nVertices - 1 then
            jplus := 0
         else
            jplus := j + 1;

         b1.x := x[j];
         b1.y := y[j];
         b2.x := x[jplus];
         b2.y := y[jplus];

         if intersect(a1, a2, b1, b2) then
         begin
            Result := False;
            Exit;
         end;
      end;
	 end;
   Result := True;
end;

function TPolygon.GetArea: Single;
var
   i: Integer;
begin
   area := 0.0;

   //First do wraparound
   area := area + x[nVertices - 1] * y[0] - x[0] * y[nVertices - 1];
   for i := 0 to nVertices - 1 do
      area := area + x[i] * y[i + 1] - x[i + 1] * y[i];
   area := area * 0.5;
   areaIsSet := True;
   Result := area;
end;

{
* Tries to add a triangle to the polygon. Returns null if it can't connect
* properly, otherwise returns a pointer to the new Polygon. Assumes bitwise
* equality of joined vertex positions.
*
* Remember to delete the pointer afterwards.
* Todo: Make this return a b2Polygon instead
* of a pointer to a heap-allocated one.
*
* For internal use.
}
function TPolygon.Add(tri: TTriangle): TPolygon;
var
   firstP, secondP, firstT, secondT: Integer;
   i: Integer;
   tipT, currOut: Integer;
   newX, newY: TPointFloatArray;
begin
   //First, find vertices that connect
   firstP := -1;
   firstT := -1;
   secondP := -1;
   secondT := -1;

   for i := 0 to nVertices - 1 do
   begin
      if (tri.x[0] = x[i]) and (tri.y[0] = y[i]) then
      begin
         if firstP = -1 then
         begin
            firstP := i;
            firstT := 0;
         end
         else
         begin
            secondP := i;
            secondT := 0;
         end;
      end
      else
         if (tri.x[1] = x[i]) and (tri.y[1] = y[i]) then
         begin
            if firstP = -1 then
            begin
               firstP := i;
               firstT := 1;
            end
            else
            begin
               secondP := i;
               secondT := 1;
            end;
         end
         else
            if (tri.x[2] = x[i]) and (tri.y[2] = y[i]) then
            begin
               if firstP = -1 then
               begin
                  firstP := i;
                  firstT := 2;
               end
               else
               begin
                  secondP := i;
                  secondT := 2;
               end;
            end;
   end;

   //Fix ordering if first should be last vertex of poly
   if (firstP = 0) and (secondP = nVertices - 1) then
   begin
      firstP := nVertices - 1;
      secondP := 0;
   end;

   //Didn't find it
   if (secondP = -1) then
   begin
      Result := nil;
      Exit;
   end;

   //Find tip index on triangle
   tipT := 0;
   // Must be typing error -- like this in PDE, AS, and haXe
   if (tipT = firstT) or (tipT = secondT) then
      tipT := 1;
   if (tipT = firstT) or (tipT = secondT) then
      tipT := 2;

   SetLength(newx, nVertices + 1);
   SetLength(newy, nVertices + 1);

   currOut := 0;
   for i := 0 to nVertices - 1 do
   begin
      newX[currOut] := x[i];
      newY[currOut] := y[i];
      if i = firstP then
      begin
         Inc(currOut);
         newX[currOut] := tri.x[tipT];
         newY[currOut] := tri.y[tipT];
      end;
      Inc(currOut);
   end;
   Result := TPolygon.Create(newX, newY, nVertices + 1);
end;

function TPolygon.Translate(dx, dy: TPointFloat): TPolygon;
var
   i: Integer;
begin
   Result := TPolygon.Create(Self);
   for i := 0 to nVertices - 1 do
   begin
      Result.x[i] := x[i] + dx;
      Result.y[i] := y[i] + dy;
   end;
end;

function TPolygon.GetVertices(var output: TPointsF): Int32;
begin
   xyArrayToPointArray(x, y, nVertices, output);
end;

procedure TPolygon.AddTo(fd: Tb2FixtureDef);
var
   vecsToAdd: TVectorArray;
   offset, ind, i: Integer;
   polyShape: Tb2PolygonShape;
begin
   if nVertices < 3 then
      Exit;

   //b2Assert(nVertices <= b2_maxPolygonVertices);

   SetLength(vecsToAdd, nVertices);
   offset := 0;

   for i := 0 to nVertices - 1 do
   begin
      //Omit identical neighbors (including wraparound)
      ind := i - offset;
      if (x[i] = x[remainder(i + 1, nVertices)]) and
         (y[i] = y[remainder(i + 1, nVertices)]) then
      begin
          Inc(offset);
          Continue;
      end;
      vecsToAdd[ind].x := x[i];
      vecsToAdd[ind].y := y[i];
   end;

   polyShape := Tb2PolygonShape.Create;
   polyShape.SetVertices(@vecsToAdd[0], ind + 1);
   fd.shape := polyShape;
end;

procedure TPolygon.MergeParallelEdges(tolerance: Single);
var
   i, lower, middle, upper: Integer;
   mergeMe: array of Boolean;
   newNVertices, currIndex: Int32;
   dx0, dy0, dx1, dy1: Double;
   norm0, norm1: Double;
   cross, dot: Double;
   copy: TPolygon;
begin
   if nVertices < 3 then
      Exit;

   SetLength(mergeMe, nVertices);
   newNVertices := nVertices;

   for i := 0 to nVertices - 1 do
   begin
      if i = 0 then
         lower := nVertices - 1
      else
         lower := i - 1;
      middle := i;
      if i = nVertices - 1 then
         upper := 0
      else
         upper := i + 1;

      dx0 := x[middle] - x[lower];
      dy0 := y[middle] - y[lower];
      dx1 := x[upper] - x[middle];
      dy1 := y[upper] - y[middle];
      norm0 := Sqrt(dx0 * dx0 + dy0 * dy0);
      norm1 := Sqrt(dx1 * dx1 + dy1 * dy1);
      if (not ((norm0 > 0.0) and (norm1 > 0.0))) and (newNVertices > 3) then
      begin
         //Merge identical points
         mergeMe[i] := True;
         Dec(newNVertices);
      end;
      dx0 := dx0 / norm0; dy0 := dy0 / norm0;
      dx1 := dx1 / norm1; dy1 := dy1 / norm1;
      cross := dx0 * dy1 - dx1 * dy0;
      dot := dx0 * dx1 + dy0 * dy1;
      if (Abs(cross) < tolerance) and (dot > 0) and (newNVertices > 3)  then
      begin
         mergeMe[i] := True;
         Dec(newNVertices);
      end
      else
         mergeMe[i] := False;
   end;

   if(newNVertices = nVertices) or (newNVertices = 0) then
      Exit;

   copy := TPolygon.Create(Self); // backup
   SetLength(x, newNVertices);
   SetLength(y, newNVertices);

   currIndex := 0;
   for i := 0 to copy.nVertices - 1 do
   begin
      if mergeMe[i] or (newNVertices = 0) or (currIndex = newNVertices) then
         Continue;
      //b2Assert(currIndex < newNVertices);
      x[currIndex] := copy.x[i];
      x[currIndex] := copy.y[i];
      Inc(currIndex);
   end;
   copy.Free;
   nVertices := newNVertices;
end;

procedure TPolygon.ReversePolygon;
begin
   UPhysics2DPolygonTool.ReversePolygon(x, y, nVertices);
   if areaIsSet then
      area := -area;
end;

end.

