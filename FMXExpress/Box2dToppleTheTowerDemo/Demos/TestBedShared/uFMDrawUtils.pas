unit uFMDrawUtils;

interface

uses
  System.Types,
  UPhysics2DTypes,
  UPhysics2D,
  System.UITypes
{$IFDEF VER270}
 , System.Math.Vectors
{$ENDIF}
  ;

procedure b2dPointsToPolygon(points: UPhysics2DTypes.TPointsF; var polygon: TPolygon; ATranslateX: Double = 0; InvertY: boolean = True; AMapHeight: Double = 500);

function VCLToFMColor(AColor: TColor): TColor;
function RGBAToFMColor(const Value: RGBA): TColor;

function VectorArray4ToPolyVertices(const Value: TVectorArray4): Tb2PolyVertices;
function AABBToPolyVertices(const Value: Tb2AABB): Tb2PolyVertices;

procedure b2PolyVerticesToPolygon(
  const vertices: Tb2PolyVertices; const acount: integer; var polygon: TPolygon;
  offsetX: double = 0; offsetY: double = 0;
  scaleX: double = 1; scaleY: double = 1;
  InvertY: boolean = True; AHeight: Double = 500);

implementation

uses
  FMX.Graphics;

function RGBAToFMColor(const Value: RGBA): TColor;
var c: TColor;
begin
  TAlphaColorRec(c).R := round(255 * TRGBA(value).red);
  TAlphaColorRec(c).G := round(255 * TRGBA(value).green);
  TAlphaColorRec(c).B := round(255 * TRGBA(value).blue);
  TAlphaColorRec(c).A := round(255 * TRGBA(value).alpha);
  Result := c;
end;

function VCLToFMColor(AColor: TColor): TColor;
begin
  Result := TAlphaColor($FF000000) or TAlphaColor(AColor);
end;

function VectorArray4ToPolyVertices(const Value: TVectorArray4): Tb2PolyVertices;
var
  i: Integer;
begin
  for i := 0 to 3 do
     Result[i] := Value[i];
end;

function AABBToPolyVertices(const Value: Tb2AABB): Tb2PolyVertices;
begin
  Result[0] := Value.lowerBound;
  Result[1].x := Value.upperBound.x;
  Result[1].y := Value.lowerBound.y;
  Result[2] := Value.upperBound;
  Result[3].x := Value.lowerBound.x;
  Result[3].y := Value.upperBound.y;
end;

procedure b2dPointsToPolygon(points: UPhysics2DTypes.TPointsF; var polygon: TPolygon; ATranslateX: Double = 0; InvertY: boolean = True; AMapHeight: Double = 500);
var i, aCount: integer;
begin
  aCount := Length(points);
  SetLength(polygon, aCount);
  for i := 0 to aCount-1 do
  begin
    polygon[i].X := points[i].x + ATranslateX;
    if not InvertY then
      polygon[i].Y := points[i].y
    else
      polygon[i].Y := AMapHeight - points[i].y;
  end;
end;

procedure b2PolyVerticesToPolygon(
  const vertices: Tb2PolyVertices; const acount: integer; var polygon: TPolygon;
  offsetX: double = 0; offsetY: double = 0;
  scaleX: double = 1; scaleY: double = 1;
  InvertY: boolean = True; AHeight: Double = 500);

var i: integer;
begin
  SetLength(polygon, aCount+1);
  for i := 0 to aCount-1 do
  begin
    polygon[i].X := vertices[i].x * scaleX + offsetX;
    if not InvertY then
      polygon[i].Y := vertices[i].y * scaleY + offsetY
    else
      polygon[i].Y := AHeight - (vertices[i].y * scaleY + offsetY);
  end;
  polygon[aCount] := polygon[0];
end;

end.
