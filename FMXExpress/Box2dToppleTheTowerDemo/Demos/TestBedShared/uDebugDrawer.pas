unit uDebugDrawer;

interface

uses
  System.UITypes,
  UPhysics2DTypes, UPhysics2D;

type
  TDebugDrawer = class(Tb2Draw)
    procedure DrawPoint(const p: TVector2; size: PhysicsFloat; const color: RGBA); virtual; abstract;
    procedure DrawAABB(const aabb: Tb2AABB; const color: RGBA); virtual; abstract;
    procedure SetDefaultFontColor(const Value: TColor); virtual; abstract;
    procedure SetCanvasTranslation(x, y: PhysicsFloat); virtual; abstract;
    procedure SetCanvasTranslationOffset(dx, dy: PhysicsFloat); virtual; abstract;
    procedure TextOutASCII(text: string; x,y: integer); virtual; abstract;
  end;

implementation

end.
