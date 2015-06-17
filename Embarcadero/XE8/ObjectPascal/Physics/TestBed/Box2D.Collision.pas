
//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit Box2D.Collision;

interface

uses
Box2D.Common,
Box2DTypes;


type

{$MinEnumSize 4}
b2PointState = (b2_nullState = 0, b2_addState = 1, b2_persistState = 2, b2_removeState = 3);
{$MinEnumSize 1}
Pb2PointState = ^b2PointState;


b2ShapeHandle = THandle;
Pb2ShapeHandle = ^b2ShapeHandle;

b2CircleShapeHandle = THandle;
Pb2CircleShapeHandle = ^b2CircleShapeHandle;

b2EdgeShapeHandle = THandle;
Pb2EdgeShapeHandle = ^b2EdgeShapeHandle;

b2PolygonShapeHandle = THandle;
Pb2PolygonShapeHandle = ^b2PolygonShapeHandle;

b2DynamicTreeHandle = THandle;
Pb2DynamicTreeHandle = ^b2DynamicTreeHandle;

b2BroadPhaseHandle = THandle;
Pb2BroadPhaseHandle = ^b2BroadPhaseHandle;

b2ChainShapeHandle = THandle;
Pb2ChainShapeHandle = ^b2ChainShapeHandle;


Pb2ContactFeature = ^b2ContactFeature;
PPb2ContactFeature = ^Pb2ContactFeature;

Pb2ContactID = ^b2ContactID;
PPb2ContactID = ^Pb2ContactID;

Pb2ManifoldPoint = ^b2ManifoldPoint;
PPb2ManifoldPoint = ^Pb2ManifoldPoint;

Pb2Manifold = ^b2Manifold;
PPb2Manifold = ^Pb2Manifold;

Pb2WorldManifold = ^b2WorldManifold;
PPb2WorldManifold = ^Pb2WorldManifold;

Pb2ClipVertex = ^b2ClipVertex;
PPb2ClipVertex = ^Pb2ClipVertex;

Pb2RayCastInput = ^b2RayCastInput;
PPb2RayCastInput = ^Pb2RayCastInput;

Pb2RayCastOutput = ^b2RayCastOutput;
PPb2RayCastOutput = ^Pb2RayCastOutput;

Pb2AABB = ^b2AABB;
PPb2AABB = ^Pb2AABB;

Pb2TreeNode = ^b2TreeNode;
PPb2TreeNode = ^Pb2TreeNode;

Pb2Pair = ^b2Pair;
PPb2Pair = ^Pb2Pair;

Pb2DistanceProxy = ^b2DistanceProxy;
PPb2DistanceProxy = ^Pb2DistanceProxy;

Pb2SimplexCache = ^b2SimplexCache;
PPb2SimplexCache = ^Pb2SimplexCache;

Pb2DistanceInput = ^b2DistanceInput;
PPb2DistanceInput = ^Pb2DistanceInput;

Pb2DistanceOutput = ^b2DistanceOutput;
PPb2DistanceOutput = ^Pb2DistanceOutput;

Pb2TOIInput = ^b2TOIInput;
PPb2TOIInput = ^Pb2TOIInput;

Pb2TOIOutput = ^b2TOIOutput;
PPb2TOIOutput = ^Pb2TOIOutput;

Pb2MassData = ^b2MassData;
PPb2MassData = ^Pb2MassData;


{ ===== Records ===== }

b2ContactFeature = record
type 
{$MinEnumSize 4}
Type_ = (e_vertex = 0, e_face = 1);
{$MinEnumSize 1}
PType_ = ^Type_;
var 
indexA: Byte;
indexB: Byte;
typeA: Byte;
typeB: Byte;

class function Create: b2ContactFeature; static; cdecl;
end;

b2ContactID = record

class function Create: b2ContactID; static; cdecl;
case Integer of
0: (cf: b2ContactFeature;);
1: (key: LongWord;);
end;

b2ManifoldPoint = record
localPoint: b2Vec2;
normalImpulse: Single;
tangentImpulse: Single;
id: b2ContactID;

class function Create: b2ManifoldPoint; static; cdecl;
end;

b2Manifold = record
type 
{$MinEnumSize 4}
Type_ = (e_circles = 0, e_faceA = 1, e_faceB = 2);
{$MinEnumSize 1}
PType_ = ^Type_;
var 
points: array[0..1] of b2ManifoldPoint;
localNormal: b2Vec2;
localPoint: b2Vec2;
&type: b2Manifold.Type_;
pointCount: Integer;

class function Create: b2Manifold; static; cdecl;
end;

b2WorldManifold = record
normal: b2Vec2;
points: array[0..1] of b2Vec2;
separations: array[0..1] of Single;

class function Create: b2WorldManifold; static; cdecl;

procedure Initialize(manifold: Pb2Manifold; const [ref] xfA: b2Transform; radiusA: Single; const [ref] xfB: b2Transform; radiusB: Single); cdecl;
end;

b2ClipVertex = record
v: b2Vec2;
id: b2ContactID;

class function Create: b2ClipVertex; static; cdecl;
end;

b2RayCastInput = record
p1: b2Vec2;
p2: b2Vec2;
maxFraction: Single;

class function Create: b2RayCastInput; static; cdecl;
end;

b2RayCastOutput = record
normal: b2Vec2;
fraction: Single;

class function Create: b2RayCastOutput; static; cdecl;
end;

b2AABB = record
lowerBound: b2Vec2;
upperBound: b2Vec2;

class function Create: b2AABB; static; cdecl;

function IsValid: Boolean; cdecl;
function GetCenter: b2Vec2; cdecl;
function GetExtents: b2Vec2; cdecl;
function GetPerimeter: Single; cdecl;
procedure Combine(const [ref] aabb: b2AABB); overload; cdecl;
procedure Combine(const [ref] aabb1: b2AABB; const [ref] aabb2: b2AABB); overload; cdecl;
function Contains(const [ref] aabb: b2AABB): Boolean; cdecl;
function RayCast(output: Pb2RayCastOutput; const [ref] input: b2RayCastInput): Boolean; cdecl;
end;

b2TreeNode = record
aabb: b2AABB;
userData: Pointer;
union1: record
case Integer of
0: (parent: Integer);
1: (next: Integer);
end;
child1: Integer;
child2: Integer;
height: Integer;

class function Create: b2TreeNode; static; cdecl;

function IsLeaf: Boolean; cdecl;
end;

b2DynamicTreeWrapper = record
FHandle: b2DynamicTreeHandle;

class function Create: b2DynamicTreeWrapper; static; cdecl;
procedure Destroy; cdecl;

class operator Implicit(handle: b2DynamicTreeHandle): b2DynamicTreeWrapper; overload;
class operator Implicit(wrapper: b2DynamicTreeWrapper): b2DynamicTreeHandle; overload;
function CreateProxy(const [ref] aabb: b2AABB; userData: Pointer): Integer; cdecl;
procedure DestroyProxy(proxyId: Integer); cdecl;
function MoveProxy(proxyId: Integer; const [ref] aabb1: b2AABB; const [ref] displacement: b2Vec2): Boolean; cdecl;
function GetUserData(proxyId: Integer): Pointer; cdecl;
function GetFatAABB(proxyId: Integer): Pb2AABB; cdecl;
procedure Validate; cdecl;
function GetHeight: Integer; cdecl;
function GetMaxBalance: Integer; cdecl;
function GetAreaRatio: Single; cdecl;
procedure RebuildBottomUp; cdecl;
procedure ShiftOrigin(const [ref] newOrigin: b2Vec2); cdecl;
end;

b2Pair = record
proxyIdA: Integer;
proxyIdB: Integer;

class function Create: b2Pair; static; cdecl;
end;

b2BroadPhaseWrapper = record
type 
{$MinEnumSize 4}
enum_anon = (e_nullProxy = - 1);
{$MinEnumSize 1}
Penum_anon = ^enum_anon;
var 
FHandle: b2BroadPhaseHandle;

class function Create: b2BroadPhaseWrapper; static; cdecl;
procedure Destroy; cdecl;

class operator Implicit(handle: b2BroadPhaseHandle): b2BroadPhaseWrapper; overload;
class operator Implicit(wrapper: b2BroadPhaseWrapper): b2BroadPhaseHandle; overload;
function CreateProxy(const [ref] aabb: b2AABB; userData: Pointer): Integer; cdecl;
procedure DestroyProxy(proxyId: Integer); cdecl;
procedure MoveProxy(proxyId: Integer; const [ref] aabb: b2AABB; const [ref] displacement: b2Vec2); cdecl;
procedure TouchProxy(proxyId: Integer); cdecl;
function GetFatAABB(proxyId: Integer): Pb2AABB; cdecl;
function GetUserData(proxyId: Integer): Pointer; cdecl;
function TestOverlap(proxyIdA: Integer; proxyIdB: Integer): Boolean; cdecl;
function GetProxyCount: Integer; cdecl;
function GetTreeHeight: Integer; cdecl;
function GetTreeBalance: Integer; cdecl;
function GetTreeQuality: Single; cdecl;
procedure ShiftOrigin(const [ref] newOrigin: b2Vec2); cdecl;
end;

b2DistanceProxy = record
m_buffer: array[0..1] of b2Vec2;
m_vertices: Pb2Vec2;
m_count: Integer;
m_radius: Single;

class function Create: b2DistanceProxy; static; cdecl;

procedure &Set(shape: b2ShapeHandle; index: Integer); cdecl;
function GetSupport(const [ref] d: b2Vec2): Integer; cdecl;
function GetSupportVertex(const [ref] d: b2Vec2): Pb2Vec2; cdecl;
function GetVertexCount: Integer; cdecl;
function GetVertex(index: Integer): Pb2Vec2; cdecl;
end;

b2SimplexCache = record
metric: Single;
count: Word;
indexA: array[0..2] of Byte;
indexB: array[0..2] of Byte;

class function Create: b2SimplexCache; static; cdecl;
end;

b2DistanceInput = record
proxyA: b2DistanceProxy;
proxyB: b2DistanceProxy;
transformA: b2Transform;
transformB: b2Transform;
useRadii: Boolean;

class function Create: b2DistanceInput; static; cdecl;
end;

b2DistanceOutput = record
pointA: b2Vec2;
pointB: b2Vec2;
distance: Single;
iterations: Integer;

class function Create: b2DistanceOutput; static; cdecl;
end;

b2TOIInput = record
proxyA: b2DistanceProxy;
proxyB: b2DistanceProxy;
sweepA: b2Sweep;
sweepB: b2Sweep;
tMax: Single;

class function Create: b2TOIInput; static; cdecl;
end;

b2TOIOutput = record
type 
{$MinEnumSize 4}
State = (e_unknown = 0, e_failed = 1, e_overlapped = 2, e_touching = 3, e_separated = 4);
{$MinEnumSize 1}
PState = ^State;
var 
state_: b2TOIOutput.State;
t: Single;

class function Create: b2TOIOutput; static; cdecl;
end;

b2MassData = record
mass: Single;
center: b2Vec2;
I: Single;

class function Create: b2MassData; static; cdecl;
end;

b2ShapeWrapper = record
type 
{$MinEnumSize 4}
Type_ = (e_circle = 0, e_edge = 1, e_polygon = 2, e_chain = 3, e_typeCount = 4);
{$MinEnumSize 1}
PType_ = ^Type_;
var 
FHandle: b2ShapeHandle;

class operator Implicit(handle: b2ShapeHandle): b2ShapeWrapper; overload;
class operator Implicit(wrapper: b2ShapeWrapper): b2ShapeHandle; overload;
function Clone(allocator: b2BlockAllocatorHandle): b2ShapeHandle; cdecl;
function GetType: b2ShapeWrapper.Type_; cdecl;
function GetChildCount: Integer; cdecl;
function TestPoint(const [ref] xf: b2Transform; const [ref] p: b2Vec2): Boolean; cdecl;
function RayCast(output: Pb2RayCastOutput; const [ref] input: b2RayCastInput; const [ref] transform: b2Transform; childIndex: Integer): Boolean; cdecl;
procedure ComputeAABB(aabb: Pb2AABB; const [ref] xf: b2Transform; childIndex: Integer); cdecl;
procedure ComputeMass(massData: Pb2MassData; density: Single); cdecl;
function Get_m_type: b2ShapeWrapper.Type_; cdecl;
procedure Set_m_type(aNewValue: b2ShapeWrapper.Type_); cdecl;
function Get_m_radius: Single; cdecl;
procedure Set_m_radius(aNewValue: Single); cdecl;
end;

b2ChainShapeWrapper = record
FHandle: b2ChainShapeHandle;

class function Create: b2ChainShapeWrapper; static; cdecl;
procedure Destroy; cdecl;

class operator Implicit(handle: b2ChainShapeHandle): b2ChainShapeWrapper; overload;
class operator Implicit(wrapper: b2ChainShapeWrapper): b2ChainShapeHandle; overload;
function Clone(allocator: b2BlockAllocatorHandle): b2ShapeHandle; cdecl;
function GetType: b2ShapeWrapper.Type_; cdecl;
function GetChildCount: Integer; cdecl;
function TestPoint(const [ref] xf: b2Transform; const [ref] p: b2Vec2): Boolean; cdecl;
function RayCast(output: Pb2RayCastOutput; const [ref] input: b2RayCastInput; const [ref] transform: b2Transform; childIndex: Integer): Boolean; cdecl;
procedure ComputeAABB(aabb: Pb2AABB; const [ref] xf: b2Transform; childIndex: Integer); cdecl;
procedure ComputeMass(massData: Pb2MassData; density: Single); cdecl;
function Get_m_type: b2ShapeWrapper.Type_; cdecl;
procedure Set_m_type(aNewValue: b2ShapeWrapper.Type_); cdecl;
function Get_m_radius: Single; cdecl;
procedure Set_m_radius(aNewValue: Single); cdecl;
procedure Clear; cdecl;
procedure CreateLoop(vertices: Pb2Vec2; count: Integer); cdecl;
procedure CreateChain(vertices: Pb2Vec2; count: Integer); cdecl;
procedure SetPrevVertex(const [ref] prevVertex: b2Vec2); cdecl;
procedure SetNextVertex(const [ref] nextVertex: b2Vec2); cdecl;
procedure GetChildEdge(edge: b2EdgeShapeHandle; index: Integer); cdecl;
function Get_m_vertices: Pb2Vec2; cdecl;
procedure Set_m_vertices(aNewValue: Pb2Vec2); cdecl;
function Get_m_count: Integer; cdecl;
procedure Set_m_count(aNewValue: Integer); cdecl;
function Get_m_prevVertex: b2Vec2; cdecl;
procedure Set_m_prevVertex(aNewValue: b2Vec2); cdecl;
function Get_m_nextVertex: b2Vec2; cdecl;
procedure Set_m_nextVertex(aNewValue: b2Vec2); cdecl;
function Get_m_hasPrevVertex: Boolean; cdecl;
procedure Set_m_hasPrevVertex(aNewValue: Boolean); cdecl;
function Get_m_hasNextVertex: Boolean; cdecl;
procedure Set_m_hasNextVertex(aNewValue: Boolean); cdecl;
end;

b2CircleShapeWrapper = record
FHandle: b2CircleShapeHandle;

class function Create: b2CircleShapeWrapper; static; cdecl;
procedure Destroy; cdecl;

class operator Implicit(handle: b2CircleShapeHandle): b2CircleShapeWrapper; overload;
class operator Implicit(wrapper: b2CircleShapeWrapper): b2CircleShapeHandle; overload;
function Clone(allocator: b2BlockAllocatorHandle): b2ShapeHandle; cdecl;
function GetType: b2ShapeWrapper.Type_; cdecl;
function GetChildCount: Integer; cdecl;
function TestPoint(const [ref] xf: b2Transform; const [ref] p: b2Vec2): Boolean; cdecl;
function RayCast(output: Pb2RayCastOutput; const [ref] input: b2RayCastInput; const [ref] transform: b2Transform; childIndex: Integer): Boolean; cdecl;
procedure ComputeAABB(aabb: Pb2AABB; const [ref] xf: b2Transform; childIndex: Integer); cdecl;
procedure ComputeMass(massData: Pb2MassData; density: Single); cdecl;
function Get_m_type: b2ShapeWrapper.Type_; cdecl;
procedure Set_m_type(aNewValue: b2ShapeWrapper.Type_); cdecl;
function Get_m_radius: Single; cdecl;
procedure Set_m_radius(aNewValue: Single); cdecl;
function GetSupport(const [ref] d: b2Vec2): Integer; cdecl;
function GetSupportVertex(const [ref] d: b2Vec2): Pb2Vec2; cdecl;
function GetVertexCount: Integer; cdecl;
function GetVertex(index: Integer): Pb2Vec2; cdecl;
function Get_m_p: b2Vec2; cdecl;
procedure Set_m_p(aNewValue: b2Vec2); cdecl;
end;

b2EdgeShapeWrapper = record
FHandle: b2EdgeShapeHandle;

class function Create: b2EdgeShapeWrapper; static; cdecl;
procedure Destroy; cdecl;

class operator Implicit(handle: b2EdgeShapeHandle): b2EdgeShapeWrapper; overload;
class operator Implicit(wrapper: b2EdgeShapeWrapper): b2EdgeShapeHandle; overload;
function Clone(allocator: b2BlockAllocatorHandle): b2ShapeHandle; cdecl;
function GetType: b2ShapeWrapper.Type_; cdecl;
function GetChildCount: Integer; cdecl;
function TestPoint(const [ref] xf: b2Transform; const [ref] p: b2Vec2): Boolean; cdecl;
function RayCast(output: Pb2RayCastOutput; const [ref] input: b2RayCastInput; const [ref] transform: b2Transform; childIndex: Integer): Boolean; cdecl;
procedure ComputeAABB(aabb: Pb2AABB; const [ref] xf: b2Transform; childIndex: Integer); cdecl;
procedure ComputeMass(massData: Pb2MassData; density: Single); cdecl;
function Get_m_type: b2ShapeWrapper.Type_; cdecl;
procedure Set_m_type(aNewValue: b2ShapeWrapper.Type_); cdecl;
function Get_m_radius: Single; cdecl;
procedure Set_m_radius(aNewValue: Single); cdecl;
procedure &Set(const [ref] v1: b2Vec2; const [ref] v2: b2Vec2); cdecl;
function Get_m_vertex1: b2Vec2; cdecl;
procedure Set_m_vertex1(aNewValue: b2Vec2); cdecl;
function Get_m_vertex2: b2Vec2; cdecl;
procedure Set_m_vertex2(aNewValue: b2Vec2); cdecl;
function Get_m_vertex0: b2Vec2; cdecl;
procedure Set_m_vertex0(aNewValue: b2Vec2); cdecl;
function Get_m_vertex3: b2Vec2; cdecl;
procedure Set_m_vertex3(aNewValue: b2Vec2); cdecl;
function Get_m_hasVertex0: Boolean; cdecl;
procedure Set_m_hasVertex0(aNewValue: Boolean); cdecl;
function Get_m_hasVertex3: Boolean; cdecl;
procedure Set_m_hasVertex3(aNewValue: Boolean); cdecl;
end;

b2PolygonShapeWrapper = record
FHandle: b2PolygonShapeHandle;

class function Create: b2PolygonShapeWrapper; static; cdecl;
procedure Destroy; cdecl;

class operator Implicit(handle: b2PolygonShapeHandle): b2PolygonShapeWrapper; overload;
class operator Implicit(wrapper: b2PolygonShapeWrapper): b2PolygonShapeHandle; overload;
function Clone(allocator: b2BlockAllocatorHandle): b2ShapeHandle; cdecl;
function GetType: b2ShapeWrapper.Type_; cdecl;
function GetChildCount: Integer; cdecl;
function TestPoint(const [ref] xf: b2Transform; const [ref] p: b2Vec2): Boolean; cdecl;
function RayCast(output: Pb2RayCastOutput; const [ref] input: b2RayCastInput; const [ref] transform: b2Transform; childIndex: Integer): Boolean; cdecl;
procedure ComputeAABB(aabb: Pb2AABB; const [ref] xf: b2Transform; childIndex: Integer); cdecl;
procedure ComputeMass(massData: Pb2MassData; density: Single); cdecl;
function Get_m_type: b2ShapeWrapper.Type_; cdecl;
procedure Set_m_type(aNewValue: b2ShapeWrapper.Type_); cdecl;
function Get_m_radius: Single; cdecl;
procedure Set_m_radius(aNewValue: Single); cdecl;
procedure &Set(points: Pb2Vec2; count: Integer); cdecl;
procedure SetAsBox(hx: Single; hy: Single); overload; cdecl;
procedure SetAsBox(hx: Single; hy: Single; const [ref] center: b2Vec2; angle: Single); overload; cdecl;
function GetVertexCount: Integer; cdecl;
function GetVertex(index: Integer): Pb2Vec2; cdecl;
function Validate: Boolean; cdecl;
function Get_m_centroid: b2Vec2; cdecl;
procedure Set_m_centroid(aNewValue: b2Vec2); cdecl;
function Get_m_count: Integer; cdecl;
procedure Set_m_count(aNewValue: Integer); cdecl;
end;


{ Accessors for the global variable 'b2_nullFeature' }
function Get_b2_nullFeature: Byte; cdecl;

procedure b2GetPointStates(state1: Pb2PointState; state2: Pb2PointState; manifold1: Pb2Manifold; manifold2: Pb2Manifold); cdecl;
procedure b2CollideCircles(manifold: Pb2Manifold; circleA: b2CircleShapeHandle; const [ref] xfA: b2Transform; circleB: b2CircleShapeHandle; const [ref] xfB: b2Transform); cdecl;
procedure b2CollidePolygonAndCircle(manifold: Pb2Manifold; polygonA: b2PolygonShapeHandle; const [ref] xfA: b2Transform; circleB: b2CircleShapeHandle; const [ref] xfB: b2Transform); cdecl;
procedure b2CollidePolygons(manifold: Pb2Manifold; polygonA: b2PolygonShapeHandle; const [ref] xfA: b2Transform; polygonB: b2PolygonShapeHandle; const [ref] xfB: b2Transform); cdecl;
procedure b2CollideEdgeAndCircle(manifold: Pb2Manifold; polygonA: b2EdgeShapeHandle; const [ref] xfA: b2Transform; circleB: b2CircleShapeHandle; const [ref] xfB: b2Transform); cdecl;
procedure b2CollideEdgeAndPolygon(manifold: Pb2Manifold; edgeA: b2EdgeShapeHandle; const [ref] xfA: b2Transform; circleB: b2PolygonShapeHandle; const [ref] xfB: b2Transform); cdecl;
function b2ClipSegmentToLine(vOut: Pb2ClipVertex; vIn: Pb2ClipVertex; const [ref] normal: b2Vec2; offset: Single; vertexIndexA: Integer): Integer; cdecl;
function b2TestOverlap(shapeA: b2ShapeHandle; indexA: Integer; shapeB: b2ShapeHandle; indexB: Integer; const [ref] xfA: b2Transform; const [ref] xfB: b2Transform): Boolean; overload; cdecl;
function b2TestOverlap(const [ref] a: b2AABB; const [ref] b: b2AABB): Boolean; overload; cdecl;
function b2PairLessThan(const [ref] pair1: b2Pair; const [ref] pair2: b2Pair): Boolean; cdecl;
procedure b2Distance(output: Pb2DistanceOutput; cache: Pb2SimplexCache; input: Pb2DistanceInput); cdecl;
procedure b2TimeOfImpact(output: Pb2TOIOutput; input: Pb2TOIInput); cdecl;

implementation

const
{$IFDEF MSWINDOWS}
  LIB_NAME = 'FlatBox2DDyn.dll';
  {$IFDEF WIN64}
    _PU = '';
  {$ELSE}
    _PU = '_';
  {$ENDIF}
{$ENDIF}
{$IFDEF ANDROID}
  LIB_NAME = 'libFlatBox2D.a';
  _PU = '';
{$ENDIF}
{$IFDEF MACOS}
  {$IFDEF IOS}
    LIB_NAME = 'libFlatBox2D.a';
  {$ELSE}
    LIB_NAME = 'libFlatBox2DDyn.dylib';
  {$ENDIF}
  {$IFDEF UNDERSCOREIMPORTNAME}
    _PU = '_';
  {$ELSE}
    _PU = '';
  {$ENDIF}
{$ENDIF}

{ ===== Record methods: import and definition ===== }

function b2ContactFeature_Create: b2ContactFeature; cdecl; external LIB_NAME name _PU + 'b2ContactFeature_b2ContactFeature'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2ContactFeature.Create: b2ContactFeature; cdecl;
begin
  Result := b2ContactFeature_Create;
end;


function b2ContactID_Create: b2ContactID; cdecl; external LIB_NAME name _PU + 'b2ContactID_b2ContactID'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2ContactID.Create: b2ContactID; cdecl;
begin
  Result := b2ContactID_Create;
end;


function b2ManifoldPoint_Create: b2ManifoldPoint; cdecl; external LIB_NAME name _PU + 'b2ManifoldPoint_b2ManifoldPoint'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2ManifoldPoint.Create: b2ManifoldPoint; cdecl;
begin
  Result := b2ManifoldPoint_Create;
end;


function b2Manifold_Create: b2Manifold; cdecl; external LIB_NAME name _PU + 'b2Manifold_b2Manifold'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2Manifold.Create: b2Manifold; cdecl;
begin
  Result := b2Manifold_Create;
end;


function b2WorldManifold_Create: b2WorldManifold; cdecl; external LIB_NAME name _PU + 'b2WorldManifold_b2WorldManifold'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2WorldManifold.Create: b2WorldManifold; cdecl;
begin
  Result := b2WorldManifold_Create;
end;

procedure b2WorldManifold_Initialize(_self: Pb2WorldManifold; manifold: Pb2Manifold; const [ref] xfA: b2Transform; radiusA: Single; const [ref] xfB: b2Transform; radiusB: Single); cdecl; external LIB_NAME name _PU + 'b2WorldManifold_Initialize'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2WorldManifold.Initialize(manifold: Pb2Manifold; const [ref] xfA: b2Transform; radiusA: Single; const [ref] xfB: b2Transform; radiusB: Single); cdecl;
begin
  b2WorldManifold_Initialize(@Self, manifold, xfA, radiusA, xfB, radiusB)
end;


function b2ClipVertex_Create: b2ClipVertex; cdecl; external LIB_NAME name _PU + 'b2ClipVertex_b2ClipVertex'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2ClipVertex.Create: b2ClipVertex; cdecl;
begin
  Result := b2ClipVertex_Create;
end;


function b2RayCastInput_Create: b2RayCastInput; cdecl; external LIB_NAME name _PU + 'b2RayCastInput_b2RayCastInput_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2RayCastInput.Create: b2RayCastInput; cdecl;
begin
  Result := b2RayCastInput_Create;
end;


function b2RayCastOutput_Create: b2RayCastOutput; cdecl; external LIB_NAME name _PU + 'b2RayCastOutput_b2RayCastOutput'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2RayCastOutput.Create: b2RayCastOutput; cdecl;
begin
  Result := b2RayCastOutput_Create;
end;


function b2AABB_Create: b2AABB; cdecl; external LIB_NAME name _PU + 'b2AABB_b2AABB_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2AABB.Create: b2AABB; cdecl;
begin
  Result := b2AABB_Create;
end;

function b2AABB_IsValid(_self: Pb2AABB): Boolean; cdecl; external LIB_NAME name _PU + 'b2AABB_IsValid'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2AABB.IsValid: Boolean; cdecl;
begin
  Result := b2AABB_IsValid(@Self)
end;

function b2AABB_GetCenter(_self: Pb2AABB): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2AABB_GetCenter'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2AABB.GetCenter: b2Vec2; cdecl;
begin
  Result := b2AABB_GetCenter(@Self)
end;

function b2AABB_GetExtents(_self: Pb2AABB): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2AABB_GetExtents'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2AABB.GetExtents: b2Vec2; cdecl;
begin
  Result := b2AABB_GetExtents(@Self)
end;

function b2AABB_GetPerimeter(_self: Pb2AABB): Single; cdecl; external LIB_NAME name _PU + 'b2AABB_GetPerimeter'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2AABB.GetPerimeter: Single; cdecl;
begin
  Result := b2AABB_GetPerimeter(@Self)
end;

procedure b2AABB_Combine(_self: Pb2AABB; const [ref] aabb: b2AABB); overload; cdecl; external LIB_NAME name _PU + 'b2AABB_Combine'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2AABB.Combine(const [ref] aabb: b2AABB); cdecl;
begin
  b2AABB_Combine(@Self, aabb)
end;

procedure b2AABB_Combine(_self: Pb2AABB; const [ref] aabb1: b2AABB; const [ref] aabb2: b2AABB); overload; cdecl; external LIB_NAME name _PU + 'b2AABB_Combine2'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2AABB.Combine(const [ref] aabb1: b2AABB; const [ref] aabb2: b2AABB); cdecl;
begin
  b2AABB_Combine(@Self, aabb1, aabb2)
end;

function b2AABB_Contains(_self: Pb2AABB; const [ref] aabb: b2AABB): Boolean; cdecl; external LIB_NAME name _PU + 'b2AABB_Contains'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2AABB.Contains(const [ref] aabb: b2AABB): Boolean; cdecl;
begin
  Result := b2AABB_Contains(@Self, aabb)
end;

function b2AABB_RayCast(_self: Pb2AABB; output: Pb2RayCastOutput; const [ref] input: b2RayCastInput): Boolean; cdecl; external LIB_NAME name _PU + 'b2AABB_RayCast'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2AABB.RayCast(output: Pb2RayCastOutput; const [ref] input: b2RayCastInput): Boolean; cdecl;
begin
  Result := b2AABB_RayCast(@Self, output, input)
end;


function b2TreeNode_Create: b2TreeNode; cdecl; external LIB_NAME name _PU + 'b2TreeNode_b2TreeNode'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2TreeNode.Create: b2TreeNode; cdecl;
begin
  Result := b2TreeNode_Create;
end;

function b2TreeNode_IsLeaf(_self: Pb2TreeNode): Boolean; cdecl; external LIB_NAME name _PU + 'b2TreeNode_IsLeaf'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2TreeNode.IsLeaf: Boolean; cdecl;
begin
  Result := b2TreeNode_IsLeaf(@Self)
end;


function b2DynamicTree_Create: b2DynamicTreeHandle; cdecl; external LIB_NAME name _PU + 'b2DynamicTree_b2DynamicTree_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2DynamicTreeWrapper.Create: b2DynamicTreeWrapper; cdecl;
begin
  Result.FHandle := b2DynamicTree_Create;
end;

procedure b2DynamicTree_Destroy(_self: b2DynamicTreeHandle); cdecl; external LIB_NAME name _PU + 'b2DynamicTree_dtor'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2DynamicTreeWrapper.Destroy; cdecl;
begin
  b2DynamicTree_Destroy(FHandle);
end;

class operator b2DynamicTreeWrapper.Implicit(handle: b2DynamicTreeHandle): b2DynamicTreeWrapper;
begin
  Result.FHandle := handle;
end;
class operator b2DynamicTreeWrapper.Implicit(wrapper: b2DynamicTreeWrapper): b2DynamicTreeHandle;
begin
  Result := wrapper.FHandle;
end;

function b2DynamicTree_CreateProxy(_self: b2DynamicTreeHandle; const [ref] aabb: b2AABB; userData: Pointer): Integer; cdecl; external LIB_NAME name _PU + 'b2DynamicTree_CreateProxy'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2DynamicTreeWrapper.CreateProxy(const [ref] aabb: b2AABB; userData: Pointer): Integer; cdecl;
begin
  Result := b2DynamicTree_CreateProxy(FHandle, aabb, userData)
end;

procedure b2DynamicTree_DestroyProxy(_self: b2DynamicTreeHandle; proxyId: Integer); cdecl; external LIB_NAME name _PU + 'b2DynamicTree_DestroyProxy'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2DynamicTreeWrapper.DestroyProxy(proxyId: Integer); cdecl;
begin
  b2DynamicTree_DestroyProxy(FHandle, proxyId)
end;

function b2DynamicTree_MoveProxy(_self: b2DynamicTreeHandle; proxyId: Integer; const [ref] aabb1: b2AABB; const [ref] displacement: b2Vec2): Boolean; cdecl; external LIB_NAME name _PU + 'b2DynamicTree_MoveProxy'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2DynamicTreeWrapper.MoveProxy(proxyId: Integer; const [ref] aabb1: b2AABB; const [ref] displacement: b2Vec2): Boolean; cdecl;
begin
  Result := b2DynamicTree_MoveProxy(FHandle, proxyId, aabb1, displacement)
end;

function b2DynamicTree_GetUserData(_self: b2DynamicTreeHandle; proxyId: Integer): Pointer; cdecl; external LIB_NAME name _PU + 'b2DynamicTree_GetUserData'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2DynamicTreeWrapper.GetUserData(proxyId: Integer): Pointer; cdecl;
begin
  Result := b2DynamicTree_GetUserData(FHandle, proxyId)
end;

function b2DynamicTree_GetFatAABB(_self: b2DynamicTreeHandle; proxyId: Integer): Pb2AABB; cdecl; external LIB_NAME name _PU + 'b2DynamicTree_GetFatAABB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2DynamicTreeWrapper.GetFatAABB(proxyId: Integer): Pb2AABB; cdecl;
begin
  Result := b2DynamicTree_GetFatAABB(FHandle, proxyId)
end;

procedure b2DynamicTree_Validate(_self: b2DynamicTreeHandle); cdecl; external LIB_NAME name _PU + 'b2DynamicTree_Validate'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2DynamicTreeWrapper.Validate; cdecl;
begin
  b2DynamicTree_Validate(FHandle)
end;

function b2DynamicTree_GetHeight(_self: b2DynamicTreeHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2DynamicTree_GetHeight'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2DynamicTreeWrapper.GetHeight: Integer; cdecl;
begin
  Result := b2DynamicTree_GetHeight(FHandle)
end;

function b2DynamicTree_GetMaxBalance(_self: b2DynamicTreeHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2DynamicTree_GetMaxBalance'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2DynamicTreeWrapper.GetMaxBalance: Integer; cdecl;
begin
  Result := b2DynamicTree_GetMaxBalance(FHandle)
end;

function b2DynamicTree_GetAreaRatio(_self: b2DynamicTreeHandle): Single; cdecl; external LIB_NAME name _PU + 'b2DynamicTree_GetAreaRatio'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2DynamicTreeWrapper.GetAreaRatio: Single; cdecl;
begin
  Result := b2DynamicTree_GetAreaRatio(FHandle)
end;

procedure b2DynamicTree_RebuildBottomUp(_self: b2DynamicTreeHandle); cdecl; external LIB_NAME name _PU + 'b2DynamicTree_RebuildBottomUp'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2DynamicTreeWrapper.RebuildBottomUp; cdecl;
begin
  b2DynamicTree_RebuildBottomUp(FHandle)
end;

procedure b2DynamicTree_ShiftOrigin(_self: b2DynamicTreeHandle; const [ref] newOrigin: b2Vec2); cdecl; external LIB_NAME name _PU + 'b2DynamicTree_ShiftOrigin'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2DynamicTreeWrapper.ShiftOrigin(const [ref] newOrigin: b2Vec2); cdecl;
begin
  b2DynamicTree_ShiftOrigin(FHandle, newOrigin)
end;


function b2Pair_Create: b2Pair; cdecl; external LIB_NAME name _PU + 'b2Pair_b2Pair'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2Pair.Create: b2Pair; cdecl;
begin
  Result := b2Pair_Create;
end;


function b2BroadPhase_Create: b2BroadPhaseHandle; cdecl; external LIB_NAME name _PU + 'b2BroadPhase_b2BroadPhase_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2BroadPhaseWrapper.Create: b2BroadPhaseWrapper; cdecl;
begin
  Result.FHandle := b2BroadPhase_Create;
end;

procedure b2BroadPhase_Destroy(_self: b2BroadPhaseHandle); cdecl; external LIB_NAME name _PU + 'b2BroadPhase_dtor'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2BroadPhaseWrapper.Destroy; cdecl;
begin
  b2BroadPhase_Destroy(FHandle);
end;

class operator b2BroadPhaseWrapper.Implicit(handle: b2BroadPhaseHandle): b2BroadPhaseWrapper;
begin
  Result.FHandle := handle;
end;
class operator b2BroadPhaseWrapper.Implicit(wrapper: b2BroadPhaseWrapper): b2BroadPhaseHandle;
begin
  Result := wrapper.FHandle;
end;

function b2BroadPhase_CreateProxy(_self: b2BroadPhaseHandle; const [ref] aabb: b2AABB; userData: Pointer): Integer; cdecl; external LIB_NAME name _PU + 'b2BroadPhase_CreateProxy'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2BroadPhaseWrapper.CreateProxy(const [ref] aabb: b2AABB; userData: Pointer): Integer; cdecl;
begin
  Result := b2BroadPhase_CreateProxy(FHandle, aabb, userData)
end;

procedure b2BroadPhase_DestroyProxy(_self: b2BroadPhaseHandle; proxyId: Integer); cdecl; external LIB_NAME name _PU + 'b2BroadPhase_DestroyProxy'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2BroadPhaseWrapper.DestroyProxy(proxyId: Integer); cdecl;
begin
  b2BroadPhase_DestroyProxy(FHandle, proxyId)
end;

procedure b2BroadPhase_MoveProxy(_self: b2BroadPhaseHandle; proxyId: Integer; const [ref] aabb: b2AABB; const [ref] displacement: b2Vec2); cdecl; external LIB_NAME name _PU + 'b2BroadPhase_MoveProxy'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2BroadPhaseWrapper.MoveProxy(proxyId: Integer; const [ref] aabb: b2AABB; const [ref] displacement: b2Vec2); cdecl;
begin
  b2BroadPhase_MoveProxy(FHandle, proxyId, aabb, displacement)
end;

procedure b2BroadPhase_TouchProxy(_self: b2BroadPhaseHandle; proxyId: Integer); cdecl; external LIB_NAME name _PU + 'b2BroadPhase_TouchProxy'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2BroadPhaseWrapper.TouchProxy(proxyId: Integer); cdecl;
begin
  b2BroadPhase_TouchProxy(FHandle, proxyId)
end;

function b2BroadPhase_GetFatAABB(_self: b2BroadPhaseHandle; proxyId: Integer): Pb2AABB; cdecl; external LIB_NAME name _PU + 'b2BroadPhase_GetFatAABB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2BroadPhaseWrapper.GetFatAABB(proxyId: Integer): Pb2AABB; cdecl;
begin
  Result := b2BroadPhase_GetFatAABB(FHandle, proxyId)
end;

function b2BroadPhase_GetUserData(_self: b2BroadPhaseHandle; proxyId: Integer): Pointer; cdecl; external LIB_NAME name _PU + 'b2BroadPhase_GetUserData'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2BroadPhaseWrapper.GetUserData(proxyId: Integer): Pointer; cdecl;
begin
  Result := b2BroadPhase_GetUserData(FHandle, proxyId)
end;

function b2BroadPhase_TestOverlap(_self: b2BroadPhaseHandle; proxyIdA: Integer; proxyIdB: Integer): Boolean; cdecl; external LIB_NAME name _PU + 'b2BroadPhase_TestOverlap'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2BroadPhaseWrapper.TestOverlap(proxyIdA: Integer; proxyIdB: Integer): Boolean; cdecl;
begin
  Result := b2BroadPhase_TestOverlap(FHandle, proxyIdA, proxyIdB)
end;

function b2BroadPhase_GetProxyCount(_self: b2BroadPhaseHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2BroadPhase_GetProxyCount'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2BroadPhaseWrapper.GetProxyCount: Integer; cdecl;
begin
  Result := b2BroadPhase_GetProxyCount(FHandle)
end;

function b2BroadPhase_GetTreeHeight(_self: b2BroadPhaseHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2BroadPhase_GetTreeHeight'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2BroadPhaseWrapper.GetTreeHeight: Integer; cdecl;
begin
  Result := b2BroadPhase_GetTreeHeight(FHandle)
end;

function b2BroadPhase_GetTreeBalance(_self: b2BroadPhaseHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2BroadPhase_GetTreeBalance'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2BroadPhaseWrapper.GetTreeBalance: Integer; cdecl;
begin
  Result := b2BroadPhase_GetTreeBalance(FHandle)
end;

function b2BroadPhase_GetTreeQuality(_self: b2BroadPhaseHandle): Single; cdecl; external LIB_NAME name _PU + 'b2BroadPhase_GetTreeQuality'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2BroadPhaseWrapper.GetTreeQuality: Single; cdecl;
begin
  Result := b2BroadPhase_GetTreeQuality(FHandle)
end;

procedure b2BroadPhase_ShiftOrigin(_self: b2BroadPhaseHandle; const [ref] newOrigin: b2Vec2); cdecl; external LIB_NAME name _PU + 'b2BroadPhase_ShiftOrigin'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2BroadPhaseWrapper.ShiftOrigin(const [ref] newOrigin: b2Vec2); cdecl;
begin
  b2BroadPhase_ShiftOrigin(FHandle, newOrigin)
end;


function b2DistanceProxy_Create: b2DistanceProxy; cdecl; external LIB_NAME name _PU + 'b2DistanceProxy_b2DistanceProxy_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2DistanceProxy.Create: b2DistanceProxy; cdecl;
begin
  Result := b2DistanceProxy_Create;
end;

procedure b2DistanceProxy__Set(_self: Pb2DistanceProxy; shape: b2ShapeHandle; index: Integer); cdecl; external LIB_NAME name _PU + 'b2DistanceProxy_Set'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2DistanceProxy.&Set(shape: b2ShapeHandle; index: Integer); cdecl;
begin
  b2DistanceProxy__Set(@Self, shape, index)
end;

function b2DistanceProxy_GetSupport(_self: Pb2DistanceProxy; const [ref] d: b2Vec2): Integer; cdecl; external LIB_NAME name _PU + 'b2DistanceProxy_GetSupport'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2DistanceProxy.GetSupport(const [ref] d: b2Vec2): Integer; cdecl;
begin
  Result := b2DistanceProxy_GetSupport(@Self, d)
end;

function b2DistanceProxy_GetSupportVertex(_self: Pb2DistanceProxy; const [ref] d: b2Vec2): Pb2Vec2; cdecl; external LIB_NAME name _PU + 'b2DistanceProxy_GetSupportVertex'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2DistanceProxy.GetSupportVertex(const [ref] d: b2Vec2): Pb2Vec2; cdecl;
begin
  Result := b2DistanceProxy_GetSupportVertex(@Self, d)
end;

function b2DistanceProxy_GetVertexCount(_self: Pb2DistanceProxy): Integer; cdecl; external LIB_NAME name _PU + 'b2DistanceProxy_GetVertexCount'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2DistanceProxy.GetVertexCount: Integer; cdecl;
begin
  Result := b2DistanceProxy_GetVertexCount(@Self)
end;

function b2DistanceProxy_GetVertex(_self: Pb2DistanceProxy; index: Integer): Pb2Vec2; cdecl; external LIB_NAME name _PU + 'b2DistanceProxy_GetVertex'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2DistanceProxy.GetVertex(index: Integer): Pb2Vec2; cdecl;
begin
  Result := b2DistanceProxy_GetVertex(@Self, index)
end;


function b2SimplexCache_Create: b2SimplexCache; cdecl; external LIB_NAME name _PU + 'b2SimplexCache_b2SimplexCache'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2SimplexCache.Create: b2SimplexCache; cdecl;
begin
  Result := b2SimplexCache_Create;
end;


function b2DistanceInput_Create: b2DistanceInput; cdecl; external LIB_NAME name _PU + 'b2DistanceInput_b2DistanceInput'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2DistanceInput.Create: b2DistanceInput; cdecl;
begin
  Result := b2DistanceInput_Create;
end;


function b2DistanceOutput_Create: b2DistanceOutput; cdecl; external LIB_NAME name _PU + 'b2DistanceOutput_b2DistanceOutput'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2DistanceOutput.Create: b2DistanceOutput; cdecl;
begin
  Result := b2DistanceOutput_Create;
end;


function b2TOIInput_Create: b2TOIInput; cdecl; external LIB_NAME name _PU + 'b2TOIInput_b2TOIInput'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2TOIInput.Create: b2TOIInput; cdecl;
begin
  Result := b2TOIInput_Create;
end;


function b2TOIOutput_Create: b2TOIOutput; cdecl; external LIB_NAME name _PU + 'b2TOIOutput_b2TOIOutput'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2TOIOutput.Create: b2TOIOutput; cdecl;
begin
  Result := b2TOIOutput_Create;
end;


function b2MassData_Create: b2MassData; cdecl; external LIB_NAME name _PU + 'b2MassData_b2MassData'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2MassData.Create: b2MassData; cdecl;
begin
  Result := b2MassData_Create;
end;


class operator b2ShapeWrapper.Implicit(handle: b2ShapeHandle): b2ShapeWrapper;
begin
  Result.FHandle := handle;
end;
class operator b2ShapeWrapper.Implicit(wrapper: b2ShapeWrapper): b2ShapeHandle;
begin
  Result := wrapper.FHandle;
end;

function b2Shape_Clone(_self: b2ShapeHandle; allocator: b2BlockAllocatorHandle): b2ShapeHandle; cdecl; external LIB_NAME name _PU + 'b2Shape_Clone'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ShapeWrapper.Clone(allocator: b2BlockAllocatorHandle): b2ShapeHandle; cdecl;
begin
  Result := b2Shape_Clone(FHandle, allocator)
end;

function b2Shape_GetType(_self: b2ShapeHandle): b2ShapeWrapper.Type_; cdecl; external LIB_NAME name _PU + 'b2Shape_GetType'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ShapeWrapper.GetType: b2ShapeWrapper.Type_; cdecl;
begin
  Result := b2Shape_GetType(FHandle)
end;

function b2Shape_GetChildCount(_self: b2ShapeHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2Shape_GetChildCount'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ShapeWrapper.GetChildCount: Integer; cdecl;
begin
  Result := b2Shape_GetChildCount(FHandle)
end;

function b2Shape_TestPoint(_self: b2ShapeHandle; const [ref] xf: b2Transform; const [ref] p: b2Vec2): Boolean; cdecl; external LIB_NAME name _PU + 'b2Shape_TestPoint'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ShapeWrapper.TestPoint(const [ref] xf: b2Transform; const [ref] p: b2Vec2): Boolean; cdecl;
begin
  Result := b2Shape_TestPoint(FHandle, xf, p)
end;

function b2Shape_RayCast(_self: b2ShapeHandle; output: Pb2RayCastOutput; const [ref] input: b2RayCastInput; const [ref] transform: b2Transform; childIndex: Integer): Boolean; cdecl; external LIB_NAME name _PU + 'b2Shape_RayCast'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ShapeWrapper.RayCast(output: Pb2RayCastOutput; const [ref] input: b2RayCastInput; const [ref] transform: b2Transform; childIndex: Integer): Boolean; cdecl;
begin
  Result := b2Shape_RayCast(FHandle, output, input, transform, childIndex)
end;

procedure b2Shape_ComputeAABB(_self: b2ShapeHandle; aabb: Pb2AABB; const [ref] xf: b2Transform; childIndex: Integer); cdecl; external LIB_NAME name _PU + 'b2Shape_ComputeAABB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ShapeWrapper.ComputeAABB(aabb: Pb2AABB; const [ref] xf: b2Transform; childIndex: Integer); cdecl;
begin
  b2Shape_ComputeAABB(FHandle, aabb, xf, childIndex)
end;

procedure b2Shape_ComputeMass(_self: b2ShapeHandle; massData: Pb2MassData; density: Single); cdecl; external LIB_NAME name _PU + 'b2Shape_ComputeMass'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ShapeWrapper.ComputeMass(massData: Pb2MassData; density: Single); cdecl;
begin
  b2Shape_ComputeMass(FHandle, massData, density)
end;

function b2Shape_Get_m_type(_self: b2ShapeHandle): b2ShapeWrapper.Type_; cdecl; external LIB_NAME name _PU + 'b2Shape_Get_m_type'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Shape_Set_m_type(_self: b2ShapeHandle; aNewValue: b2ShapeWrapper.Type_); cdecl; external LIB_NAME name _PU + 'b2Shape_Set_m_type'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ShapeWrapper.Get_m_type: b2ShapeWrapper.Type_; cdecl;
begin
  Result := b2Shape_Get_m_type(FHandle);
end;

procedure b2ShapeWrapper.Set_m_type(aNewValue: b2ShapeWrapper.Type_); cdecl;
begin
  b2Shape_Set_m_type(FHandle, aNewValue);
end;

function b2Shape_Get_m_radius(_self: b2ShapeHandle): Single; cdecl; external LIB_NAME name _PU + 'b2Shape_Get_m_radius'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Shape_Set_m_radius(_self: b2ShapeHandle; aNewValue: Single); cdecl; external LIB_NAME name _PU + 'b2Shape_Set_m_radius'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ShapeWrapper.Get_m_radius: Single; cdecl;
begin
  Result := b2Shape_Get_m_radius(FHandle);
end;

procedure b2ShapeWrapper.Set_m_radius(aNewValue: Single); cdecl;
begin
  b2Shape_Set_m_radius(FHandle, aNewValue);
end;


function b2ChainShape_Create: b2ChainShapeHandle; cdecl; external LIB_NAME name _PU + 'b2ChainShape_b2ChainShape_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2ChainShapeWrapper.Create: b2ChainShapeWrapper; cdecl;
begin
  Result.FHandle := b2ChainShape_Create;
end;

procedure b2ChainShape_Destroy(_self: b2ChainShapeHandle); cdecl; external LIB_NAME name _PU + 'b2ChainShape_dtor'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ChainShapeWrapper.Destroy; cdecl;
begin
  b2ChainShape_Destroy(FHandle);
end;

class operator b2ChainShapeWrapper.Implicit(handle: b2ChainShapeHandle): b2ChainShapeWrapper;
begin
  Result.FHandle := handle;
end;
class operator b2ChainShapeWrapper.Implicit(wrapper: b2ChainShapeWrapper): b2ChainShapeHandle;
begin
  Result := wrapper.FHandle;
end;

function b2ChainShape_Clone(_self: b2ChainShapeHandle; allocator: b2BlockAllocatorHandle): b2ShapeHandle; cdecl; external LIB_NAME name _PU + 'b2ChainShape_Clone'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ChainShapeWrapper.Clone(allocator: b2BlockAllocatorHandle): b2ShapeHandle; cdecl;
begin
  Result := b2ChainShape_Clone(FHandle, allocator)
end;

function b2ChainShape_GetType(_self: b2ChainShapeHandle): b2ShapeWrapper.Type_; cdecl; external LIB_NAME name _PU + 'b2ChainShape_GetType'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ChainShapeWrapper.GetType: b2ShapeWrapper.Type_; cdecl;
begin
  Result := b2ChainShape_GetType(FHandle)
end;

function b2ChainShape_GetChildCount(_self: b2ChainShapeHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2ChainShape_GetChildCount'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ChainShapeWrapper.GetChildCount: Integer; cdecl;
begin
  Result := b2ChainShape_GetChildCount(FHandle)
end;

function b2ChainShape_TestPoint(_self: b2ChainShapeHandle; const [ref] xf: b2Transform; const [ref] p: b2Vec2): Boolean; cdecl; external LIB_NAME name _PU + 'b2ChainShape_TestPoint'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ChainShapeWrapper.TestPoint(const [ref] xf: b2Transform; const [ref] p: b2Vec2): Boolean; cdecl;
begin
  Result := b2ChainShape_TestPoint(FHandle, xf, p)
end;

function b2ChainShape_RayCast(_self: b2ChainShapeHandle; output: Pb2RayCastOutput; const [ref] input: b2RayCastInput; const [ref] transform: b2Transform; childIndex: Integer): Boolean; cdecl; external LIB_NAME name _PU + 'b2ChainShape_RayCast'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ChainShapeWrapper.RayCast(output: Pb2RayCastOutput; const [ref] input: b2RayCastInput; const [ref] transform: b2Transform; childIndex: Integer): Boolean; cdecl;
begin
  Result := b2ChainShape_RayCast(FHandle, output, input, transform, childIndex)
end;

procedure b2ChainShape_ComputeAABB(_self: b2ChainShapeHandle; aabb: Pb2AABB; const [ref] xf: b2Transform; childIndex: Integer); cdecl; external LIB_NAME name _PU + 'b2ChainShape_ComputeAABB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ChainShapeWrapper.ComputeAABB(aabb: Pb2AABB; const [ref] xf: b2Transform; childIndex: Integer); cdecl;
begin
  b2ChainShape_ComputeAABB(FHandle, aabb, xf, childIndex)
end;

procedure b2ChainShape_ComputeMass(_self: b2ChainShapeHandle; massData: Pb2MassData; density: Single); cdecl; external LIB_NAME name _PU + 'b2ChainShape_ComputeMass'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ChainShapeWrapper.ComputeMass(massData: Pb2MassData; density: Single); cdecl;
begin
  b2ChainShape_ComputeMass(FHandle, massData, density)
end;

function b2ChainShape_Get_m_type(_self: b2ChainShapeHandle): b2ShapeWrapper.Type_; cdecl; external LIB_NAME name _PU + 'b2ChainShape_Get_m_type'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ChainShape_Set_m_type(_self: b2ChainShapeHandle; aNewValue: b2ShapeWrapper.Type_); cdecl; external LIB_NAME name _PU + 'b2ChainShape_Set_m_type'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ChainShapeWrapper.Get_m_type: b2ShapeWrapper.Type_; cdecl;
begin
  Result := b2ChainShape_Get_m_type(FHandle);
end;

procedure b2ChainShapeWrapper.Set_m_type(aNewValue: b2ShapeWrapper.Type_); cdecl;
begin
  b2ChainShape_Set_m_type(FHandle, aNewValue);
end;

function b2ChainShape_Get_m_radius(_self: b2ChainShapeHandle): Single; cdecl; external LIB_NAME name _PU + 'b2ChainShape_Get_m_radius'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ChainShape_Set_m_radius(_self: b2ChainShapeHandle; aNewValue: Single); cdecl; external LIB_NAME name _PU + 'b2ChainShape_Set_m_radius'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ChainShapeWrapper.Get_m_radius: Single; cdecl;
begin
  Result := b2ChainShape_Get_m_radius(FHandle);
end;

procedure b2ChainShapeWrapper.Set_m_radius(aNewValue: Single); cdecl;
begin
  b2ChainShape_Set_m_radius(FHandle, aNewValue);
end;

procedure b2ChainShape_Clear(_self: b2ChainShapeHandle); cdecl; external LIB_NAME name _PU + 'b2ChainShape_Clear'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ChainShapeWrapper.Clear; cdecl;
begin
  b2ChainShape_Clear(FHandle)
end;

procedure b2ChainShape_CreateLoop(_self: b2ChainShapeHandle; vertices: Pb2Vec2; count: Integer); cdecl; external LIB_NAME name _PU + 'b2ChainShape_CreateLoop'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ChainShapeWrapper.CreateLoop(vertices: Pb2Vec2; count: Integer); cdecl;
begin
  b2ChainShape_CreateLoop(FHandle, vertices, count)
end;

procedure b2ChainShape_CreateChain(_self: b2ChainShapeHandle; vertices: Pb2Vec2; count: Integer); cdecl; external LIB_NAME name _PU + 'b2ChainShape_CreateChain'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ChainShapeWrapper.CreateChain(vertices: Pb2Vec2; count: Integer); cdecl;
begin
  b2ChainShape_CreateChain(FHandle, vertices, count)
end;

procedure b2ChainShape_SetPrevVertex(_self: b2ChainShapeHandle; const [ref] prevVertex: b2Vec2); cdecl; external LIB_NAME name _PU + 'b2ChainShape_SetPrevVertex'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ChainShapeWrapper.SetPrevVertex(const [ref] prevVertex: b2Vec2); cdecl;
begin
  b2ChainShape_SetPrevVertex(FHandle, prevVertex)
end;

procedure b2ChainShape_SetNextVertex(_self: b2ChainShapeHandle; const [ref] nextVertex: b2Vec2); cdecl; external LIB_NAME name _PU + 'b2ChainShape_SetNextVertex'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ChainShapeWrapper.SetNextVertex(const [ref] nextVertex: b2Vec2); cdecl;
begin
  b2ChainShape_SetNextVertex(FHandle, nextVertex)
end;

procedure b2ChainShape_GetChildEdge(_self: b2ChainShapeHandle; edge: b2EdgeShapeHandle; index: Integer); cdecl; external LIB_NAME name _PU + 'b2ChainShape_GetChildEdge'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ChainShapeWrapper.GetChildEdge(edge: b2EdgeShapeHandle; index: Integer); cdecl;
begin
  b2ChainShape_GetChildEdge(FHandle, edge, index)
end;

function b2ChainShape_Get_m_vertices(_self: b2ChainShapeHandle): Pb2Vec2; cdecl; external LIB_NAME name _PU + 'b2ChainShape_Get_m_vertices'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ChainShape_Set_m_vertices(_self: b2ChainShapeHandle; aNewValue: Pb2Vec2); cdecl; external LIB_NAME name _PU + 'b2ChainShape_Set_m_vertices'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ChainShapeWrapper.Get_m_vertices: Pb2Vec2; cdecl;
begin
  Result := b2ChainShape_Get_m_vertices(FHandle);
end;

procedure b2ChainShapeWrapper.Set_m_vertices(aNewValue: Pb2Vec2); cdecl;
begin
  b2ChainShape_Set_m_vertices(FHandle, aNewValue);
end;

function b2ChainShape_Get_m_count(_self: b2ChainShapeHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2ChainShape_Get_m_count'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ChainShape_Set_m_count(_self: b2ChainShapeHandle; aNewValue: Integer); cdecl; external LIB_NAME name _PU + 'b2ChainShape_Set_m_count'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ChainShapeWrapper.Get_m_count: Integer; cdecl;
begin
  Result := b2ChainShape_Get_m_count(FHandle);
end;

procedure b2ChainShapeWrapper.Set_m_count(aNewValue: Integer); cdecl;
begin
  b2ChainShape_Set_m_count(FHandle, aNewValue);
end;

function b2ChainShape_Get_m_prevVertex(_self: b2ChainShapeHandle): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2ChainShape_Get_m_prevVertex'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ChainShape_Set_m_prevVertex(_self: b2ChainShapeHandle; aNewValue: b2Vec2); cdecl; external LIB_NAME name _PU + 'b2ChainShape_Set_m_prevVertex'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ChainShapeWrapper.Get_m_prevVertex: b2Vec2; cdecl;
begin
  Result := b2ChainShape_Get_m_prevVertex(FHandle);
end;

procedure b2ChainShapeWrapper.Set_m_prevVertex(aNewValue: b2Vec2); cdecl;
begin
  b2ChainShape_Set_m_prevVertex(FHandle, aNewValue);
end;

function b2ChainShape_Get_m_nextVertex(_self: b2ChainShapeHandle): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2ChainShape_Get_m_nextVertex'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ChainShape_Set_m_nextVertex(_self: b2ChainShapeHandle; aNewValue: b2Vec2); cdecl; external LIB_NAME name _PU + 'b2ChainShape_Set_m_nextVertex'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ChainShapeWrapper.Get_m_nextVertex: b2Vec2; cdecl;
begin
  Result := b2ChainShape_Get_m_nextVertex(FHandle);
end;

procedure b2ChainShapeWrapper.Set_m_nextVertex(aNewValue: b2Vec2); cdecl;
begin
  b2ChainShape_Set_m_nextVertex(FHandle, aNewValue);
end;

function b2ChainShape_Get_m_hasPrevVertex(_self: b2ChainShapeHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2ChainShape_Get_m_hasPrevVertex'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ChainShape_Set_m_hasPrevVertex(_self: b2ChainShapeHandle; aNewValue: Boolean); cdecl; external LIB_NAME name _PU + 'b2ChainShape_Set_m_hasPrevVertex'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ChainShapeWrapper.Get_m_hasPrevVertex: Boolean; cdecl;
begin
  Result := b2ChainShape_Get_m_hasPrevVertex(FHandle);
end;

procedure b2ChainShapeWrapper.Set_m_hasPrevVertex(aNewValue: Boolean); cdecl;
begin
  b2ChainShape_Set_m_hasPrevVertex(FHandle, aNewValue);
end;

function b2ChainShape_Get_m_hasNextVertex(_self: b2ChainShapeHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2ChainShape_Get_m_hasNextVertex'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2ChainShape_Set_m_hasNextVertex(_self: b2ChainShapeHandle; aNewValue: Boolean); cdecl; external LIB_NAME name _PU + 'b2ChainShape_Set_m_hasNextVertex'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ChainShapeWrapper.Get_m_hasNextVertex: Boolean; cdecl;
begin
  Result := b2ChainShape_Get_m_hasNextVertex(FHandle);
end;

procedure b2ChainShapeWrapper.Set_m_hasNextVertex(aNewValue: Boolean); cdecl;
begin
  b2ChainShape_Set_m_hasNextVertex(FHandle, aNewValue);
end;


function b2CircleShape_Create: b2CircleShapeHandle; cdecl; external LIB_NAME name _PU + 'b2CircleShape_b2CircleShape_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2CircleShapeWrapper.Create: b2CircleShapeWrapper; cdecl;
begin
  Result.FHandle := b2CircleShape_Create;
end;

procedure b2CircleShape_Destroy(_self: b2CircleShapeHandle); cdecl; external LIB_NAME name _PU + 'b2CircleShape_dtor'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2CircleShapeWrapper.Destroy; cdecl;
begin
  b2CircleShape_Destroy(FHandle);
end;

class operator b2CircleShapeWrapper.Implicit(handle: b2CircleShapeHandle): b2CircleShapeWrapper;
begin
  Result.FHandle := handle;
end;
class operator b2CircleShapeWrapper.Implicit(wrapper: b2CircleShapeWrapper): b2CircleShapeHandle;
begin
  Result := wrapper.FHandle;
end;

function b2CircleShape_Clone(_self: b2CircleShapeHandle; allocator: b2BlockAllocatorHandle): b2ShapeHandle; cdecl; external LIB_NAME name _PU + 'b2CircleShape_Clone'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2CircleShapeWrapper.Clone(allocator: b2BlockAllocatorHandle): b2ShapeHandle; cdecl;
begin
  Result := b2CircleShape_Clone(FHandle, allocator)
end;

function b2CircleShape_GetType(_self: b2CircleShapeHandle): b2ShapeWrapper.Type_; cdecl; external LIB_NAME name _PU + 'b2CircleShape_GetType'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2CircleShapeWrapper.GetType: b2ShapeWrapper.Type_; cdecl;
begin
  Result := b2CircleShape_GetType(FHandle)
end;

function b2CircleShape_GetChildCount(_self: b2CircleShapeHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2CircleShape_GetChildCount'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2CircleShapeWrapper.GetChildCount: Integer; cdecl;
begin
  Result := b2CircleShape_GetChildCount(FHandle)
end;

function b2CircleShape_TestPoint(_self: b2CircleShapeHandle; const [ref] xf: b2Transform; const [ref] p: b2Vec2): Boolean; cdecl; external LIB_NAME name _PU + 'b2CircleShape_TestPoint'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2CircleShapeWrapper.TestPoint(const [ref] xf: b2Transform; const [ref] p: b2Vec2): Boolean; cdecl;
begin
  Result := b2CircleShape_TestPoint(FHandle, xf, p)
end;

function b2CircleShape_RayCast(_self: b2CircleShapeHandle; output: Pb2RayCastOutput; const [ref] input: b2RayCastInput; const [ref] transform: b2Transform; childIndex: Integer): Boolean; cdecl; external LIB_NAME name _PU + 'b2CircleShape_RayCast'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2CircleShapeWrapper.RayCast(output: Pb2RayCastOutput; const [ref] input: b2RayCastInput; const [ref] transform: b2Transform; childIndex: Integer): Boolean; cdecl;
begin
  Result := b2CircleShape_RayCast(FHandle, output, input, transform, childIndex)
end;

procedure b2CircleShape_ComputeAABB(_self: b2CircleShapeHandle; aabb: Pb2AABB; const [ref] xf: b2Transform; childIndex: Integer); cdecl; external LIB_NAME name _PU + 'b2CircleShape_ComputeAABB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2CircleShapeWrapper.ComputeAABB(aabb: Pb2AABB; const [ref] xf: b2Transform; childIndex: Integer); cdecl;
begin
  b2CircleShape_ComputeAABB(FHandle, aabb, xf, childIndex)
end;

procedure b2CircleShape_ComputeMass(_self: b2CircleShapeHandle; massData: Pb2MassData; density: Single); cdecl; external LIB_NAME name _PU + 'b2CircleShape_ComputeMass'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2CircleShapeWrapper.ComputeMass(massData: Pb2MassData; density: Single); cdecl;
begin
  b2CircleShape_ComputeMass(FHandle, massData, density)
end;

function b2CircleShape_Get_m_type(_self: b2CircleShapeHandle): b2ShapeWrapper.Type_; cdecl; external LIB_NAME name _PU + 'b2CircleShape_Get_m_type'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2CircleShape_Set_m_type(_self: b2CircleShapeHandle; aNewValue: b2ShapeWrapper.Type_); cdecl; external LIB_NAME name _PU + 'b2CircleShape_Set_m_type'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2CircleShapeWrapper.Get_m_type: b2ShapeWrapper.Type_; cdecl;
begin
  Result := b2CircleShape_Get_m_type(FHandle);
end;

procedure b2CircleShapeWrapper.Set_m_type(aNewValue: b2ShapeWrapper.Type_); cdecl;
begin
  b2CircleShape_Set_m_type(FHandle, aNewValue);
end;

function b2CircleShape_Get_m_radius(_self: b2CircleShapeHandle): Single; cdecl; external LIB_NAME name _PU + 'b2CircleShape_Get_m_radius'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2CircleShape_Set_m_radius(_self: b2CircleShapeHandle; aNewValue: Single); cdecl; external LIB_NAME name _PU + 'b2CircleShape_Set_m_radius'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2CircleShapeWrapper.Get_m_radius: Single; cdecl;
begin
  Result := b2CircleShape_Get_m_radius(FHandle);
end;

procedure b2CircleShapeWrapper.Set_m_radius(aNewValue: Single); cdecl;
begin
  b2CircleShape_Set_m_radius(FHandle, aNewValue);
end;

function b2CircleShape_GetSupport(_self: b2CircleShapeHandle; const [ref] d: b2Vec2): Integer; cdecl; external LIB_NAME name _PU + 'b2CircleShape_GetSupport'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2CircleShapeWrapper.GetSupport(const [ref] d: b2Vec2): Integer; cdecl;
begin
  Result := b2CircleShape_GetSupport(FHandle, d)
end;

function b2CircleShape_GetSupportVertex(_self: b2CircleShapeHandle; const [ref] d: b2Vec2): Pb2Vec2; cdecl; external LIB_NAME name _PU + 'b2CircleShape_GetSupportVertex'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2CircleShapeWrapper.GetSupportVertex(const [ref] d: b2Vec2): Pb2Vec2; cdecl;
begin
  Result := b2CircleShape_GetSupportVertex(FHandle, d)
end;

function b2CircleShape_GetVertexCount(_self: b2CircleShapeHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2CircleShape_GetVertexCount'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2CircleShapeWrapper.GetVertexCount: Integer; cdecl;
begin
  Result := b2CircleShape_GetVertexCount(FHandle)
end;

function b2CircleShape_GetVertex(_self: b2CircleShapeHandle; index: Integer): Pb2Vec2; cdecl; external LIB_NAME name _PU + 'b2CircleShape_GetVertex'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2CircleShapeWrapper.GetVertex(index: Integer): Pb2Vec2; cdecl;
begin
  Result := b2CircleShape_GetVertex(FHandle, index)
end;

function b2CircleShape_Get_m_p(_self: b2CircleShapeHandle): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2CircleShape_Get_m_p'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2CircleShape_Set_m_p(_self: b2CircleShapeHandle; aNewValue: b2Vec2); cdecl; external LIB_NAME name _PU + 'b2CircleShape_Set_m_p'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2CircleShapeWrapper.Get_m_p: b2Vec2; cdecl;
begin
  Result := b2CircleShape_Get_m_p(FHandle);
end;

procedure b2CircleShapeWrapper.Set_m_p(aNewValue: b2Vec2); cdecl;
begin
  b2CircleShape_Set_m_p(FHandle, aNewValue);
end;


function b2EdgeShape_Create: b2EdgeShapeHandle; cdecl; external LIB_NAME name _PU + 'b2EdgeShape_b2EdgeShape_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2EdgeShapeWrapper.Create: b2EdgeShapeWrapper; cdecl;
begin
  Result.FHandle := b2EdgeShape_Create;
end;

procedure b2EdgeShape_Destroy(_self: b2EdgeShapeHandle); cdecl; external LIB_NAME name _PU + 'b2EdgeShape_dtor'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2EdgeShapeWrapper.Destroy; cdecl;
begin
  b2EdgeShape_Destroy(FHandle);
end;

class operator b2EdgeShapeWrapper.Implicit(handle: b2EdgeShapeHandle): b2EdgeShapeWrapper;
begin
  Result.FHandle := handle;
end;
class operator b2EdgeShapeWrapper.Implicit(wrapper: b2EdgeShapeWrapper): b2EdgeShapeHandle;
begin
  Result := wrapper.FHandle;
end;

function b2EdgeShape_Clone(_self: b2EdgeShapeHandle; allocator: b2BlockAllocatorHandle): b2ShapeHandle; cdecl; external LIB_NAME name _PU + 'b2EdgeShape_Clone'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2EdgeShapeWrapper.Clone(allocator: b2BlockAllocatorHandle): b2ShapeHandle; cdecl;
begin
  Result := b2EdgeShape_Clone(FHandle, allocator)
end;

function b2EdgeShape_GetType(_self: b2EdgeShapeHandle): b2ShapeWrapper.Type_; cdecl; external LIB_NAME name _PU + 'b2EdgeShape_GetType'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2EdgeShapeWrapper.GetType: b2ShapeWrapper.Type_; cdecl;
begin
  Result := b2EdgeShape_GetType(FHandle)
end;

function b2EdgeShape_GetChildCount(_self: b2EdgeShapeHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2EdgeShape_GetChildCount'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2EdgeShapeWrapper.GetChildCount: Integer; cdecl;
begin
  Result := b2EdgeShape_GetChildCount(FHandle)
end;

function b2EdgeShape_TestPoint(_self: b2EdgeShapeHandle; const [ref] xf: b2Transform; const [ref] p: b2Vec2): Boolean; cdecl; external LIB_NAME name _PU + 'b2EdgeShape_TestPoint'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2EdgeShapeWrapper.TestPoint(const [ref] xf: b2Transform; const [ref] p: b2Vec2): Boolean; cdecl;
begin
  Result := b2EdgeShape_TestPoint(FHandle, xf, p)
end;

function b2EdgeShape_RayCast(_self: b2EdgeShapeHandle; output: Pb2RayCastOutput; const [ref] input: b2RayCastInput; const [ref] transform: b2Transform; childIndex: Integer): Boolean; cdecl; external LIB_NAME name _PU + 'b2EdgeShape_RayCast'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2EdgeShapeWrapper.RayCast(output: Pb2RayCastOutput; const [ref] input: b2RayCastInput; const [ref] transform: b2Transform; childIndex: Integer): Boolean; cdecl;
begin
  Result := b2EdgeShape_RayCast(FHandle, output, input, transform, childIndex)
end;

procedure b2EdgeShape_ComputeAABB(_self: b2EdgeShapeHandle; aabb: Pb2AABB; const [ref] xf: b2Transform; childIndex: Integer); cdecl; external LIB_NAME name _PU + 'b2EdgeShape_ComputeAABB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2EdgeShapeWrapper.ComputeAABB(aabb: Pb2AABB; const [ref] xf: b2Transform; childIndex: Integer); cdecl;
begin
  b2EdgeShape_ComputeAABB(FHandle, aabb, xf, childIndex)
end;

procedure b2EdgeShape_ComputeMass(_self: b2EdgeShapeHandle; massData: Pb2MassData; density: Single); cdecl; external LIB_NAME name _PU + 'b2EdgeShape_ComputeMass'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2EdgeShapeWrapper.ComputeMass(massData: Pb2MassData; density: Single); cdecl;
begin
  b2EdgeShape_ComputeMass(FHandle, massData, density)
end;

function b2EdgeShape_Get_m_type(_self: b2EdgeShapeHandle): b2ShapeWrapper.Type_; cdecl; external LIB_NAME name _PU + 'b2EdgeShape_Get_m_type'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2EdgeShape_Set_m_type(_self: b2EdgeShapeHandle; aNewValue: b2ShapeWrapper.Type_); cdecl; external LIB_NAME name _PU + 'b2EdgeShape_Set_m_type'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2EdgeShapeWrapper.Get_m_type: b2ShapeWrapper.Type_; cdecl;
begin
  Result := b2EdgeShape_Get_m_type(FHandle);
end;

procedure b2EdgeShapeWrapper.Set_m_type(aNewValue: b2ShapeWrapper.Type_); cdecl;
begin
  b2EdgeShape_Set_m_type(FHandle, aNewValue);
end;

function b2EdgeShape_Get_m_radius(_self: b2EdgeShapeHandle): Single; cdecl; external LIB_NAME name _PU + 'b2EdgeShape_Get_m_radius'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2EdgeShape_Set_m_radius(_self: b2EdgeShapeHandle; aNewValue: Single); cdecl; external LIB_NAME name _PU + 'b2EdgeShape_Set_m_radius'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2EdgeShapeWrapper.Get_m_radius: Single; cdecl;
begin
  Result := b2EdgeShape_Get_m_radius(FHandle);
end;

procedure b2EdgeShapeWrapper.Set_m_radius(aNewValue: Single); cdecl;
begin
  b2EdgeShape_Set_m_radius(FHandle, aNewValue);
end;

procedure b2EdgeShape__Set(_self: b2EdgeShapeHandle; const [ref] v1: b2Vec2; const [ref] v2: b2Vec2); cdecl; external LIB_NAME name _PU + 'b2EdgeShape_Set'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2EdgeShapeWrapper.&Set(const [ref] v1: b2Vec2; const [ref] v2: b2Vec2); cdecl;
begin
  b2EdgeShape__Set(FHandle, v1, v2)
end;

function b2EdgeShape_Get_m_vertex1(_self: b2EdgeShapeHandle): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2EdgeShape_Get_m_vertex1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2EdgeShape_Set_m_vertex1(_self: b2EdgeShapeHandle; aNewValue: b2Vec2); cdecl; external LIB_NAME name _PU + 'b2EdgeShape_Set_m_vertex1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2EdgeShapeWrapper.Get_m_vertex1: b2Vec2; cdecl;
begin
  Result := b2EdgeShape_Get_m_vertex1(FHandle);
end;

procedure b2EdgeShapeWrapper.Set_m_vertex1(aNewValue: b2Vec2); cdecl;
begin
  b2EdgeShape_Set_m_vertex1(FHandle, aNewValue);
end;

function b2EdgeShape_Get_m_vertex2(_self: b2EdgeShapeHandle): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2EdgeShape_Get_m_vertex2'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2EdgeShape_Set_m_vertex2(_self: b2EdgeShapeHandle; aNewValue: b2Vec2); cdecl; external LIB_NAME name _PU + 'b2EdgeShape_Set_m_vertex2'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2EdgeShapeWrapper.Get_m_vertex2: b2Vec2; cdecl;
begin
  Result := b2EdgeShape_Get_m_vertex2(FHandle);
end;

procedure b2EdgeShapeWrapper.Set_m_vertex2(aNewValue: b2Vec2); cdecl;
begin
  b2EdgeShape_Set_m_vertex2(FHandle, aNewValue);
end;

function b2EdgeShape_Get_m_vertex0(_self: b2EdgeShapeHandle): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2EdgeShape_Get_m_vertex0'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2EdgeShape_Set_m_vertex0(_self: b2EdgeShapeHandle; aNewValue: b2Vec2); cdecl; external LIB_NAME name _PU + 'b2EdgeShape_Set_m_vertex0'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2EdgeShapeWrapper.Get_m_vertex0: b2Vec2; cdecl;
begin
  Result := b2EdgeShape_Get_m_vertex0(FHandle);
end;

procedure b2EdgeShapeWrapper.Set_m_vertex0(aNewValue: b2Vec2); cdecl;
begin
  b2EdgeShape_Set_m_vertex0(FHandle, aNewValue);
end;

function b2EdgeShape_Get_m_vertex3(_self: b2EdgeShapeHandle): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2EdgeShape_Get_m_vertex3'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2EdgeShape_Set_m_vertex3(_self: b2EdgeShapeHandle; aNewValue: b2Vec2); cdecl; external LIB_NAME name _PU + 'b2EdgeShape_Set_m_vertex3'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2EdgeShapeWrapper.Get_m_vertex3: b2Vec2; cdecl;
begin
  Result := b2EdgeShape_Get_m_vertex3(FHandle);
end;

procedure b2EdgeShapeWrapper.Set_m_vertex3(aNewValue: b2Vec2); cdecl;
begin
  b2EdgeShape_Set_m_vertex3(FHandle, aNewValue);
end;

function b2EdgeShape_Get_m_hasVertex0(_self: b2EdgeShapeHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2EdgeShape_Get_m_hasVertex0'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2EdgeShape_Set_m_hasVertex0(_self: b2EdgeShapeHandle; aNewValue: Boolean); cdecl; external LIB_NAME name _PU + 'b2EdgeShape_Set_m_hasVertex0'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2EdgeShapeWrapper.Get_m_hasVertex0: Boolean; cdecl;
begin
  Result := b2EdgeShape_Get_m_hasVertex0(FHandle);
end;

procedure b2EdgeShapeWrapper.Set_m_hasVertex0(aNewValue: Boolean); cdecl;
begin
  b2EdgeShape_Set_m_hasVertex0(FHandle, aNewValue);
end;

function b2EdgeShape_Get_m_hasVertex3(_self: b2EdgeShapeHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2EdgeShape_Get_m_hasVertex3'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2EdgeShape_Set_m_hasVertex3(_self: b2EdgeShapeHandle; aNewValue: Boolean); cdecl; external LIB_NAME name _PU + 'b2EdgeShape_Set_m_hasVertex3'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2EdgeShapeWrapper.Get_m_hasVertex3: Boolean; cdecl;
begin
  Result := b2EdgeShape_Get_m_hasVertex3(FHandle);
end;

procedure b2EdgeShapeWrapper.Set_m_hasVertex3(aNewValue: Boolean); cdecl;
begin
  b2EdgeShape_Set_m_hasVertex3(FHandle, aNewValue);
end;


function b2PolygonShape_Create: b2PolygonShapeHandle; cdecl; external LIB_NAME name _PU + 'b2PolygonShape_b2PolygonShape_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2PolygonShapeWrapper.Create: b2PolygonShapeWrapper; cdecl;
begin
  Result.FHandle := b2PolygonShape_Create;
end;

procedure b2PolygonShape_Destroy(_self: b2PolygonShapeHandle); cdecl; external LIB_NAME name _PU + 'b2PolygonShape_dtor'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PolygonShapeWrapper.Destroy; cdecl;
begin
  b2PolygonShape_Destroy(FHandle);
end;

class operator b2PolygonShapeWrapper.Implicit(handle: b2PolygonShapeHandle): b2PolygonShapeWrapper;
begin
  Result.FHandle := handle;
end;
class operator b2PolygonShapeWrapper.Implicit(wrapper: b2PolygonShapeWrapper): b2PolygonShapeHandle;
begin
  Result := wrapper.FHandle;
end;

function b2PolygonShape_Clone(_self: b2PolygonShapeHandle; allocator: b2BlockAllocatorHandle): b2ShapeHandle; cdecl; external LIB_NAME name _PU + 'b2PolygonShape_Clone'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PolygonShapeWrapper.Clone(allocator: b2BlockAllocatorHandle): b2ShapeHandle; cdecl;
begin
  Result := b2PolygonShape_Clone(FHandle, allocator)
end;

function b2PolygonShape_GetType(_self: b2PolygonShapeHandle): b2ShapeWrapper.Type_; cdecl; external LIB_NAME name _PU + 'b2PolygonShape_GetType'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PolygonShapeWrapper.GetType: b2ShapeWrapper.Type_; cdecl;
begin
  Result := b2PolygonShape_GetType(FHandle)
end;

function b2PolygonShape_GetChildCount(_self: b2PolygonShapeHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2PolygonShape_GetChildCount'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PolygonShapeWrapper.GetChildCount: Integer; cdecl;
begin
  Result := b2PolygonShape_GetChildCount(FHandle)
end;

function b2PolygonShape_TestPoint(_self: b2PolygonShapeHandle; const [ref] xf: b2Transform; const [ref] p: b2Vec2): Boolean; cdecl; external LIB_NAME name _PU + 'b2PolygonShape_TestPoint'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PolygonShapeWrapper.TestPoint(const [ref] xf: b2Transform; const [ref] p: b2Vec2): Boolean; cdecl;
begin
  Result := b2PolygonShape_TestPoint(FHandle, xf, p)
end;

function b2PolygonShape_RayCast(_self: b2PolygonShapeHandle; output: Pb2RayCastOutput; const [ref] input: b2RayCastInput; const [ref] transform: b2Transform; childIndex: Integer): Boolean; cdecl; external LIB_NAME name _PU + 'b2PolygonShape_RayCast'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PolygonShapeWrapper.RayCast(output: Pb2RayCastOutput; const [ref] input: b2RayCastInput; const [ref] transform: b2Transform; childIndex: Integer): Boolean; cdecl;
begin
  Result := b2PolygonShape_RayCast(FHandle, output, input, transform, childIndex)
end;

procedure b2PolygonShape_ComputeAABB(_self: b2PolygonShapeHandle; aabb: Pb2AABB; const [ref] xf: b2Transform; childIndex: Integer); cdecl; external LIB_NAME name _PU + 'b2PolygonShape_ComputeAABB'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PolygonShapeWrapper.ComputeAABB(aabb: Pb2AABB; const [ref] xf: b2Transform; childIndex: Integer); cdecl;
begin
  b2PolygonShape_ComputeAABB(FHandle, aabb, xf, childIndex)
end;

procedure b2PolygonShape_ComputeMass(_self: b2PolygonShapeHandle; massData: Pb2MassData; density: Single); cdecl; external LIB_NAME name _PU + 'b2PolygonShape_ComputeMass'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PolygonShapeWrapper.ComputeMass(massData: Pb2MassData; density: Single); cdecl;
begin
  b2PolygonShape_ComputeMass(FHandle, massData, density)
end;

function b2PolygonShape_Get_m_type(_self: b2PolygonShapeHandle): b2ShapeWrapper.Type_; cdecl; external LIB_NAME name _PU + 'b2PolygonShape_Get_m_type'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PolygonShape_Set_m_type(_self: b2PolygonShapeHandle; aNewValue: b2ShapeWrapper.Type_); cdecl; external LIB_NAME name _PU + 'b2PolygonShape_Set_m_type'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PolygonShapeWrapper.Get_m_type: b2ShapeWrapper.Type_; cdecl;
begin
  Result := b2PolygonShape_Get_m_type(FHandle);
end;

procedure b2PolygonShapeWrapper.Set_m_type(aNewValue: b2ShapeWrapper.Type_); cdecl;
begin
  b2PolygonShape_Set_m_type(FHandle, aNewValue);
end;

function b2PolygonShape_Get_m_radius(_self: b2PolygonShapeHandle): Single; cdecl; external LIB_NAME name _PU + 'b2PolygonShape_Get_m_radius'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PolygonShape_Set_m_radius(_self: b2PolygonShapeHandle; aNewValue: Single); cdecl; external LIB_NAME name _PU + 'b2PolygonShape_Set_m_radius'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PolygonShapeWrapper.Get_m_radius: Single; cdecl;
begin
  Result := b2PolygonShape_Get_m_radius(FHandle);
end;

procedure b2PolygonShapeWrapper.Set_m_radius(aNewValue: Single); cdecl;
begin
  b2PolygonShape_Set_m_radius(FHandle, aNewValue);
end;

procedure b2PolygonShape__Set(_self: b2PolygonShapeHandle; points: Pb2Vec2; count: Integer); cdecl; external LIB_NAME name _PU + 'b2PolygonShape_Set'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PolygonShapeWrapper.&Set(points: Pb2Vec2; count: Integer); cdecl;
begin
  b2PolygonShape__Set(FHandle, points, count)
end;

procedure b2PolygonShape_SetAsBox(_self: b2PolygonShapeHandle; hx: Single; hy: Single); overload; cdecl; external LIB_NAME name _PU + 'b2PolygonShape_SetAsBox'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PolygonShapeWrapper.SetAsBox(hx: Single; hy: Single); cdecl;
begin
  b2PolygonShape_SetAsBox(FHandle, hx, hy)
end;

procedure b2PolygonShape_SetAsBox(_self: b2PolygonShapeHandle; hx: Single; hy: Single; const [ref] center: b2Vec2; angle: Single); overload; cdecl; external LIB_NAME name _PU + 'b2PolygonShape_SetAsBox2'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PolygonShapeWrapper.SetAsBox(hx: Single; hy: Single; const [ref] center: b2Vec2; angle: Single); cdecl;
begin
  b2PolygonShape_SetAsBox(FHandle, hx, hy, center, angle)
end;

function b2PolygonShape_GetVertexCount(_self: b2PolygonShapeHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2PolygonShape_GetVertexCount'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PolygonShapeWrapper.GetVertexCount: Integer; cdecl;
begin
  Result := b2PolygonShape_GetVertexCount(FHandle)
end;

function b2PolygonShape_GetVertex(_self: b2PolygonShapeHandle; index: Integer): Pb2Vec2; cdecl; external LIB_NAME name _PU + 'b2PolygonShape_GetVertex'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PolygonShapeWrapper.GetVertex(index: Integer): Pb2Vec2; cdecl;
begin
  Result := b2PolygonShape_GetVertex(FHandle, index)
end;

function b2PolygonShape_Validate(_self: b2PolygonShapeHandle): Boolean; cdecl; external LIB_NAME name _PU + 'b2PolygonShape_Validate'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PolygonShapeWrapper.Validate: Boolean; cdecl;
begin
  Result := b2PolygonShape_Validate(FHandle)
end;

function b2PolygonShape_Get_m_centroid(_self: b2PolygonShapeHandle): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2PolygonShape_Get_m_centroid'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PolygonShape_Set_m_centroid(_self: b2PolygonShapeHandle; aNewValue: b2Vec2); cdecl; external LIB_NAME name _PU + 'b2PolygonShape_Set_m_centroid'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PolygonShapeWrapper.Get_m_centroid: b2Vec2; cdecl;
begin
  Result := b2PolygonShape_Get_m_centroid(FHandle);
end;

procedure b2PolygonShapeWrapper.Set_m_centroid(aNewValue: b2Vec2); cdecl;
begin
  b2PolygonShape_Set_m_centroid(FHandle, aNewValue);
end;

function b2PolygonShape_Get_m_count(_self: b2PolygonShapeHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2PolygonShape_Get_m_count'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2PolygonShape_Set_m_count(_self: b2PolygonShapeHandle; aNewValue: Integer); cdecl; external LIB_NAME name _PU + 'b2PolygonShape_Set_m_count'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PolygonShapeWrapper.Get_m_count: Integer; cdecl;
begin
  Result := b2PolygonShape_Get_m_count(FHandle);
end;

procedure b2PolygonShapeWrapper.Set_m_count(aNewValue: Integer); cdecl;
begin
  b2PolygonShape_Set_m_count(FHandle, aNewValue);
end;


function Get_b2_nullFeature; cdecl; external LIB_NAME name _PU + 'Get_b2_nullFeature'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};


procedure b2GetPointStates(state1: Pb2PointState; state2: Pb2PointState; manifold1: Pb2Manifold; manifold2: Pb2Manifold); cdecl; external LIB_NAME name _PU + 'Collision_b2GetPointStates'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2CollideCircles(manifold: Pb2Manifold; circleA: b2CircleShapeHandle; const [ref] xfA: b2Transform; circleB: b2CircleShapeHandle; const [ref] xfB: b2Transform); cdecl; external LIB_NAME name _PU + 'Collision_b2CollideCircles'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2CollidePolygonAndCircle(manifold: Pb2Manifold; polygonA: b2PolygonShapeHandle; const [ref] xfA: b2Transform; circleB: b2CircleShapeHandle; const [ref] xfB: b2Transform); cdecl; external LIB_NAME name _PU + 'Collision_b2CollidePolygonAndCircle'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2CollidePolygons(manifold: Pb2Manifold; polygonA: b2PolygonShapeHandle; const [ref] xfA: b2Transform; polygonB: b2PolygonShapeHandle; const [ref] xfB: b2Transform); cdecl; external LIB_NAME name _PU + 'Collision_b2CollidePolygons'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2CollideEdgeAndCircle(manifold: Pb2Manifold; polygonA: b2EdgeShapeHandle; const [ref] xfA: b2Transform; circleB: b2CircleShapeHandle; const [ref] xfB: b2Transform); cdecl; external LIB_NAME name _PU + 'Collision_b2CollideEdgeAndCircle'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2CollideEdgeAndPolygon(manifold: Pb2Manifold; edgeA: b2EdgeShapeHandle; const [ref] xfA: b2Transform; circleB: b2PolygonShapeHandle; const [ref] xfB: b2Transform); cdecl; external LIB_NAME name _PU + 'Collision_b2CollideEdgeAndPolygon'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2ClipSegmentToLine(vOut: Pb2ClipVertex; vIn: Pb2ClipVertex; const [ref] normal: b2Vec2; offset: Single; vertexIndexA: Integer): Integer; cdecl; external LIB_NAME name _PU + 'Collision_b2ClipSegmentToLine'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2TestOverlap(shapeA: b2ShapeHandle; indexA: Integer; shapeB: b2ShapeHandle; indexB: Integer; const [ref] xfA: b2Transform; const [ref] xfB: b2Transform): Boolean; overload; cdecl; external LIB_NAME name _PU + 'Collision_b2TestOverlap'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2TestOverlap(const [ref] a: b2AABB; const [ref] b: b2AABB): Boolean; overload; cdecl; external LIB_NAME name _PU + 'Collision_b2TestOverlap2'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2PairLessThan(const [ref] pair1: b2Pair; const [ref] pair2: b2Pair): Boolean; cdecl; external LIB_NAME name _PU + 'Collision_b2PairLessThan'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Distance(output: Pb2DistanceOutput; cache: Pb2SimplexCache; input: Pb2DistanceInput); cdecl; external LIB_NAME name _PU + 'Collision_b2Distance'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2TimeOfImpact(output: Pb2TOIOutput; input: Pb2TOIInput); cdecl; external LIB_NAME name _PU + 'Collision_b2TimeOfImpact'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};


end.
