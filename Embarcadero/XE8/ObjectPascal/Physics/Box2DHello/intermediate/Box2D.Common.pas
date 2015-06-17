
//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit Box2D.Common;

interface

uses
Box2DTypes;

const
b2_chunkSize: Integer = 16 * 1024;
b2_maxBlockSize: Integer = 640;
b2_blockSizes: Integer = 14;
b2_chunkArrayIncrement: Integer = 128;
b2_stackSize: Integer = 100 * 1024;
b2_maxStackEntries: Integer = 32;


type

b2BlockAllocatorHandle = THandle;
Pb2BlockAllocatorHandle = ^b2BlockAllocatorHandle;

b2DrawHandle = THandle;
Pb2DrawHandle = ^b2DrawHandle;

b2StackAllocatorHandle = THandle;
Pb2StackAllocatorHandle = ^b2StackAllocatorHandle;


Pb2Version = ^b2Version;
PPb2Version = ^Pb2Version;

Pb2Vec2 = ^b2Vec2;
PPb2Vec2 = ^Pb2Vec2;

Pb2Vec3 = ^b2Vec3;
PPb2Vec3 = ^Pb2Vec3;

Pb2Mat22 = ^b2Mat22;
PPb2Mat22 = ^Pb2Mat22;

Pb2Mat33 = ^b2Mat33;
PPb2Mat33 = ^Pb2Mat33;

Pb2Rot = ^b2Rot;
PPb2Rot = ^Pb2Rot;

Pb2Transform = ^b2Transform;
PPb2Transform = ^Pb2Transform;

Pb2Sweep = ^b2Sweep;
PPb2Sweep = ^Pb2Sweep;

Pb2Color = ^b2Color;
PPb2Color = ^Pb2Color;

Pb2StackEntry = ^b2StackEntry;
PPb2StackEntry = ^Pb2StackEntry;

Pb2Timer = ^b2Timer;
PPb2Timer = ^Pb2Timer;


{ ===== Records ===== }

b2Version = record
major: Integer;
minor: Integer;
revision: Integer;

class function Create: b2Version; static; cdecl;
end;

b2BlockAllocatorWrapper = record
FHandle: b2BlockAllocatorHandle;

class function Create: b2BlockAllocatorWrapper; static; cdecl;
procedure Destroy; cdecl;

class operator Implicit(handle: b2BlockAllocatorHandle): b2BlockAllocatorWrapper; overload;
class operator Implicit(wrapper: b2BlockAllocatorWrapper): b2BlockAllocatorHandle; overload;
function Allocate(size: Integer): Pointer; cdecl;
procedure Free(p: Pointer; size: Integer); cdecl;
procedure Clear; cdecl;
end;

b2Vec2 = record
x: Single;
y: Single;

class function Create: b2Vec2; overload; static; cdecl;
class function Create(x: Single; y: Single): b2Vec2; overload; static; cdecl;

 
class operator Negative(const a: b2Vec2): b2Vec2; 
class operator Add(const lhs: b2Vec2; const rhs: b2Vec2): b2Vec2; 
class operator Subtract(const lhs: b2Vec2; const rhs: b2Vec2): b2Vec2; 
class operator Multiply(const lhs: Single; const rhs: b2Vec2): b2Vec2; overload; 
class operator Multiply(const lhs: b2Vec2; const rhs: Single): b2Vec2; overload;

procedure SetZero; cdecl;
procedure &Set(x_: Single; y_: Single); cdecl;
function Length: Single; cdecl;
function LengthSquared: Single; cdecl;
function Normalize: Single; cdecl;
function IsValid: Boolean; cdecl;
function Skew: b2Vec2; cdecl;
end;

b2Vec3 = record
x: Single;
y: Single;
z: Single;

class function Create: b2Vec3; overload; static; cdecl;
class function Create(x: Single; y: Single; z: Single): b2Vec3; overload; static; cdecl;

procedure SetZero; cdecl;
procedure &Set(x_: Single; y_: Single; z_: Single); cdecl;
end;

b2Mat22 = record
ex: b2Vec2;
ey: b2Vec2;

class function Create: b2Mat22; overload; static; cdecl;
class function Create(const [ref] c1: b2Vec2; const [ref] c2: b2Vec2): b2Mat22; overload; static; cdecl;
class function Create(a11: Single; a12: Single; a21: Single; a22: Single): b2Mat22; overload; static; cdecl;

procedure &Set(const [ref] c1: b2Vec2; const [ref] c2: b2Vec2); cdecl;
procedure SetIdentity; cdecl;
procedure SetZero; cdecl;
function GetInverse: b2Mat22; cdecl;
function Solve(const [ref] b: b2Vec2): b2Vec2; cdecl;
end;

b2Mat33 = record
ex: b2Vec3;
ey: b2Vec3;
ez: b2Vec3;

class function Create: b2Mat33; overload; static; cdecl;
class function Create(const [ref] c1: b2Vec3; const [ref] c2: b2Vec3; const [ref] c3: b2Vec3): b2Mat33; overload; static; cdecl;

procedure SetZero; cdecl;
function Solve33(const [ref] b: b2Vec3): b2Vec3; cdecl;
function Solve22(const [ref] b: b2Vec2): b2Vec2; cdecl;
procedure GetInverse22(M: Pb2Mat33); cdecl;
procedure GetSymInverse33(M: Pb2Mat33); cdecl;
end;

b2Rot = record
s: Single;
c: Single;

class function Create: b2Rot; overload; static; cdecl;
class function Create(angle: Single): b2Rot; overload; static; cdecl;

procedure &Set(angle: Single); cdecl;
procedure SetIdentity; cdecl;
function GetAngle: Single; cdecl;
function GetXAxis: b2Vec2; cdecl;
function GetYAxis: b2Vec2; cdecl;
end;

b2Transform = record
p: b2Vec2;
q: b2Rot;

class function Create: b2Transform; overload; static; cdecl;
class function Create(const [ref] position: b2Vec2; const [ref] rotation: b2Rot): b2Transform; overload; static; cdecl;

procedure SetIdentity; cdecl;
procedure &Set(const [ref] position: b2Vec2; angle: Single); cdecl;
end;

b2Sweep = record
localCenter: b2Vec2;
c0: b2Vec2;
c: b2Vec2;
a0: Single;
a: Single;
alpha0: Single;

class function Create: b2Sweep; static; cdecl;

procedure GetTransform(xfb: Pb2Transform; beta: Single); cdecl;
procedure Advance(alpha: Single); cdecl;
procedure Normalize; cdecl;
end;

b2Color = record
r: Single;
g: Single;
b: Single;
a: Single;

class function Create: b2Color; overload; static; cdecl;
class function Create(r: Single; g: Single; b: Single; a: Single): b2Color; overload; static; cdecl;

procedure &Set(ri: Single; gi: Single; bi: Single; ai: Single); cdecl;
end;

b2DrawWrapper = record
type 
{$MinEnumSize 4}
enum_anon = (e_shapeBit = 1, e_jointBit = 2, e_aabbBit = 4, e_pairBit = 8, e_centerOfMassBit = 16);
{$MinEnumSize 1}
Penum_anon = ^enum_anon;
var 
FHandle: b2DrawHandle;

class operator Implicit(handle: b2DrawHandle): b2DrawWrapper; overload;
class operator Implicit(wrapper: b2DrawWrapper): b2DrawHandle; overload;
procedure SetFlags(flags: LongWord); cdecl;
function GetFlags: LongWord; cdecl;
procedure AppendFlags(flags: LongWord); cdecl;
procedure ClearFlags(flags: LongWord); cdecl;
procedure DrawPolygon(vertices: Pb2Vec2; vertexCount: Integer; const [ref] color: b2Color); cdecl;
procedure DrawSolidPolygon(vertices: Pb2Vec2; vertexCount: Integer; const [ref] color: b2Color); cdecl;
procedure DrawCircle(const [ref] center: b2Vec2; radius: Single; const [ref] color: b2Color); cdecl;
procedure DrawSolidCircle(const [ref] center: b2Vec2; radius: Single; const [ref] axis: b2Vec2; const [ref] color: b2Color); cdecl;
procedure DrawSegment(const [ref] p1: b2Vec2; const [ref] p2: b2Vec2; const [ref] color: b2Color); cdecl;
procedure DrawTransform(const [ref] xf: b2Transform); cdecl;
end;

b2StackEntry = record
data: tdCharPtr;
size: Integer;
usedMalloc: Boolean;

class function Create: b2StackEntry; static; cdecl;
end;

b2StackAllocatorWrapper = record
FHandle: b2StackAllocatorHandle;

class function Create: b2StackAllocatorWrapper; static; cdecl;
procedure Destroy; cdecl;

class operator Implicit(handle: b2StackAllocatorHandle): b2StackAllocatorWrapper; overload;
class operator Implicit(wrapper: b2StackAllocatorWrapper): b2StackAllocatorHandle; overload;
function Allocate(size: Integer): Pointer; cdecl;
procedure Free(p: Pointer); cdecl;
function GetMaxAllocation: Integer; cdecl;
end;

b2Timer = record
private 
{$IFDEF MSWINDOWS} 
  m_start: Double; 
{$ELSE} 
  m_start_sec: LongWord; 
  m_start_usec: LongWord; 
{$ENDIF} 
    

public 
class function Create: b2Timer; static; cdecl;

procedure Reset; cdecl;
function GetMilliseconds: Single; cdecl;
end;

{ ===== Delegate interfaces ===== }

Ib2Draw = interface
['{251796EC-5843-2641-5E13-EF05F9D46768}']
procedure DrawPolygon(vertices: Pb2Vec2; vertexCount: Integer; const [ref] color: b2Color);  cdecl;
procedure DrawSolidPolygon(vertices: Pb2Vec2; vertexCount: Integer; const [ref] color: b2Color);  cdecl;
procedure DrawCircle(const [ref] center: b2Vec2; radius: Single; const [ref] color: b2Color);  cdecl;
procedure DrawSolidCircle(const [ref] center: b2Vec2; radius: Single; const [ref] axis: b2Vec2; const [ref] color: b2Color);  cdecl;
procedure DrawSegment(const [ref] p1: b2Vec2; const [ref] p2: b2Vec2; const [ref] color: b2Color);  cdecl;
procedure DrawTransform(const [ref] xf: b2Transform);  cdecl;
end;

{ ===== CreateDestroy of Delegate interfaces ===== }

function Create_b2Draw_delegate(Intf: Ib2Draw): b2DrawHandle; cdecl;
procedure Destroy_b2Draw_delegate(handle: b2DrawHandle); cdecl;


{ Accessors for the global variable 'b2_version' }
function Get_b2_version: b2Version; cdecl;
procedure Set_b2_version(newValue: b2Version); cdecl;

{ Accessors for the global variable 'b2Vec2_zero' }
function Get_b2Vec2_zero: b2Vec2; cdecl;

function b2Alloc(size: Integer): Pointer; cdecl;
procedure b2Free(mem: Pointer); cdecl;
function b2IsValid(x: Single): Boolean; cdecl;
function b2InvSqrt(x: Single): Single; cdecl;
function b2Dot(const [ref] a: b2Vec2; const [ref] b: b2Vec2): Single; overload; cdecl;
function b2Cross(const [ref] a: b2Vec2; const [ref] b: b2Vec2): Single; overload; cdecl;
function b2Cross(const [ref] a: b2Vec2; s: Single): b2Vec2; overload; cdecl;
function b2Cross(s: Single; const [ref] a: b2Vec2): b2Vec2; overload; cdecl;
function b2Mul(const [ref] A: b2Mat22; const [ref] v: b2Vec2): b2Vec2; overload; cdecl;
function b2MulT(const [ref] A: b2Mat22; const [ref] v: b2Vec2): b2Vec2; overload; cdecl;
function b2Distance(const [ref] a: b2Vec2; const [ref] b: b2Vec2): Single; cdecl;
function b2DistanceSquared(const [ref] a: b2Vec2; const [ref] b: b2Vec2): Single; cdecl;
function b2Dot(const [ref] a: b2Vec3; const [ref] b: b2Vec3): Single; overload; cdecl;
function b2Cross(const [ref] a: b2Vec3; const [ref] b: b2Vec3): b2Vec3; overload; cdecl;
function b2Mul(const [ref] A: b2Mat22; const [ref] B: b2Mat22): b2Mat22; overload; cdecl;
function b2MulT(const [ref] A: b2Mat22; const [ref] B: b2Mat22): b2Mat22; overload; cdecl;
function b2Mul(const [ref] A: b2Mat33; const [ref] v: b2Vec3): b2Vec3; overload; cdecl;
function b2Mul22(const [ref] A: b2Mat33; const [ref] v: b2Vec2): b2Vec2; cdecl;
function b2Mul(const [ref] q: b2Rot; const [ref] r: b2Rot): b2Rot; overload; cdecl;
function b2MulT(const [ref] q: b2Rot; const [ref] r: b2Rot): b2Rot; overload; cdecl;
function b2Mul(const [ref] q: b2Rot; const [ref] v: b2Vec2): b2Vec2; overload; cdecl;
function b2MulT(const [ref] q: b2Rot; const [ref] v: b2Vec2): b2Vec2; overload; cdecl;
function b2Mul(const [ref] T: b2Transform; const [ref] v: b2Vec2): b2Vec2; overload; cdecl;
function b2MulT(const [ref] T: b2Transform; const [ref] v: b2Vec2): b2Vec2; overload; cdecl;
function b2Mul(const [ref] A: b2Transform; const [ref] B: b2Transform): b2Transform; overload; cdecl;
function b2MulT(const [ref] A: b2Transform; const [ref] B: b2Transform): b2Transform; overload; cdecl;
function b2Abs(const [ref] a: b2Vec2): b2Vec2; overload; cdecl;
function b2Abs(const [ref] A: b2Mat22): b2Mat22; overload; cdecl;
function b2Min(const [ref] a: b2Vec2; const [ref] b: b2Vec2): b2Vec2; cdecl;
function b2Max(const [ref] a: b2Vec2; const [ref] b: b2Vec2): b2Vec2; cdecl;
function b2Clamp(const [ref] a: b2Vec2; const [ref] low: b2Vec2; const [ref] high: b2Vec2): b2Vec2; cdecl;
function b2NextPowerOfTwo(x: LongWord): LongWord; cdecl;
function b2IsPowerOfTwo(x: LongWord): Boolean; cdecl;

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

function Create_b2Draw_delegate; cdecl; external LIB_NAME name _PU + 'Create_b2Draw_delegate'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};


procedure Destroy_b2Draw_delegate; cdecl; external LIB_NAME name _PU + 'Destroy_b2Draw_delegate'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};



{ ===== Record methods: import and definition ===== }

function b2Version_Create: b2Version; cdecl; external LIB_NAME name _PU + 'b2Version_b2Version'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2Version.Create: b2Version; cdecl;
begin
  Result := b2Version_Create;
end;


function b2BlockAllocator_Create: b2BlockAllocatorHandle; cdecl; external LIB_NAME name _PU + 'b2BlockAllocator_b2BlockAllocator_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2BlockAllocatorWrapper.Create: b2BlockAllocatorWrapper; cdecl;
begin
  Result.FHandle := b2BlockAllocator_Create;
end;

procedure b2BlockAllocator_Destroy(_self: b2BlockAllocatorHandle); cdecl; external LIB_NAME name _PU + 'b2BlockAllocator_dtor'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2BlockAllocatorWrapper.Destroy; cdecl;
begin
  b2BlockAllocator_Destroy(FHandle);
end;

class operator b2BlockAllocatorWrapper.Implicit(handle: b2BlockAllocatorHandle): b2BlockAllocatorWrapper;
begin
  Result.FHandle := handle;
end;
class operator b2BlockAllocatorWrapper.Implicit(wrapper: b2BlockAllocatorWrapper): b2BlockAllocatorHandle;
begin
  Result := wrapper.FHandle;
end;

function b2BlockAllocator_Allocate(_self: b2BlockAllocatorHandle; size: Integer): Pointer; cdecl; external LIB_NAME name _PU + 'b2BlockAllocator_Allocate'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2BlockAllocatorWrapper.Allocate(size: Integer): Pointer; cdecl;
begin
  Result := b2BlockAllocator_Allocate(FHandle, size)
end;

procedure b2BlockAllocator_Free(_self: b2BlockAllocatorHandle; p: Pointer; size: Integer); cdecl; external LIB_NAME name _PU + 'b2BlockAllocator_Free'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2BlockAllocatorWrapper.Free(p: Pointer; size: Integer); cdecl;
begin
  b2BlockAllocator_Free(FHandle, p, size)
end;

procedure b2BlockAllocator_Clear(_self: b2BlockAllocatorHandle); cdecl; external LIB_NAME name _PU + 'b2BlockAllocator_Clear'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2BlockAllocatorWrapper.Clear; cdecl;
begin
  b2BlockAllocator_Clear(FHandle)
end;


function b2Vec2_Create: b2Vec2; overload; cdecl; external LIB_NAME name _PU + 'b2Vec2_b2Vec2_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2Vec2.Create: b2Vec2; cdecl;
begin
  Result := b2Vec2_Create;
end;

function b2Vec2_Create(x: Single; y: Single): b2Vec2; overload; cdecl; external LIB_NAME name _PU + 'b2Vec2_b2Vec2_2'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2Vec2.Create(x: Single; y: Single): b2Vec2; cdecl;
begin
  Result := b2Vec2_Create(x, y);
end;


 
class operator b2Vec2.Negative(const a: b2Vec2): b2Vec2;  
begin  
  Result.x := -a.x;  
  Result.y := -a.y;  
end; 
     
class operator b2Vec2.Add(const lhs, rhs: b2Vec2): b2Vec2; 
begin 
  Result.x := lhs.x + rhs.x; 
  Result.y := lhs.y + rhs.y; 
end; 
 
class operator b2Vec2.Multiply(const lhs: Single; const rhs: b2Vec2): b2Vec2; 
begin 
  Result.x := lhs * rhs.x; 
  Result.y := lhs * rhs.y; 
end; 
 
class operator b2Vec2.Multiply(const lhs: b2Vec2; const rhs: Single): b2Vec2; 
begin 
  Result.x := lhs.x * rhs; 
  Result.y := lhs.y * rhs; 
end; 
 
class operator b2Vec2.Subtract(const lhs, rhs: b2Vec2): b2Vec2; 
begin 
  Result.x := lhs.x - rhs.x; 
  Result.y := lhs.y - rhs.y; 
end; 
    
procedure b2Vec2_SetZero(_self: Pb2Vec2); cdecl; external LIB_NAME name _PU + 'b2Vec2_SetZero'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Vec2.SetZero; cdecl;
begin
  b2Vec2_SetZero(@Self)
end;

procedure b2Vec2__Set(_self: Pb2Vec2; x_: Single; y_: Single); cdecl; external LIB_NAME name _PU + 'b2Vec2_Set'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Vec2.&Set(x_: Single; y_: Single); cdecl;
begin
  b2Vec2__Set(@Self, x_, y_)
end;

function b2Vec2_Length(_self: Pb2Vec2): Single; cdecl; external LIB_NAME name _PU + 'b2Vec2_Length'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Vec2.Length: Single; cdecl;
begin
  Result := b2Vec2_Length(@Self)
end;

function b2Vec2_LengthSquared(_self: Pb2Vec2): Single; cdecl; external LIB_NAME name _PU + 'b2Vec2_LengthSquared'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Vec2.LengthSquared: Single; cdecl;
begin
  Result := b2Vec2_LengthSquared(@Self)
end;

function b2Vec2_Normalize(_self: Pb2Vec2): Single; cdecl; external LIB_NAME name _PU + 'b2Vec2_Normalize'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Vec2.Normalize: Single; cdecl;
begin
  Result := b2Vec2_Normalize(@Self)
end;

function b2Vec2_IsValid(_self: Pb2Vec2): Boolean; cdecl; external LIB_NAME name _PU + 'b2Vec2_IsValid'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Vec2.IsValid: Boolean; cdecl;
begin
  Result := b2Vec2_IsValid(@Self)
end;

function b2Vec2_Skew(_self: Pb2Vec2): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2Vec2_Skew'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Vec2.Skew: b2Vec2; cdecl;
begin
  Result := b2Vec2_Skew(@Self)
end;


function b2Vec3_Create: b2Vec3; overload; cdecl; external LIB_NAME name _PU + 'b2Vec3_b2Vec3_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2Vec3.Create: b2Vec3; cdecl;
begin
  Result := b2Vec3_Create;
end;

function b2Vec3_Create(x: Single; y: Single; z: Single): b2Vec3; overload; cdecl; external LIB_NAME name _PU + 'b2Vec3_b2Vec3_2'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2Vec3.Create(x: Single; y: Single; z: Single): b2Vec3; cdecl;
begin
  Result := b2Vec3_Create(x, y, z);
end;

procedure b2Vec3_SetZero(_self: Pb2Vec3); cdecl; external LIB_NAME name _PU + 'b2Vec3_SetZero'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Vec3.SetZero; cdecl;
begin
  b2Vec3_SetZero(@Self)
end;

procedure b2Vec3__Set(_self: Pb2Vec3; x_: Single; y_: Single; z_: Single); cdecl; external LIB_NAME name _PU + 'b2Vec3_Set'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Vec3.&Set(x_: Single; y_: Single; z_: Single); cdecl;
begin
  b2Vec3__Set(@Self, x_, y_, z_)
end;


function b2Mat22_Create: b2Mat22; overload; cdecl; external LIB_NAME name _PU + 'b2Mat22_b2Mat22_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2Mat22.Create: b2Mat22; cdecl;
begin
  Result := b2Mat22_Create;
end;

function b2Mat22_Create(const [ref] c1: b2Vec2; const [ref] c2: b2Vec2): b2Mat22; overload; cdecl; external LIB_NAME name _PU + 'b2Mat22_b2Mat22_2'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2Mat22.Create(const [ref] c1: b2Vec2; const [ref] c2: b2Vec2): b2Mat22; cdecl;
begin
  Result := b2Mat22_Create(c1, c2);
end;

function b2Mat22_Create(a11: Single; a12: Single; a21: Single; a22: Single): b2Mat22; overload; cdecl; external LIB_NAME name _PU + 'b2Mat22_b2Mat22_3'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2Mat22.Create(a11: Single; a12: Single; a21: Single; a22: Single): b2Mat22; cdecl;
begin
  Result := b2Mat22_Create(a11, a12, a21, a22);
end;

procedure b2Mat22__Set(_self: Pb2Mat22; const [ref] c1: b2Vec2; const [ref] c2: b2Vec2); cdecl; external LIB_NAME name _PU + 'b2Mat22_Set'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Mat22.&Set(const [ref] c1: b2Vec2; const [ref] c2: b2Vec2); cdecl;
begin
  b2Mat22__Set(@Self, c1, c2)
end;

procedure b2Mat22_SetIdentity(_self: Pb2Mat22); cdecl; external LIB_NAME name _PU + 'b2Mat22_SetIdentity'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Mat22.SetIdentity; cdecl;
begin
  b2Mat22_SetIdentity(@Self)
end;

procedure b2Mat22_SetZero(_self: Pb2Mat22); cdecl; external LIB_NAME name _PU + 'b2Mat22_SetZero'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Mat22.SetZero; cdecl;
begin
  b2Mat22_SetZero(@Self)
end;

function b2Mat22_GetInverse(_self: Pb2Mat22): b2Mat22; cdecl; external LIB_NAME name _PU + 'b2Mat22_GetInverse'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Mat22.GetInverse: b2Mat22; cdecl;
begin
  Result := b2Mat22_GetInverse(@Self)
end;

function b2Mat22_Solve(_self: Pb2Mat22; const [ref] b: b2Vec2): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2Mat22_Solve'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Mat22.Solve(const [ref] b: b2Vec2): b2Vec2; cdecl;
begin
  Result := b2Mat22_Solve(@Self, b)
end;


function b2Mat33_Create: b2Mat33; overload; cdecl; external LIB_NAME name _PU + 'b2Mat33_b2Mat33_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2Mat33.Create: b2Mat33; cdecl;
begin
  Result := b2Mat33_Create;
end;

function b2Mat33_Create(const [ref] c1: b2Vec3; const [ref] c2: b2Vec3; const [ref] c3: b2Vec3): b2Mat33; overload; cdecl; external LIB_NAME name _PU + 'b2Mat33_b2Mat33_2'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2Mat33.Create(const [ref] c1: b2Vec3; const [ref] c2: b2Vec3; const [ref] c3: b2Vec3): b2Mat33; cdecl;
begin
  Result := b2Mat33_Create(c1, c2, c3);
end;

procedure b2Mat33_SetZero(_self: Pb2Mat33); cdecl; external LIB_NAME name _PU + 'b2Mat33_SetZero'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Mat33.SetZero; cdecl;
begin
  b2Mat33_SetZero(@Self)
end;

function b2Mat33_Solve33(_self: Pb2Mat33; const [ref] b: b2Vec3): b2Vec3; cdecl; external LIB_NAME name _PU + 'b2Mat33_Solve33'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Mat33.Solve33(const [ref] b: b2Vec3): b2Vec3; cdecl;
begin
  Result := b2Mat33_Solve33(@Self, b)
end;

function b2Mat33_Solve22(_self: Pb2Mat33; const [ref] b: b2Vec2): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2Mat33_Solve22'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Mat33.Solve22(const [ref] b: b2Vec2): b2Vec2; cdecl;
begin
  Result := b2Mat33_Solve22(@Self, b)
end;

procedure b2Mat33_GetInverse22(_self: Pb2Mat33; M: Pb2Mat33); cdecl; external LIB_NAME name _PU + 'b2Mat33_GetInverse22'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Mat33.GetInverse22(M: Pb2Mat33); cdecl;
begin
  b2Mat33_GetInverse22(@Self, M)
end;

procedure b2Mat33_GetSymInverse33(_self: Pb2Mat33; M: Pb2Mat33); cdecl; external LIB_NAME name _PU + 'b2Mat33_GetSymInverse33'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Mat33.GetSymInverse33(M: Pb2Mat33); cdecl;
begin
  b2Mat33_GetSymInverse33(@Self, M)
end;


function b2Rot_Create: b2Rot; overload; cdecl; external LIB_NAME name _PU + 'b2Rot_b2Rot_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2Rot.Create: b2Rot; cdecl;
begin
  Result := b2Rot_Create;
end;

function b2Rot_Create(angle: Single): b2Rot; overload; cdecl; external LIB_NAME name _PU + 'b2Rot_b2Rot_2'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2Rot.Create(angle: Single): b2Rot; cdecl;
begin
  Result := b2Rot_Create(angle);
end;

procedure b2Rot__Set(_self: Pb2Rot; angle: Single); cdecl; external LIB_NAME name _PU + 'b2Rot_Set'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Rot.&Set(angle: Single); cdecl;
begin
  b2Rot__Set(@Self, angle)
end;

procedure b2Rot_SetIdentity(_self: Pb2Rot); cdecl; external LIB_NAME name _PU + 'b2Rot_SetIdentity'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Rot.SetIdentity; cdecl;
begin
  b2Rot_SetIdentity(@Self)
end;

function b2Rot_GetAngle(_self: Pb2Rot): Single; cdecl; external LIB_NAME name _PU + 'b2Rot_GetAngle'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Rot.GetAngle: Single; cdecl;
begin
  Result := b2Rot_GetAngle(@Self)
end;

function b2Rot_GetXAxis(_self: Pb2Rot): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2Rot_GetXAxis'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Rot.GetXAxis: b2Vec2; cdecl;
begin
  Result := b2Rot_GetXAxis(@Self)
end;

function b2Rot_GetYAxis(_self: Pb2Rot): b2Vec2; cdecl; external LIB_NAME name _PU + 'b2Rot_GetYAxis'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Rot.GetYAxis: b2Vec2; cdecl;
begin
  Result := b2Rot_GetYAxis(@Self)
end;


function b2Transform_Create: b2Transform; overload; cdecl; external LIB_NAME name _PU + 'b2Transform_b2Transform_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2Transform.Create: b2Transform; cdecl;
begin
  Result := b2Transform_Create;
end;

function b2Transform_Create(const [ref] position: b2Vec2; const [ref] rotation: b2Rot): b2Transform; overload; cdecl; external LIB_NAME name _PU + 'b2Transform_b2Transform_2'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2Transform.Create(const [ref] position: b2Vec2; const [ref] rotation: b2Rot): b2Transform; cdecl;
begin
  Result := b2Transform_Create(position, rotation);
end;

procedure b2Transform_SetIdentity(_self: Pb2Transform); cdecl; external LIB_NAME name _PU + 'b2Transform_SetIdentity'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Transform.SetIdentity; cdecl;
begin
  b2Transform_SetIdentity(@Self)
end;

procedure b2Transform__Set(_self: Pb2Transform; const [ref] position: b2Vec2; angle: Single); cdecl; external LIB_NAME name _PU + 'b2Transform_Set'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Transform.&Set(const [ref] position: b2Vec2; angle: Single); cdecl;
begin
  b2Transform__Set(@Self, position, angle)
end;


function b2Sweep_Create: b2Sweep; cdecl; external LIB_NAME name _PU + 'b2Sweep_b2Sweep'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2Sweep.Create: b2Sweep; cdecl;
begin
  Result := b2Sweep_Create;
end;

procedure b2Sweep_GetTransform(_self: Pb2Sweep; xfb: Pb2Transform; beta: Single); cdecl; external LIB_NAME name _PU + 'b2Sweep_GetTransform'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Sweep.GetTransform(xfb: Pb2Transform; beta: Single); cdecl;
begin
  b2Sweep_GetTransform(@Self, xfb, beta)
end;

procedure b2Sweep_Advance(_self: Pb2Sweep; alpha: Single); cdecl; external LIB_NAME name _PU + 'b2Sweep_Advance'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Sweep.Advance(alpha: Single); cdecl;
begin
  b2Sweep_Advance(@Self, alpha)
end;

procedure b2Sweep_Normalize(_self: Pb2Sweep); cdecl; external LIB_NAME name _PU + 'b2Sweep_Normalize'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Sweep.Normalize; cdecl;
begin
  b2Sweep_Normalize(@Self)
end;


function b2Color_Create: b2Color; overload; cdecl; external LIB_NAME name _PU + 'b2Color_b2Color_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2Color.Create: b2Color; cdecl;
begin
  Result := b2Color_Create;
end;

function b2Color_Create(r: Single; g: Single; b: Single; a: Single): b2Color; overload; cdecl; external LIB_NAME name _PU + 'b2Color_b2Color_2'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2Color.Create(r: Single; g: Single; b: Single; a: Single): b2Color; cdecl;
begin
  Result := b2Color_Create(r, g, b, a);
end;

procedure b2Color__Set(_self: Pb2Color; ri: Single; gi: Single; bi: Single; ai: Single); cdecl; external LIB_NAME name _PU + 'b2Color_Set'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Color.&Set(ri: Single; gi: Single; bi: Single; ai: Single); cdecl;
begin
  b2Color__Set(@Self, ri, gi, bi, ai)
end;


class operator b2DrawWrapper.Implicit(handle: b2DrawHandle): b2DrawWrapper;
begin
  Result.FHandle := handle;
end;
class operator b2DrawWrapper.Implicit(wrapper: b2DrawWrapper): b2DrawHandle;
begin
  Result := wrapper.FHandle;
end;

procedure b2Draw_SetFlags(_self: b2DrawHandle; flags: LongWord); cdecl; external LIB_NAME name _PU + 'b2Draw_SetFlags'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2DrawWrapper.SetFlags(flags: LongWord); cdecl;
begin
  b2Draw_SetFlags(FHandle, flags)
end;

function b2Draw_GetFlags(_self: b2DrawHandle): LongWord; cdecl; external LIB_NAME name _PU + 'b2Draw_GetFlags'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2DrawWrapper.GetFlags: LongWord; cdecl;
begin
  Result := b2Draw_GetFlags(FHandle)
end;

procedure b2Draw_AppendFlags(_self: b2DrawHandle; flags: LongWord); cdecl; external LIB_NAME name _PU + 'b2Draw_AppendFlags'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2DrawWrapper.AppendFlags(flags: LongWord); cdecl;
begin
  b2Draw_AppendFlags(FHandle, flags)
end;

procedure b2Draw_ClearFlags(_self: b2DrawHandle; flags: LongWord); cdecl; external LIB_NAME name _PU + 'b2Draw_ClearFlags'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2DrawWrapper.ClearFlags(flags: LongWord); cdecl;
begin
  b2Draw_ClearFlags(FHandle, flags)
end;

procedure b2Draw_DrawPolygon(_self: b2DrawHandle; vertices: Pb2Vec2; vertexCount: Integer; const [ref] color: b2Color); cdecl; external LIB_NAME name _PU + 'b2Draw_DrawPolygon'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2DrawWrapper.DrawPolygon(vertices: Pb2Vec2; vertexCount: Integer; const [ref] color: b2Color); cdecl;
begin
  b2Draw_DrawPolygon(FHandle, vertices, vertexCount, color)
end;

procedure b2Draw_DrawSolidPolygon(_self: b2DrawHandle; vertices: Pb2Vec2; vertexCount: Integer; const [ref] color: b2Color); cdecl; external LIB_NAME name _PU + 'b2Draw_DrawSolidPolygon'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2DrawWrapper.DrawSolidPolygon(vertices: Pb2Vec2; vertexCount: Integer; const [ref] color: b2Color); cdecl;
begin
  b2Draw_DrawSolidPolygon(FHandle, vertices, vertexCount, color)
end;

procedure b2Draw_DrawCircle(_self: b2DrawHandle; const [ref] center: b2Vec2; radius: Single; const [ref] color: b2Color); cdecl; external LIB_NAME name _PU + 'b2Draw_DrawCircle'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2DrawWrapper.DrawCircle(const [ref] center: b2Vec2; radius: Single; const [ref] color: b2Color); cdecl;
begin
  b2Draw_DrawCircle(FHandle, center, radius, color)
end;

procedure b2Draw_DrawSolidCircle(_self: b2DrawHandle; const [ref] center: b2Vec2; radius: Single; const [ref] axis: b2Vec2; const [ref] color: b2Color); cdecl; external LIB_NAME name _PU + 'b2Draw_DrawSolidCircle'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2DrawWrapper.DrawSolidCircle(const [ref] center: b2Vec2; radius: Single; const [ref] axis: b2Vec2; const [ref] color: b2Color); cdecl;
begin
  b2Draw_DrawSolidCircle(FHandle, center, radius, axis, color)
end;

procedure b2Draw_DrawSegment(_self: b2DrawHandle; const [ref] p1: b2Vec2; const [ref] p2: b2Vec2; const [ref] color: b2Color); cdecl; external LIB_NAME name _PU + 'b2Draw_DrawSegment'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2DrawWrapper.DrawSegment(const [ref] p1: b2Vec2; const [ref] p2: b2Vec2; const [ref] color: b2Color); cdecl;
begin
  b2Draw_DrawSegment(FHandle, p1, p2, color)
end;

procedure b2Draw_DrawTransform(_self: b2DrawHandle; const [ref] xf: b2Transform); cdecl; external LIB_NAME name _PU + 'b2Draw_DrawTransform'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2DrawWrapper.DrawTransform(const [ref] xf: b2Transform); cdecl;
begin
  b2Draw_DrawTransform(FHandle, xf)
end;


function b2StackEntry_Create: b2StackEntry; cdecl; external LIB_NAME name _PU + 'b2StackEntry_b2StackEntry'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2StackEntry.Create: b2StackEntry; cdecl;
begin
  Result := b2StackEntry_Create;
end;


function b2StackAllocator_Create: b2StackAllocatorHandle; cdecl; external LIB_NAME name _PU + 'b2StackAllocator_b2StackAllocator_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2StackAllocatorWrapper.Create: b2StackAllocatorWrapper; cdecl;
begin
  Result.FHandle := b2StackAllocator_Create;
end;

procedure b2StackAllocator_Destroy(_self: b2StackAllocatorHandle); cdecl; external LIB_NAME name _PU + 'b2StackAllocator_dtor'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2StackAllocatorWrapper.Destroy; cdecl;
begin
  b2StackAllocator_Destroy(FHandle);
end;

class operator b2StackAllocatorWrapper.Implicit(handle: b2StackAllocatorHandle): b2StackAllocatorWrapper;
begin
  Result.FHandle := handle;
end;
class operator b2StackAllocatorWrapper.Implicit(wrapper: b2StackAllocatorWrapper): b2StackAllocatorHandle;
begin
  Result := wrapper.FHandle;
end;

function b2StackAllocator_Allocate(_self: b2StackAllocatorHandle; size: Integer): Pointer; cdecl; external LIB_NAME name _PU + 'b2StackAllocator_Allocate'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2StackAllocatorWrapper.Allocate(size: Integer): Pointer; cdecl;
begin
  Result := b2StackAllocator_Allocate(FHandle, size)
end;

procedure b2StackAllocator_Free(_self: b2StackAllocatorHandle; p: Pointer); cdecl; external LIB_NAME name _PU + 'b2StackAllocator_Free'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2StackAllocatorWrapper.Free(p: Pointer); cdecl;
begin
  b2StackAllocator_Free(FHandle, p)
end;

function b2StackAllocator_GetMaxAllocation(_self: b2StackAllocatorHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2StackAllocator_GetMaxAllocation'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2StackAllocatorWrapper.GetMaxAllocation: Integer; cdecl;
begin
  Result := b2StackAllocator_GetMaxAllocation(FHandle)
end;


function b2Timer_Create: b2Timer; cdecl; external LIB_NAME name _PU + 'b2Timer_b2Timer_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2Timer.Create: b2Timer; cdecl;
begin
  Result := b2Timer_Create;
end;

procedure b2Timer_Reset(_self: Pb2Timer); cdecl; external LIB_NAME name _PU + 'b2Timer_Reset'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Timer.Reset; cdecl;
begin
  b2Timer_Reset(@Self)
end;

function b2Timer_GetMilliseconds(_self: Pb2Timer): Single; cdecl; external LIB_NAME name _PU + 'b2Timer_GetMilliseconds'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Timer.GetMilliseconds: Single; cdecl;
begin
  Result := b2Timer_GetMilliseconds(@Self)
end;


function Get_b2_version; cdecl; external LIB_NAME name _PU + 'Get_b2_version'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure Set_b2_version; cdecl; external LIB_NAME name _PU + 'Set_b2_version'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function Get_b2Vec2_zero; cdecl; external LIB_NAME name _PU + 'Get_b2Vec2_zero'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};


function b2Alloc(size: Integer): Pointer; cdecl; external LIB_NAME name _PU + 'Common_b2Alloc'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2Free(mem: Pointer); cdecl; external LIB_NAME name _PU + 'Common_b2Free'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2IsValid(x: Single): Boolean; cdecl; external LIB_NAME name _PU + 'Common_b2IsValid'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2InvSqrt(x: Single): Single; cdecl; external LIB_NAME name _PU + 'Common_b2InvSqrt'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Dot(const [ref] a: b2Vec2; const [ref] b: b2Vec2): Single; overload; cdecl; external LIB_NAME name _PU + 'Common_b2Dot'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Cross(const [ref] a: b2Vec2; const [ref] b: b2Vec2): Single; overload; cdecl; external LIB_NAME name _PU + 'Common_b2Cross'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Cross(const [ref] a: b2Vec2; s: Single): b2Vec2; overload; cdecl; external LIB_NAME name _PU + 'Common_b2Cross2'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Cross(s: Single; const [ref] a: b2Vec2): b2Vec2; overload; cdecl; external LIB_NAME name _PU + 'Common_b2Cross3'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Mul(const [ref] A: b2Mat22; const [ref] v: b2Vec2): b2Vec2; overload; cdecl; external LIB_NAME name _PU + 'Common_b2Mul'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2MulT(const [ref] A: b2Mat22; const [ref] v: b2Vec2): b2Vec2; overload; cdecl; external LIB_NAME name _PU + 'Common_b2MulT'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Distance(const [ref] a: b2Vec2; const [ref] b: b2Vec2): Single; cdecl; external LIB_NAME name _PU + 'Common_b2Distance'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2DistanceSquared(const [ref] a: b2Vec2; const [ref] b: b2Vec2): Single; cdecl; external LIB_NAME name _PU + 'Common_b2DistanceSquared'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Dot(const [ref] a: b2Vec3; const [ref] b: b2Vec3): Single; overload; cdecl; external LIB_NAME name _PU + 'Common_b2Dot2'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Cross(const [ref] a: b2Vec3; const [ref] b: b2Vec3): b2Vec3; overload; cdecl; external LIB_NAME name _PU + 'Common_b2Cross4'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Mul(const [ref] A: b2Mat22; const [ref] B: b2Mat22): b2Mat22; overload; cdecl; external LIB_NAME name _PU + 'Common_b2Mul2'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2MulT(const [ref] A: b2Mat22; const [ref] B: b2Mat22): b2Mat22; overload; cdecl; external LIB_NAME name _PU + 'Common_b2MulT2'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Mul(const [ref] A: b2Mat33; const [ref] v: b2Vec3): b2Vec3; overload; cdecl; external LIB_NAME name _PU + 'Common_b2Mul3'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Mul22(const [ref] A: b2Mat33; const [ref] v: b2Vec2): b2Vec2; cdecl; external LIB_NAME name _PU + 'Common_b2Mul22'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Mul(const [ref] q: b2Rot; const [ref] r: b2Rot): b2Rot; overload; cdecl; external LIB_NAME name _PU + 'Common_b2Mul4'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2MulT(const [ref] q: b2Rot; const [ref] r: b2Rot): b2Rot; overload; cdecl; external LIB_NAME name _PU + 'Common_b2MulT3'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Mul(const [ref] q: b2Rot; const [ref] v: b2Vec2): b2Vec2; overload; cdecl; external LIB_NAME name _PU + 'Common_b2Mul5'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2MulT(const [ref] q: b2Rot; const [ref] v: b2Vec2): b2Vec2; overload; cdecl; external LIB_NAME name _PU + 'Common_b2MulT4'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Mul(const [ref] T: b2Transform; const [ref] v: b2Vec2): b2Vec2; overload; cdecl; external LIB_NAME name _PU + 'Common_b2Mul6'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2MulT(const [ref] T: b2Transform; const [ref] v: b2Vec2): b2Vec2; overload; cdecl; external LIB_NAME name _PU + 'Common_b2MulT5'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Mul(const [ref] A: b2Transform; const [ref] B: b2Transform): b2Transform; overload; cdecl; external LIB_NAME name _PU + 'Common_b2Mul7'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2MulT(const [ref] A: b2Transform; const [ref] B: b2Transform): b2Transform; overload; cdecl; external LIB_NAME name _PU + 'Common_b2MulT6'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Abs(const [ref] a: b2Vec2): b2Vec2; overload; cdecl; external LIB_NAME name _PU + 'Common_b2Abs'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Abs(const [ref] A: b2Mat22): b2Mat22; overload; cdecl; external LIB_NAME name _PU + 'Common_b2Abs2'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Min(const [ref] a: b2Vec2; const [ref] b: b2Vec2): b2Vec2; cdecl; external LIB_NAME name _PU + 'Common_b2Min'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Max(const [ref] a: b2Vec2; const [ref] b: b2Vec2): b2Vec2; cdecl; external LIB_NAME name _PU + 'Common_b2Max'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2Clamp(const [ref] a: b2Vec2; const [ref] low: b2Vec2; const [ref] high: b2Vec2): b2Vec2; cdecl; external LIB_NAME name _PU + 'Common_b2Clamp'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2NextPowerOfTwo(x: LongWord): LongWord; cdecl; external LIB_NAME name _PU + 'Common_b2NextPowerOfTwo'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2IsPowerOfTwo(x: LongWord): Boolean; cdecl; external LIB_NAME name _PU + 'Common_b2IsPowerOfTwo'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};


end.
