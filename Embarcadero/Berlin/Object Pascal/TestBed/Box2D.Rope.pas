// ==========================================================================
//
//   Copyright(c) 2012-2014 Embarcadero Technologies, Inc.
//
// ==========================================================================

//
// Delphi-C++ Library Bridge
// Interface for library FlatBox2D
//

unit Box2D.Rope;

interface

uses
Box2D.Common,
Box2DTypes;

type

b2RopeHandle = THandle;
Pb2RopeHandle = ^b2RopeHandle;


Pb2RopeDef = ^b2RopeDef;
PPb2RopeDef = ^Pb2RopeDef;


{ ===== Records ===== }

b2RopeDef = record
vertices: Pb2Vec2;
count: Integer;
masses: PSingle;
gravity: b2Vec2;
damping: Single;
k2: Single;  { Stretching stiffness}
k3: Single;  { Bending stiffness. Values above 0.5 can make the simulation blow up.}

class function Create: b2RopeDef; static; cdecl;
end;

b2RopeWrapper = record
FHandle: b2RopeHandle;

class function Create: b2RopeWrapper; static; cdecl;
procedure Destroy; cdecl;

class operator Implicit(handle: b2RopeHandle): b2RopeWrapper; overload;
class operator Implicit(wrapper: b2RopeWrapper): b2RopeHandle; overload;
procedure Initialize(def: Pb2RopeDef); cdecl;
procedure Step(timeStep: Single; iterations: Integer); cdecl;
function GetVertexCount: Integer; cdecl;
function GetVertices: Pb2Vec2; cdecl;
procedure Draw(draw: b2DrawHandle); cdecl;
procedure SetAngle(angle: Single); cdecl;
end;


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

function b2RopeDef_Create: b2RopeDef; cdecl; external LIB_NAME name _PU + 'b2RopeDef_b2RopeDef_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2RopeDef.Create: b2RopeDef; cdecl;
begin
  Result := b2RopeDef_Create;
end;


function b2Rope_Create: b2RopeHandle; cdecl; external LIB_NAME name _PU + 'b2Rope_b2Rope_1'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

class function b2RopeWrapper.Create: b2RopeWrapper; cdecl;
begin
  Result.FHandle := b2Rope_Create;
end;

procedure b2Rope_Destroy(_self: b2RopeHandle); cdecl; external LIB_NAME name _PU + 'b2Rope_dtor'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2RopeWrapper.Destroy; cdecl;
begin
  b2Rope_Destroy(FHandle);
end;

class operator b2RopeWrapper.Implicit(handle: b2RopeHandle): b2RopeWrapper;
begin
  Result.FHandle := handle;
end;
class operator b2RopeWrapper.Implicit(wrapper: b2RopeWrapper): b2RopeHandle;
begin
  Result := wrapper.FHandle;
end;

procedure b2Rope_Initialize(_self: b2RopeHandle; def: Pb2RopeDef); cdecl; external LIB_NAME name _PU + 'b2Rope_Initialize'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2RopeWrapper.Initialize(def: Pb2RopeDef); cdecl;
begin
  b2Rope_Initialize(FHandle, def)
end;

procedure b2Rope_Step(_self: b2RopeHandle; timeStep: Single; iterations: Integer); cdecl; external LIB_NAME name _PU + 'b2Rope_Step'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2RopeWrapper.Step(timeStep: Single; iterations: Integer); cdecl;
begin
  b2Rope_Step(FHandle, timeStep, iterations)
end;

function b2Rope_GetVertexCount(_self: b2RopeHandle): Integer; cdecl; external LIB_NAME name _PU + 'b2Rope_GetVertexCount'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2RopeWrapper.GetVertexCount: Integer; cdecl;
begin
  Result := b2Rope_GetVertexCount(FHandle)
end;

function b2Rope_GetVertices(_self: b2RopeHandle): Pb2Vec2; cdecl; external LIB_NAME name _PU + 'b2Rope_GetVertices'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

function b2RopeWrapper.GetVertices: Pb2Vec2; cdecl;
begin
  Result := b2Rope_GetVertices(FHandle)
end;

procedure b2Rope_Draw(_self: b2RopeHandle; draw: b2DrawHandle); cdecl; external LIB_NAME name _PU + 'b2Rope_Draw'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2RopeWrapper.Draw(draw: b2DrawHandle); cdecl;
begin
  b2Rope_Draw(FHandle, draw)
end;

procedure b2Rope_SetAngle(_self: b2RopeHandle; angle: Single); cdecl; external LIB_NAME name _PU + 'b2Rope_SetAngle'
  {$IF DEFINED(ANDROID)} dependency 'gnustl_static' {$ELSEIF DEFINED(IOS)} {$ENDIF};

procedure b2RopeWrapper.SetAngle(angle: Single); cdecl;
begin
  b2Rope_SetAngle(FHandle, angle)
end;



initialization

{$IF defined(CPUX64)}
Assert(Sizeof(b2RopeDef) = 48, 'Size mismatch in b2RopeDef');
{$ELSE}
Assert(Sizeof(b2RopeDef) = 32, 'Size mismatch in b2RopeDef');
{$ENDIF}


end.
