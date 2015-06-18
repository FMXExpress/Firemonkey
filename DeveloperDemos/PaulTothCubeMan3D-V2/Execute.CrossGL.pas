unit Execute.CrossGL;

{
   Cross-platform OpenGL implementation for Delphi XE2/XE5

   (c)2014 Execute SARL  <contact@execute.re>

   http://www.execute.re

}

interface

uses
  System.SysUtils,
  System.Math

{$IFDEF Android}
  ,Androidapi.Log
  ,Androidapi.AppGlue
  ,Androidapi.Looper
  ,Androidapi.Egl
  ,Androidapi.Gles
  ,Androidapi.NativeWindow
{$ENDIF}

{$IFDEF MSWINDOWS}
  ,Winapi.Windows
  ,Winapi.Messages
  ,Winapi.OpenGL
{$ENDIF}

{$IFDEF MACOS}
  ,Macapi.Mach
  ,Macapi.CocoaTypes
  ,Execute.MacOS
{$ENDIF};

type
// base class for a single form OpenGL application
  TGLContext = class
  private
    FWidth        : Integer;
    FHeight       : Integer;
    FFocused      : Boolean;
    FFieldOfView  : Single;
    FZNear        : Single;
    FZFar         : Single;
    FTicks        : Cardinal;
    FTime         : Cardinal;
    FFrames       : Cardinal;
    procedure CreateContext;
    procedure DestroyContext;
    procedure Repaint;
    procedure SetSize(AWidth, AHeight: Integer);

  {$IFDEF Android}
  private
    FApp          : Pandroid_app;
    FDisplay      : EGLDisplay;
    FSurface      : EGLSurface;
    FContext      : EGLContext;
    FConfigChanged: Boolean;
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  private
    FHandle       : THandle;
    FDC           : THandle;
    FGL           : THandle;
    class function FromHWnd(hWnd: THandle): TGLContext;
  {$ENDIF}

  {$IFDEF MACOS}
  private
    FApp          : NSApplication;
  {$ENDIF}

  protected
    procedure OnSetup; virtual;
    procedure OnResize; virtual;
    procedure OnEnter; virtual;
    procedure OnLeave; virtual;
    procedure OnPaint; virtual;
  public
    constructor Create(AWidth, AHeight: Integer; AFieldOfView, AZNear, AZFar: Single);
    procedure Execute;
    procedure SwapBuffers;
    property Focused: Boolean read FFocused;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Time: Cardinal read FTime;
  end;

const
 GL_NO_ERROR                    =$0000;

 GL_ZERO                        =$0000;
 GL_ONE                         =$0001;

 GL_POINTS                      =$0000; // Treats each vertex as a single point. Vertex n defines point n. N points are drawn.
 GL_LINES                       =$0001; // Treats each pair of vertexes as an independent line segment. Vertexes 2n - 1 and 2n define line n. N/2 lines are drawn.
 GL_LINE_LOOP                   =$0002; // Draws a connected group of line segments from the first vertex to the last, then back to the first. Vertexes n and n+1 define line n. The last line, however, is defined by vertexes N and 1. N lines are drawn.
 GL_LINE_STRIP                  =$0003; // Draws a connected group of line segments from the first vertex to the last. Vertexes n and n+1 define line n. N - 1 lines are drawn.
 GL_TRIANGLES                   =$0004; // Treats each triplet of vertexes as an independent triangle. Vertexes 3n - 2, 3n-1, and 3n define triangle n. N/3 triangles are drawn.
 GL_TRIANGLE_STRIP              =$0005; // Draws a connected group of triangles. One triangle is defined for each vertex presented after the first two vertexes. For odd n, vertexes n, n+1, and n+2 define triangle n.
                                        // For even n, vertexes n+1, n, and n+2 define triangle n. N  - 2 triangles are drawn.
 GL_TRIANGLE_FAN                =$0006; // Draws a connected group of triangles. One triangle is defined for each vertex presented after the first two vertexes. Vertexes 1, n+1, and n+2 define triangle n. N - 2 triangles are drawn.
 GL_QUADS                       =$0007; // Treats each group of four vertexes as an independent quadrilateral. Vertexes 4n - 3, 4n - 2, 4n - 1, and 4n define quadrilateral n. N/4 quadrilaterals are drawn.
 GL_QUAD_STRIP                  =$0008; // Draws a connected group of quadrilaterals. One quadrilateral is defined for each pair of vertexes presented after the first pair. Vertexes 2n - 1, 2n, 2n+2, and 2n+1 define quadrilateral n. N quadrilaterals are drawn. Note that the order in which vertexes are used to construct a quadrilateral from strip data is different from that used with independent data.
 GL_POLYGON                     =$0009; // Draws a single, convex polygon. Vertexes 1 through N define this polygon.

 GL_DEPTH_BUFFER_BIT            =$0100; // Indicates the depth buffer.

 GL_ACCUM                       =$0100;
 GL_LOAD                        =$0101;
 GL_RETURN                      =$0102;
 GL_MULT                        =$0103;
 GL_ADD                         =$0104;

 GL_ACCUM_BUFFER_BIT            =$0200; // Indicates the accumulation buffer.
 GL_NEVER                       =$0200; // Never passes.
 GL_LESS                        =$0201; // Passes if the incoming z value is less than the stored z value.
 GL_EQUAL                       =$0202; // Passes if the incoming z value is equal to the stored z value.
 GL_LEQUAL                      =$0203; // Passes if the incoming z value is less than or equal to the stored z value.
 GL_GREATER                     =$0204; // Passes if the incoming z value is greater than the stored z value.
 GL_NOTEQUAL                    =$0205; // Passes if the incoming z value is not equal to the stored z value.
 GL_GEQUAL                      =$0206; // Passes if the incoming z value is greater than or equal to the stored z value.
 GL_ALWAYS                      =$0207; // Always passes.

 GL_SRC_COLOR                   =$0300;
 GL_ONE_MINUS_SRC_COLOR         =$0301;
 GL_SRC_ALPHA                   =$0302;
 GL_ONE_MINUS_SRC_ALPHA         =$0303;
 GL_DST_ALPHA                   =$0304;
 GL_ONE_MINUS_DST_ALPHA         =$0305;
 GL_DST_COLOR                   =$0306;
 GL_ONE_MINUS_DST_COLOR         =$0307;
 GL_SRC_ALPHA_SATURATE          =$0308;

 GL_STENCIL_BUFFER_BIT          =$0400; // Indicates the stencil buffer.
 GL_FRONT                       =$0404;
 GL_BACK                        =$0405;
 GL_FRONT_AND_BACK              =$0408;

 GL_INVALID_ENUM                =$0500; // is generated if cap is not one of the values listed above.
 GL_INVALID_VALUE               =$0501;
 GL_INVALID_OPERATION           =$0502; // is generated if glEnable is called between a call to glBegin and the corresponding call to glEnd.
 GL_STACK_OVERFLOW              =$0503;
 GL_STACK_UNDERFLOW             =$0504;
 GL_OUT_OF_MEMORY               =$0505;

 GL_EXP                         =$0800;
 GL_EXP2                        =$0801;

 GL_CW                          =$0900;
 GL_CCW                         =$0901;

 GL_POINT_SMOOTH                =$0B10; // If enabled, draw points with proper filtering. Otherwise, draw aliased points. See glPointSize.
 GL_LINE_SMOOTH                 =$0B20; // If enabled, draw lines with correct filtering. Otherwise, draw aliased lines. See glLineWidth.
 GL_LINE_STIPPLE                =$0B24; // If enabled, use the current line stipple pattern when drawing lines. See glLineStipple.
 GL_POLYGON_SMOOTH              =$0B41; // If enabled, draw polygons with proper filtering. Otherwise, draw aliased polygons. See glPolygonMode.
 GL_POLYGON_STIPPLE             =$0B42; // If enabled, use the current polygon stipple pattern when rendering polygons. See glPolygonStipple.
 GL_CULL_FACE                   =$0B44; // If enabled, cull polygons based on their winding in window coordinates. See glCullFace.
 GL_LIGHTING                    =$0B50; // If enabled, use the current lighting parameters to compute the vertex color or index. Otherwise, simply associate the current color or index with each vertex. See glMaterial, glLightModel, and glLight.
 GL_LIGHT_MODEL_LOCAL_VIEWER    =$0B51;
 GL_LIGHT_MODEL_TWO_SIDE        =$0B52;
 GL_LIGHT_MODEL_AMBIENT         =$0B53;
 GL_COLOR_MATERIAL              =$0B57; // If enabled, have one or more material parameters track the current color. See glColorMaterial.
 GL_FOG                         =$0B60; // If enabled, blend a fog color into the posttexturing color. See glFog.
 GL_FOG_INDEX                   =$0B61;
 GL_FOG_DENSITY                 =$0B62;
 GL_FOG_START                   =$0B63;
 GL_FOG_END                     =$0B64;
 GL_FOG_MODE                    =$0B65;
 GL_FOG_COLOR                   =$0B66;
 GL_DEPTH_TEST                  =$0B71; // If enabled, do depth comparisons and update the depth buffer. See glDepthFunc and glDepthRange.
 GL_ACCUM_CLEAR_VALUE           =$0B80;
 GL_STENCIL_TEST                =$0B90; // If enabled, do stencil testing and update the stencil buffer. See glStencilFunc and glStencilOp.
 GL_NORMALIZE                   =$0BA1; // If enabled, normal vectors specified with glNormal are scaled to unit length after transformation. See glNormal.
 GL_VIEWPORT                    =$0BA2;
 GL_MODELVIEW_MATRIX            =$0BA6;
 GL_PROJECTION_MATRIX           =$0BA7;
 GL_ALPHA_TEST                  =$0BC0; // If enabled, do alpha testing. See glAlphaFunc.
 GL_DITHER                      =$0BD0; // If enabled, dither color components or indices before they are written to the color buffer.
 GL_BLEND_DST                   =$0BE0;
 GL_BLEND_SRC                   =$0BE1;
 GL_BLEND                       =$0BE2; // If enabled, blend the incoming RGBA color values with the values in the color buffers. See glBlendFunc.
 GL_LOGIC_OP                    =$0BF1; // If enabled, apply the currently selected logical operation to the incoming and color buffer indices. See glLogicOp.

 GL_SCISSOR_TEST                =$0C11; // If enabled, discard fragments that are outside the scissor rectangle. See glScissor.
 GL_PERSPECTIVE_CORRECTION_HINT =$0C50;
 GL_POINT_SMOOTH_HINT           =$0C51;
 GL_LINE_SMOOTH_HINT            =$0C52;
 GL_POLYGON_SMOOTH_HINT         =$0C53;
 GL_FOG_HINT                    =$0C54;
 GL_TEXTURE_GEN_S               =$0C60; // If enabled, the s texture coordinate is computed using the texture generation function defined with glTexGen. Otherwise, the current s texture coordinate is used.
 GL_TEXTURE_GEN_T               =$0C61; // If enabled, the t texture coordinate is computed using the texture generation function defined with glTexGen. Otherwise, the current t texture coordinate is used.
 GL_TEXTURE_GEN_Q               =$0C63; // If enabled, the q texture coordinate is computed using the texture generation function defined with glTexGen. Otherwise, the current q texture coordinate is used.
 GL_TEXTURE_GEN_R               =$0C62; // If enabled, the r texture coordinate is computed using the texture generation function defined with glTexGen. Otherwise, the current r texture coordinate is used.
 GL_UNPACK_SWAP_BYTES           =$0CF0;
 GL_UNPACK_LSB_FIRST            =$0CF1;
 GL_UNPACK_ROW_LENGTH           =$0CF2;
 GL_UNPACK_SKIP_PIXELS          =$0CF4;
 GL_UNPACK_SKIP_ROWS            =$0CF3;
 GL_UNPACK_ALIGNMENT            =$0CF5;

 GL_PACK_SWAP_BYTES             =$0D00;
 GL_PACK_LSB_FIRST              =$0D01;
 GL_PACK_ROW_LENGTH             =$0D02;
 GL_PACK_SKIP_PIXELS            =$0D04;
 GL_PACK_SKIP_ROWS              =$0D03;
 GL_PACK_ALIGNMENT              =$0D05;
 GL_AUTO_NORMAL                 =$0D80; // If enabled, compute surface normal vectors analytically when either GL_MAP2_VERTEX_3 or GL_MAP2_VERTEX_4 is used to generate vertexes. See glMap2.

 GL_TEXTURE_1D                  =$0DE0; // If enabled, one-dimensional texturing is performed (unless two-dimensional texturing is also enabled). See glTexImage1D.
 GL_TEXTURE_2D                  =$0DE1; // If enabled, two-dimensional texturing is performed. See glTexImage2D.

 GL_DONT_CARE                   =$1100; // The client doesn't have a preference.
 GL_FASTEST                     =$1101; // The most efficient option should be chosen.
 GL_NICEST                      =$1102; // The most correct, or highest quality, option should be chosen.

 GL_AMBIENT                     =$1200;
 GL_DIFFUSE                     =$1201;
 GL_SPECULAR                    =$1202;
 GL_POSITION                    =$1203;
 GL_SPOT_DIRECTION              =$1204;
 GL_SPOT_EXPONENT               =$1205;
 GL_SPOT_CUTOFF                 =$1206;
 GL_CONSTANT_ATTENUATION        =$1207;
 GL_LINEAR_ATTENUATION          =$1208;
 GL_QUADRATIC_ATTENUATION       =$1209;

 GL_COMPILE                     =$1300;
 GL_COMPILE_AND_EXECUTE         =$1301;

 GL_BYTE                        =$1400;
 GL_UNSIGNED_BYTE               =$1401;
 GL_SHORT                       =$1402;
 GL_UNSIGNED_SHORT              =$1403;
 GL_INT                         =$1404;
 GL_UNSIGNED_INT                =$1405;
 GL_FLOAT                       =$1406;
 GL_2_BYTES                     =$1407;
 GL_3_BYTES                     =$1408;
 GL_4_BYTES                     =$1409;
 GL_DOUBLE                      =$140A;
 GL_DOUBLE_EXT                  =$140A;

 GL_INVERT                      =$150A;

 GL_EMISSION                    =$1600;
 GL_SHININESS                   =$1601;
 GL_AMBIENT_AND_DIFFUSE         =$1602;

 GL_MODELVIEW                   =$1700; // Applies subsequent matrix operations to the modelview matrix stack.
 GL_PROJECTION                  =$1701; // Applies subsequent matrix operations to the projection matrix stack.
 GL_TEXTURE                     =$1702; // Applies subsequent matrix operations to the texture matrix stack.

 GL_COLOR_INDEX                 =$1900;
 GL_RED                         =$1903;
 GL_GREEN                       =$1904;
 GL_BLUE                        =$1905;
 GL_ALPHA                       =$1906;
 GL_RGB                         =$1907;
 GL_RGBA                        =$1908;
 GL_LUMINANCE                   =$1909;
 GL_LUMINANCE_ALPHA             =$190A;

 GL_BITMAP                      =$1A00;

 GL_POINT                       =$1B00;
 GL_LINE                        =$1B01;
 GL_FILL                        =$1B02;

 GL_RENDER                      =$1C00;
 GL_FEEDBACK                    =$1C01;
 GL_SELECT                      =$1C02;

 GL_FLAT                        =$1D00;
 GL_SMOOTH                      =$1D01;

 GL_KEEP                        =$1E00;
 GL_REPLACE                     =$1E01;
 GL_INCR                        =$1E02;
 GL_DECR                        =$1E03;

 GL_VENDOR                      =$1F00; // Returns the company responsible for this GL implementation. This name does not change from release to release.
 GL_RENDERER                    =$1F01; // Returns the name of the renderer. This name is typically specific to a particular configuration of a hardware platform. It does not change from release to release.
 GL_VERSION                     =$1F02; // Returns a version or release number.
 GL_EXTENSIONS                  =$1F03; // Returns a space-separated list of supported extensions to GL. );

 GL_MODULATE                    =$2100;
 GL_DECAL                       =$2101;

 GL_TEXTURE_ENV_MODE            =$2200;
 GL_TEXTURE_ENV_COLOR           =$2201;
 GL_TEXTURE_ENV                 =$2300;

 GL_NEAREST                     =$2600;
 GL_LINEAR                      =$2601;

 GL_NEAREST_MIPMAP_NEAREST      =$2700;
 GL_LINEAR_MIPMAP_NEAREST       =$2701;
 GL_NEAREST_MIPMAP_LINEAR       =$2702;
 GL_LINEAR_MIPMAP_LINEAR        =$2703;

 GL_TEXTURE_MAG_FILTER          =$2800;
 GL_TEXTURE_MIN_FILTER          =$2801;
 GL_TEXTURE_WRAP_S              =$2802;
 GL_TEXTURE_WRAP_T              =$2803;

 GL_CLAMP                       =$2900;
 GL_REPEAT                      =$2901;

 GL_COLOR_BUFFER_BIT            =$4000; // Indicates the buffers currently enabled for color writing.
 GL_LIGHT0                      =$4000; // If enabled, include light i in the evaluation of the lighting equation. See glLightModel and glLight.
 GL_LIGHT1                      =$4001;
 GL_LIGHT2                      =$4002;
 GL_LIGHT3                      =$4003;
 GL_LIGHT4                      =$4004;
 GL_LIGHT5                      =$4005;
 GL_LIGHT6                      =$4006;
 GL_LIGHT7                      =$4007;

 GL_POLYGON_OFFSET_EXT          =$8037;
 GL_POLYGON_OFFSET_FILL         =$8037;

 GL_VERTEX_ARRAY                =$8074;
 GL_NORMAL_ARRAY                =$8075;
 GL_COLOR_ARRAY                 =$8076;
 GL_INDEX_ARRAY                 =$8077;
 GL_TEXTURE_COORD_ARRAY         =$8078;
 GL_EDGE_FLAG_ARRAY             =$8079;
 GL_VERTEX_ARRAY_SIZE           =$807A;
 GL_VERTEX_ARRAY_TYPE           =$807B;
 GL_VERTEX_ARRAY_STRIDE         =$807C;
 GL_NORMAL_ARRAY_TYPE           =$807E;
 GL_NORMAL_ARRAY_STRIDE         =$807F;
 GL_COLOR_ARRAY_SIZE            =$8081;
 GL_COLOR_ARRAY_TYPE            =$8082;
 GL_COLOR_ARRAY_STRIDE          =$8083;
 GL_INDEX_ARRAY_TYPE            =$8085;
 GL_INDEX_ARRAY_STRIDE          =$8086;
 GL_TEXTURE_COORD_ARRAY_SIZE    =$8088;
 GL_TEXTURE_COORD_ARRAY_TYPE    =$8089;
 GL_TEXTURE_COORD_ARRAY_STRIDE  =$808A;
 GL_EDGE_FLAG_ARRAY_STRIDE      =$808C;

 GL_BGR_EXT                     =$80E0;
 GL_BGRA_EXT                    =$80E1;

{$IFDEF Android}
const
  OpenGL = '/usr/lib/libGLESv1_CM.so';
  Prefix = '';
  {$DEFINE CDECL}
{$ENDIF}

{$IFDEF MSWINDOWS}
const
  OpenGL = 'OpenGL32.DLL';
  {$DEFINE STDCALL}
{$ENDIF}

{$IFDEF MACOS}
const
  OpenGL = '/System/Library/Frameworks/OpenGL.framework/OpenGL';
  Prefix = '_';
  {$DEFINE CDECL}
{$ENDIF}

{$IFDEF CDECL}
procedure glClear(mask: GLbitfield); cdecl; external OpenGL name Prefix + 'glClear';
procedure glClearColor(red, green, blue, alpha: GLclampf); cdecl; external OpenGL name Prefix + 'glClearColor';
procedure glDrawArrays(mode: GLenum; first: GLint; count: GLsizei); cdecl; external OpenGL name Prefix + 'glDrawArrays';
procedure glEnable(cap: GLenum); cdecl; external OpenGL name Prefix + 'glEnable';
procedure glEnableClientState(array_: GLenum); cdecl; external OpenGL name Prefix + 'glEnableClientState';
procedure glFlush; cdecl; external OpenGL name Prefix + 'glFlush';
procedure glHint(target, mode: GLenum); cdecl; external OpenGL name Prefix + 'glHint';
procedure glLightfv(light, pname: GLenum; params: PGLfloat); cdecl; external OpenGL name Prefix + 'glLightfv';
procedure glLoadIdentity; cdecl; external OpenGL name Prefix + 'glLoadIdentity';
procedure glMatrixMode (mode: GLenum); cdecl; external OpenGL name Prefix + 'glMatrixMode';
procedure glMultMatrixf (m: PGLfloat); cdecl; external OpenGL name Prefix + 'glMultMatrixf';
procedure glNormalPointer(type_: GLenum; stride: GLsizei; pointer: PGLvoid); cdecl; external OpenGL name Prefix + 'glNormalPointer';
procedure glPopMatrix; cdecl; external OpenGL name Prefix + 'glPopMatrix';
procedure glPushMatrix; cdecl; external OpenGL name Prefix + 'glPushMatrix';
procedure glRotatef(angle, x, y, z: GLfloat); cdecl; external OpenGL name Prefix + 'glRotatef';
procedure glShadeModel(mode: GLenum); cdecl; external OpenGL name Prefix + 'glShadeModel';
procedure glTranslatef(x, y, z: GLfloat); cdecl; external OpenGL name Prefix + 'glTranslatef';
procedure glVertexPointer(size: GLint; type_: GLenum; stride: GLsizei; pointer: PGLvoid); cdecl; external OpenGL name Prefix + 'glVertexPointer';
procedure glViewport (x,y: GLint; width, height: GLsizei); cdecl; external OpenGL name Prefix + 'glViewport';
{$ENDIF}

{$IFDEF STDCALL}
procedure glClear(mask: GLbitfield); stdcall; external OpenGL;
procedure glClearColor(red, green, blue, alpha: GLclampf); stdcall; external OpenGL;
procedure glDrawArrays(mode: GLenum; first: GLint; count: GLsizei); stdcall; external OpenGL;
procedure glEnable(cap: GLenum); stdcall; external OpenGL;
procedure glEnableClientState(array_: GLenum); stdcall; external OpenGL;
procedure glFlush; stdcall; external OpenGL;
procedure glHint(target, mode: GLenum); stdcall; external OpenGL;
procedure glLightfv(light, pname: GLenum; params: PGLfloat); stdcall; external OpenGL;
procedure glLoadIdentity; stdcall; external OpenGL;
procedure glNormalPointer(type_: GLenum; stride: GLsizei; pointer: Pointer); stdcall; external OpenGL;
procedure glMatrixMode (mode: GLenum); stdcall; external OpenGL;
procedure glMultMatrixf (m: PGLfloat); stdcall; external OpenGL;
procedure glPopMatrix; stdcall; external OpenGL;
procedure glPushMatrix; stdcall; external OpenGL;
procedure glRotatef(angle, x, y, z: GLfloat); stdcall; external OpenGL;
procedure glShadeModel(mode: GLenum); stdcall; external OpenGL;
procedure glTranslatef(x, y, z: GLfloat); stdcall; external OpenGL;
procedure glVertexPointer(size: GLint; type_: GLenum; stride: GLsizei; pointer: Pointer); stdcall; external OpenGL;
procedure glViewport (x,y: GLint; width, height: GLsizei); stdcall; external OpenGL;
{$ENDIF}

procedure Log(const Str: string);

implementation

{$IFDEF ANDROID}

procedure Log(const Str: string);
{$IFDEF DEBUG}
var
  M: TMarshaller;
begin
// Oh dear ! give us back our AnsiString !
  LOGI(M.AsAnsi(Str).ToPointer);
end;
{$ELSE}
begin
end;
{$ENDIF}

// GetTickCount from  System.pas (not published)
const
  CLOCK_MONOTONIC = 1;

type
  timespec = record
    tv_sec: Longint;
    tv_nsec: Int32;
  end;
  Ptimespec = ^timespec;

function clock_gettime(Clk: Integer; Tp: Ptimespec): Integer; cdecl;
  external '/system/lib/libc' name 'clock_gettime';

function GetTickCount: Cardinal; inline;
var
  res: timespec;
begin
  clock_gettime(CLOCK_MONOTONIC, @res);
  Result := (Int64(1000000000) * res.tv_sec + res.tv_nsec) div 1000000;
end;
{$ENDIF Android}

{$IFDEF MSWINDOWS}
procedure Log(const Str: string);
begin
{$IFDEF DEBUG}
  AllocConsole;
  WriteLn(Str);
{$ENDIF DEBUG}
end;
{$ENDIF MSWINDOWS}

{$IFDEF MACOS}

procedure Log(const Str: string);
begin
{$IFDEF DEBUG}
  WriteLn(Str);
{$ENDIF DEBUG}
end;

const
  LibcLib = '/usr/lib/libc.dylib';

type
  TTimebaseInfoData = record
    Numer: UInt32;
    Denom: UInt32;
  end;

function MachAbsoluteTime: UInt64; cdecl; external LibcLib name '_mach_absolute_time';
function MachTimebaseInfo(var TimebaseInfoData: TTimebaseInfoData): Integer; cdecl; external LibcLib name '_mach_timebase_info';

function AbsoluteToNanoseconds(AbsoluteTime: UInt64): UInt64;
var
  Info: TTimebaseInfoData;
begin
  MachTimebaseInfo(Info);
  Result := AbsoluteTime * Info.Numer;
  Result := Result div Info.Denom;
end;

function GetTickCount: Cardinal; inline;
begin
  Result := AbsoluteToNanoseconds(MachAbsoluteTime) div 1000000;
end;

{$ENDIF MACOS}

procedure gluPerspective(fovy, aspect, zNear, zFar: double);
var
  m: array[0..3,0..3] of Single;
  sn, cs: Extended;
  cotangent, deltaZ: Double;
  radians: Double;
begin
  if aspect = 0 then
    Exit;
  deltaZ := zFar - zNear;
  if deltaZ = 0 then
    Exit;
  radians := fovy * PI / 360;
  SinCos(radians, sn, cs);
  if sn = 0 then
    Exit;
  cotangent := cs / sn;
  FillChar(m, SizeOf(m), 0);
  m[0,0] := cotangent / aspect;
  m[1,1] := cotangent;
  m[2,2] := -(zFar + zNear) / deltaZ;
  m[2,3] := -1;
  m[3,2] := -2 * zNear * zFar / deltaZ;
  glMultMatrixf(@m);
end;

{$IFDEF Android}
procedure onAppCmd(app: pandroid_app; cmd: Integer); cdecl;
begin
  case cmd of
    APP_CMD_INIT_WINDOW:
    begin
      Log('CrossGL: APP_CMD_INIT_WINDOW');
      if (app.window <> nil) then
      begin
        TGLContext(app.userData).CreateContext;
      end;
    end;
    APP_CMD_TERM_WINDOW:
    begin
      Log('CrossGL: APP_CMD_TERM_WINDOW');
      TGLContext(app.userData).DestroyContext;
    end;
    APP_CMD_GAINED_FOCUS:
    begin
      Log('CrossGL: APP_CMD_GAINED_FOCUS');
      TGLContext(app.userData).OnEnter;
    end;
    APP_CMD_LOST_FOCUS:
    begin
      Log('CrossGL: APP_CMD_LOST_FOCUS');
      TGLContext(app.userData).OnLeave;
    end;
    APP_CMD_CONFIG_CHANGED:
    begin
      Log('CrossGL: APP_CMD_CONFIG_CHANGED');
      TGLContext(app.userData).FConfigChanged := True;
    end;
  end;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
const
  WindowClassName = 'Execute.CrossGL';

function WndProc(hWnd: THandle; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
begin
  Result := DefWindowProc(hWnd, Msg, wParam, lParam);
  case Msg of
    WM_CREATE : SetWindowLong(hWnd, GWL_USERDATA, NativeInt(CREATESTRUCT(Pointer(lParam)^).lpCreateParams));
    WM_PAINT  : TGLContext.FromHWnd(hWnd).Repaint;
    WM_SIZE   : TGLContext.FromHWnd(hWnd).SetSize(LOWORD(lParam), HIWORD(lParam));
    WM_DESTROY: PostQuitMessage(0);
  end;
end;
{$ENDIF}

{$IFDEF MACOS}
type
  TCrossGLApplication = class(NSApplication)
  private
    FContext: TGLContext;
  protected
    procedure SetupGL; override;
    procedure OnResize; override;
    procedure OnPaint; override;
    procedure OnIdle; override;
  public
    constructor Create(AContext: TGLContext);
  end;

constructor TCrossGLApplication.Create(AContext: TGLContext);
begin
  FContext := AContext;
  inherited Create(FContext.FWidth, FContext.FHeight);
end;

procedure TCrossGLApplication.SetupGL;
begin
  inherited;
  FContext.CreateContext;
end;

procedure TCrossGLApplication.OnResize;
begin
  FContext.SetSize(Width, Height);
end;

procedure TCrossGLApplication.OnPaint;
begin
  FContext.Repaint;
end;

procedure TCrossGLApplication.OnIdle;
begin
  Invalidate;
end;
{$ENDIF}

{ TGLContext }

constructor TGLContext.Create(AWidth, AHeight: Integer; AFieldOfView, AZNear, AZFar: Single);
{$IFDEF MSWINDOWS}
var
  WndClass: TWndClass;
{$ENDIF}
begin
  Log('CrossGL: InitApp');
  FWidth := AWidth;
  FHeight := AHeight;
  FFieldOfView := AFieldOfView;
  FZNear:= AZNear;
  FZFar := AZFar;

{$IFDEF Android}
  FApp := InitApp;
  FApp.userData := Self;
  FApp.onAppCmd := OnAppCmd;
  FDisplay := EGL_NO_DISPLAY;
{$ENDIF}

{$IFDEF MSWINDOWS}
  FillChar(WndClass, SizeOf(WndClass), 0);
  WndClass.lpfnWndProc := @WndProc;
  WndClass.hInstance := hInstance;
  WndClass.lpszClassName := WindowClassName;
  if RegisterClass(WndClass) = 0 then
    RaiseLastOSError;
  FHandle := CreateWindowEx(
    0,
    WindowClassName,
    'CrossGL by Execute',
    WS_OVERLAPPEDWINDOW or WS_VISIBLE,
    Integer(CW_USEDEFAULT), Integer(CW_USEDEFAULT), FWidth, FHeight,
    0, 0, hInstance,
    Self
  );
  if FHandle = 0 then
    RaiseLastOSError;
  CreateContext;
{$ENDIF}

{$IFDEF MACOS}
  FApp := TCrossGLApplication.Create(Self);
{$ENDIF}
end;

procedure TGLContext.CreateContext;
{$IFDEF Android}
const
  attribs : array[0..8] of EGLint = (
    EGL_DEPTH_SIZE, 16,
    EGL_BLUE_SIZE, 4,
    EGL_GREEN_SIZE, 4,
    EGL_RED_SIZE, 4,
    EGL_NONE
  );
var
  format      : EGLint;
  numConfigs  : EGLint;
  config      : EGLConfig;
{$ENDIF}
{$IFDEF MSWINDOWS}
var
  pfd        : TPIXELFORMATDESCRIPTOR;
  PixelFormat: Integer;
{$ENDIF}
begin
{$IFDEF Android}
  FDisplay := eglGetDisplay(EGL_DEFAULT_DISPLAY);
  eglInitialize(FDisplay, nil, nil);
  eglChooseConfig(FDisplay, @attribs[0], @config, 1, @numConfigs);
  eglGetConfigAttrib(FDisplay, config, EGL_NATIVE_VISUAL_ID, @format);

  ANativeWindow_setBuffersGeometry(FApp.window, 0, 0, format);

  FSurface := eglCreateWindowSurface(FDisplay, config, FApp.window, nil);
  FContext := eglCreateContext(FDisplay, config, nil, nil);

  if (eglMakeCurrent(FDisplay, FSurface, FSurface, FContext) = EGL_FALSE) then
  begin
    Abort;
  end;

  eglQuerySurface(FDisplay, FSurface, EGL_WIDTH, @FWidth);
  eglQuerySurface(FDisplay, FSurface, EGL_HEIGHT, @FHeight);
{$ENDIF}

{$IFDEF MSWINDOWS}
  FillChar(pfd, SizeOf(pfd), 0);
  pfd.nSize        := SizeOf(pfd);
  pfd.nVersion     := 1;
  pfd.dwFlags      := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
  pfd.iLayerType   := PFD_MAIN_PLANE;
  pfd.iPixelType   := PFD_TYPE_RGBA;
  pfd.cColorBits   := 32;
  pfd.iLayerType   := PFD_MAIN_PLANE;
  pfd.cDepthBits   := 16;
  pfd.cStencilBits := 32;

  FDC := GetDC(FHandle);

  PixelFormat := ChoosePixelFormat(FDC, @pfd);
  if PixelFormat = 0 then
      RaiseLastOSError;

  if not SetPixelFormat(FDC, PixelFormat, @pfd) then
    RaiseLastOSError;

  FGL := wglCreateContext(FDC);
  wglMakeCurrent(FDC, FGL);
{$ENDIF}

  OnSetup;
  OnResize;

  FTicks := GetTickCount;
  FFocused := True;
end;

procedure TGLContext.DestroyContext;
begin
{$IFDEF Android}
  if (FDisplay <> EGL_NO_DISPLAY) then
  begin
    eglMakeCurrent(FDisplay, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT);
    if (FContext <> EGL_NO_CONTEXT) then
    begin
        eglDestroyContext(FDisplay, FContext);
        FContext := EGL_NO_CONTEXT;
    end;
    if (FSurface <> EGL_NO_SURFACE) then
    begin
        eglDestroySurface(FDisplay, FSurface);
        FSurface := EGL_NO_SURFACE;
    end;
    eglTerminate(FDisplay);
    FDisplay := EGL_NO_DISPLAY;
  end;
{$ENDIF}
end;

procedure TGLContext.Execute;

{$IFDEF Android}
var
  ident : Integer;
  events: Integer;
  source: pandroid_poll_source;
  width : Integer;
  height: Integer;
{$ENDIF}

{$IFDEF MSWINDOWS}
var
  Msg: TMsg;
{$ENDIF}

begin

{$IFDEF Android}
  while FApp.destroyRequested = 0 do
  begin

    ident := ALooper_pollAll(0, nil, @events, @source);

    if (ident >= 0) and (source <> nil) then
      source.process(FApp, source);

    if FFocused and (FDisplay <> EGL_NO_DISPLAY) then
    begin
      if FConfigChanged then
      begin
        width := FWidth;
        height := FHeight;
        // it take some frames to retrieve the right dimensions !
        eglQuerySurface(FDisplay, FSurface, EGL_WIDTH, @FWidth);
        eglQuerySurface(FDisplay, FSurface, EGL_HEIGHT, @FHeight);
        if (width <> FWidth) or (height <> FHeight) then
        begin
          FConfigChanged := False;
          OnResize();
        end;
      end;
      Repaint();
    end;

  end;
{$ENDIF}

{$IFDEF MSWINDOWS}
  while GetMessage(Msg, 0, 0, 0) do
  begin
    if Msg.Message = WM_QUIT then
      Break;
    TranslateMessage(Msg);
    DispatchMessage(Msg);
    InvalidateRect(FHandle, nil, False);

  end;
  DestroyContext;
{$ENDIF}

{$IFDEF MACOS}
  FApp.Run;
  DestroyContext;
{$ENDIF}
end;

{$IFDEF MSWINDOWS}
class function TGLContext.FromHWnd(hWnd: THandle): TGLContext;
begin
  Result := TObject(GetWindowLong(hWnd, GWL_USERDATA)) as TGLContext;
end;
{$ENDIF}

procedure TGLContext.SetSize(AWidth, AHeight: Integer);
begin
  FWidth := AWidth;
  FHeight := AHeight;
  OnResize();
end;

procedure TGLContext.Repaint;
var
  FPS: Cardinal;
begin
  FTime := GetTickCount - FTicks;
  if FTime > 0 then
  begin
    FPS := (1000 * FFrames) div FTime;
    //Log(IntToStr(FPS) + ' FPS');
    if FPS < 25 then
    begin
      Inc(FFrames);
      OnPaint;
    end else begin
      Sleep(40 * FFrames - FTime); // 40ms per Frame = 25 FPS
    end;
  end;
end;

procedure TGLContext.OnEnter;
begin

end;

procedure TGLContext.OnLeave;
begin

end;

procedure TGLContext.OnResize;
begin
  glViewPort(0, 0, FWidth, FHeight);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(FFieldOfView, FWidth / FHeight, FZNear, FZFar);
  glMatrixMode(GL_MODELVIEW);
end;

procedure TGLContext.OnSetup;
begin
end;

procedure TGLContext.OnPaint;
begin
end;

procedure TGLContext.SwapBuffers;
begin
  glFlush;
{$IFDEF Android}
  eglSwapBuffers(FDisplay, FSurface);
{$ENDIF}
{$IFDEF MSWINDOWS}
  Winapi.Windows.SwapBuffers(FDC);
{$ENDIF}
end;

end.

