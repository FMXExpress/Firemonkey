program CubeMan3D;

{
  Delphi XE5/Android version of the FlashPascal CubeMan3D sample
  (c)2014 Execute SARL <contact@execute.re>
  http://www.execute.re
  
  this is an XE5 Android application in only 826Kb !
  
  see also
  http://flashpascal.execute.re/?q=CubeMan3D
}

uses
  Posix.SysTypes,
  Androidapi.AppGlue,
  Androidapi.Looper,
  Androidapi.Input,
  Androidapi.Egl,
  Androidapi.Gles,
  Androidapi.NativeWindow,
  Androidapi.NativeActivity,
  Androidapi.Log,
  Math;

var
  display : EGLDisplay = EGL_NO_DISPLAY;
  surface : EGLSurface;
  context : EGLContext;
  width   : Integer;
  height  : Integer;
  focused : Boolean;

  Vertices: array[0..7, 0..2] of Single = (
    (-1.0, -1.0, -1.0), // 0
    ( 1.0, -1.0, -1.0), // 1
    ( 1.0,  1.0, -1.0), // 2
    (-1.0,  1.0, -1.0), // 3
    (-1.0, -1.0,  1.0), // 4
    ( 1.0, -1.0,  1.0), // 5
    ( 1.0,  1.0,  1.0), // 6
    (-1.0,  1.0,  1.0)  // 7
  );

  Indices: array[0..11, 0..2] of Byte = (
    (0, 5, 4), (0, 1, 5),
    (1, 6, 5), (1, 2, 6),
    (2, 7, 6), (2, 3, 7),
    (3, 4, 7), (3, 0, 4),
    (4, 6, 7), (4, 5, 6),
    (3, 1, 0), (3, 2, 1)
  );

  Diffuse : array[0..3] of Single = (0.5, 0.5, 1.0, 1);
  Ambient : array[0..3] of Single = (0.0, 0.0, 0.5, 1);
  Position: array[0..3] of Single = (0.0, 0.0, 0.0, 1);

const
  CUBE_SIZE = 3 * 2 * 6;
  CUBE_0 = 0;
  CUBE_1 = CUBE_SIZE;
  CUBE_2 = 2 * CUBE_SIZE;

type
  TVertex = record
    x, y, z: Single;
  end;

  TPoint = record
    Vertex: TVertex;
    Normal: TVertex;
  end;

  TFace = array[0..2] of TPoint; // triangles

  TCube = array[0..11] of TFace;  // 12 triangles

var
  Cubes: array[0..2] of TCube;
  Timer: Cardinal;
  Ticks: Cardinal;

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

// from Androidapi.AppGlue
//--------------------------
const
  CLOCK_MONOTONIC = 1;

type
  timespec = record
    tv_sec: time_t;
    tv_nsec: Int32;
  end;
  Ptimespec = ^timespec;

function clock_gettime(Clk: Integer; Tp: Ptimespec): Integer; cdecl;
  external '/system/lib/libc' name 'clock_gettime';

function GetMilliSecs: Cardinal; inline;
var
  res: timespec;
begin
  clock_gettime(CLOCK_MONOTONIC, @res);
  Result := (Int64(1000000000) * res.tv_sec + res.tv_nsec) div 1000000;
end;
//------------------------

procedure onResize;
begin
  if display <> EGL_NO_DISPLAY then
  begin
    eglQuerySurface(display, surface, EGL_WIDTH, @width);
    eglQuerySurface(display, surface, EGL_HEIGHT, @height);

    glViewPort(0, 0, width, height);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluPerspective(45, width/height, 100, 300);
    glMatrixMode(GL_MODELVIEW);
  end;
end;

function initOpenGL(app: pandroid_app): Integer; cdecl;
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
begin
  display := eglGetDisplay(EGL_DEFAULT_DISPLAY);
  eglInitialize(display, nil, nil);
  eglChooseConfig(display, @attribs[0], @config, 1, @numConfigs);
  eglGetConfigAttrib(display, config, EGL_NATIVE_VISUAL_ID, @format);

  ANativeWindow_setBuffersGeometry(app.window, 0, 0, format);

  surface := eglCreateWindowSurface(display, config, app.window, nil);
  context := eglCreateContext(display, config, nil, nil);

  if (eglMakeCurrent(display, surface, surface, context) = EGL_FALSE) then
  begin
    LOGW('Unable to eglMakeCurrent');
    Exit(-1);
  end;

  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_FASTEST);
  glEnable(GL_CULL_FACE);
  glEnable(GL_DEPTH_TEST);
  glShadeModel(GL_SMOOTH);

  onResize();

  glLightfv(GL_LIGHT0, GL_AMBIENT, @Ambient);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, @Diffuse);
  glLightfv(GL_LIGHT0, GL_POSITION, @Position);
  glEnable(GL_LIGHT0);
  glEnable(GL_LIGHTING);

  glVertexPointer(3, GL_FLOAT, SizeOf(TPoint), @Cubes[0, 0, 0].Vertex);
  glNormalPointer(GL_FLOAT, SizeOf(TPoint), @Cubes[0, 0, 0].Normal);

  glEnableClientState(GL_VERTEX_ARRAY);
  glEnableClientState(GL_NORMAL_ARRAY);

  focused := True;
  Ticks := GetMilliSecs;

  Result := 0;
end;

procedure drawFrame(); cdecl;
begin
  if display = EGL_NO_DISPLAY then
    Exit;

  glClearColor(0.5, 0.5, 0.5, 1);

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  glLoadIdentity();
  glTranslatef(0, 0, -250);
  glRotatef(GetMilliSecs/50, 0, 1, 0);

  Timer := (GetMilliSecs - Ticks) div 2;

  glPushMatrix;
    glDrawArrays(GL_TRIANGLES, CUBE_0, CUBE_SIZE);
    glTranslatef(0, +30, 0);
    glDrawArrays(GL_TRIANGLES, CUBE_1, CUBE_SIZE);
  glPopMatrix;
  glPushMatrix;
    glTranslatef(0, +15, 25-10);
    glRotatef(+45, 1, 0, 0);
    glRotatef(Abs((Timer div 3) mod 320 - 160) - 80, 0, 1, 0);
    glTranslatef(0, 0, 10);
    glDrawArrays(GL_TRIANGLES, CUBE_2, CUBE_SIZE);
    glTranslatef(0, 0, 20-10);
    glRotatef(Abs((Timer div 3) mod 180 - 90), 0, 1, 0);
    glTranslatef(0, 0, 10);
    glDrawArrays(GL_TRIANGLES, CUBE_2, CUBE_SIZE);
  glPopMatrix;
  glPushMatrix;
    glTranslatef(0, +15, 10-25);
    glRotatef(-45, 1, 0, 0);
    glRotatef(Abs((Timer div 3) mod 320 - 160) - 80, 0, 1, 0);
    glTranslatef(0, 0, -10);
    glDrawArrays(GL_TRIANGLES, CUBE_2, CUBE_SIZE);
    glTranslatef(0, 0, 10-20);
    glRotatef(-Abs((Timer div 3) mod 180 - 90), 0, 1, 0);
    glTranslatef(0, 0, -10);
    glDrawArrays(GL_TRIANGLES, CUBE_2, CUBE_SIZE);
  glPopMatrix;
  glPushMatrix;
    glTranslatef(0, 10-25, 10);
    glRotatef(-90, 1, 0, 0);
    glRotatef(Abs((Timer div 4) mod 240 - 120) - 60, 0, 1, 0);
    glTranslatef(0, 0, -10);
    glDrawArrays(GL_TRIANGLES, CUBE_2, CUBE_SIZE);
    glTranslatef(0, 0, 10-20);
    glRotatef((1-((Timer div 2) mod 480) div 240) * (120 -  Abs((Timer div 2) mod 240 - 120)), 0, 1, 0);
    glTranslatef(0, 0, -10);
    glDrawArrays(GL_TRIANGLES, CUBE_2, CUBE_SIZE);
  glPopMatrix;
  glPushMatrix;
    glTranslatef(0, 10-25, -10);
    glRotatef(-90, 1, 0, 0);
    glRotatef(-Abs((Timer div 4) mod 240 - 120) + 60, 0, 1, 0);
    glTranslatef(0, 0, -10);
    glDrawArrays(GL_TRIANGLES, CUBE_2, CUBE_SIZE);
    glTranslatef(0, 0, 10-20);
    glRotatef((((Timer div 2) mod 480) div 240) * (120 -  Abs((Timer div 2) mod 240 - 120)), 0, 1, 0);
    glTranslatef(0, 0, -10);
    glDrawArrays(GL_TRIANGLES, CUBE_2, CUBE_SIZE);
  glPopMatrix;

  eglSwapBuffers(display, surface);
end;

procedure doneOpenGL(); cdecl;
begin
  if (display <> EGL_NO_DISPLAY) then
  begin
      eglMakeCurrent(display, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT);
      if (context <> EGL_NO_CONTEXT) then
      begin
          eglDestroyContext(display, context);
      end;
      if (surface <> EGL_NO_SURFACE) then
      begin
          eglDestroySurface(display, surface);
      end;
      eglTerminate(display);
  end;
  display := EGL_NO_DISPLAY;
  context := EGL_NO_CONTEXT;
  surface := EGL_NO_SURFACE;
end;

(**
 * Process the next main command.
 *)
procedure onAppCmd(app: pandroid_app; cmd: Integer); cdecl;
begin
  case cmd of
    APP_CMD_INIT_WINDOW:
    begin
      if (app.window <> nil) then
      begin
        initOpenGL(app);
        drawFrame();
      end;
    end;
    APP_CMD_TERM_WINDOW:
    begin
      doneOpenGL();
    end;
    APP_CMD_GAINED_FOCUS:
    begin
      focused := True;
    end;
    APP_CMD_LOST_FOCUS:
    begin
      focused := False;
    end;
    APP_CMD_CONFIG_CHANGED:
    begin
      // how to handle screen orientation ?!
      onResize();
    end;
  end;
end;

procedure main(state: pandroid_app);
var
  ident : Integer;
  events: Integer;
  source: pandroid_poll_source;
begin

  state.onAppCmd := onAppCmd;

  while state.destroyRequested = 0 do
  begin

    ident := ALooper_pollAll(0, nil, @events, @source);

    if (ident >= 0) and (source <> nil) then
      source.process(state, source);

    if focused then
      drawFrame();

  end;

end;

procedure BuildCube(var Cube: TCube; Width, Height, Depth: Single);
var
  Index : Integer;
  Face  : Integer;
  Vertex: Integer;
  Normal: TVertex;
begin
  Index := 0;
  FillChar(Normal, SizeOf(Normal), 0);
  for Face := 0 to 11 do
  begin
    case Face of
      0:
      begin
        Normal.y := -1;
      end;
      2:
      begin
        Normal.y := 0;
        Normal.x := 1;
      end;
      4:
      begin
        Normal.x := 0;
        Normal.y := 1;
      end;
      6:
      begin
        Normal.y := 0;
        Normal.x := -1;
      end;
      8:
      begin
        Normal.x := 0;
        Normal.z := 1;
      end;
     10:
      begin
        Normal.z := -1;
      end;
    end;
    for Vertex := 0 to 2 do
    begin
      Cube[Face, Vertex].Vertex.x := Vertices[Indices[Face, Vertex], 0] * Width;
      Cube[Face, Vertex].Vertex.y := Vertices[Indices[Face, Vertex], 1] * Height;
      Cube[Face, Vertex].Vertex.z := Vertices[Indices[Face, Vertex], 2] * Depth;
      Cube[Face, Vertex].Normal := Normal;
    end;
  end;
end;

begin
  BuildCube(Cubes[0], 10, 17, 15);
  BuildCube(Cubes[1], 10, 10, 10);
  BuildCube(Cubes[2],  6,  6, 10);
  main(InitApp);
end.
