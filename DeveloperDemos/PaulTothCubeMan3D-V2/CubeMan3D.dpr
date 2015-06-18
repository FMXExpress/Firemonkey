program CubeMan3D;
{
   CubeMan3D demo program for Delphi XE2/XE5

   (c)2014 Execute SARL  <contact@execute.re>

   http://www.execute.re
   
   Build the Win32/64 and MacOS versions with Build.cmd
   
   Build the Android version from the IDE (dccaarm.exe can only build a .so, not an APK)

}
uses
  Execute.CrossGL in 'Execute.CrossGL.pas';

var
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

procedure BuildCube(var Cube: TCube; Width, Height, Depth: Single);
var
  Face  : Integer;
  Vertex: Integer;
  Normal: TVertex;
begin
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

type
  TApplication = class(TGLContext)
  protected
    procedure OnSetup; override;
    procedure OnPaint; override;
  end;

var
  Application: TApplication;

procedure TApplication.OnSetup;
begin
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_FASTEST);
  glEnable(GL_CULL_FACE);
  glEnable(GL_DEPTH_TEST);
  glShadeModel(GL_SMOOTH);

  glLightfv(GL_LIGHT0, GL_AMBIENT, @Ambient);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, @Diffuse);
  glLightfv(GL_LIGHT0, GL_POSITION, @Position);
  glEnable(GL_LIGHT0);
  glEnable(GL_LIGHTING);

  glVertexPointer(3, GL_FLOAT, SizeOf(TPoint), @Cubes[0, 0, 0].Vertex);
  glNormalPointer(GL_FLOAT, SizeOf(TPoint), @Cubes[0, 0, 0].Normal);

  glEnableClientState(GL_VERTEX_ARRAY);
  glEnableClientState(GL_NORMAL_ARRAY);
end;


procedure TApplication.OnPaint;
var
  Timer: Cardinal;
begin
  glClearColor(0.5, 0.5, 0.5, 1);

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  glLoadIdentity();
  glTranslatef(0, 0, -250);
  glRotatef(Time/50, 0, 1, 0);

  Timer := Time div 2;

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
    glRotatef((1-Integer((Timer div 2) mod 480) div 240) * (120 -  Abs((Timer div 2) mod 240 - 120)), 0, 1, 0);
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
    glRotatef((Integer((Timer div 2) mod 480) div 240) * (120 -  Abs((Timer div 2) mod 240 - 120)), 0, 1, 0);
    glTranslatef(0, 0, -10);
    glDrawArrays(GL_TRIANGLES, CUBE_2, CUBE_SIZE);
  glPopMatrix;

  SwapBuffers;
end;

begin
  Log('CrossGL: Create Application');
  Application := TApplication.Create(400, 600, 45, 100, 300);

  Log('CrossGL: Setup cubes');
  BuildCube(Cubes[0], 10, 17, 15);
  BuildCube(Cubes[1], 10, 10, 10);
  BuildCube(Cubes[2],  6,  6, 10);

  Log('CrossGL: Execute');
  Application.Execute;

  Log('CrossGL: Done');
end.
