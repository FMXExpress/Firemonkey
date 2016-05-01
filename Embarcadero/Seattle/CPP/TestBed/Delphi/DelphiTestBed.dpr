program DelphiTestBed;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainTestBed in 'MainTestBed.pas' {Form6},
  DebugDraw in 'DebugDraw.pas',
  Box2D.Collision in '..\..\..\intermediate\FlatBox2D\Box2D.Collision.pas',
  Box2D.Common in '..\..\..\intermediate\FlatBox2D\Box2D.Common.pas',
  Box2D.Dynamics in '..\..\..\intermediate\FlatBox2D\Box2D.Dynamics.pas',
  Box2D.Rope in '..\..\..\intermediate\FlatBox2D\Box2D.Rope.pas',
  Box2DTypes in '..\..\..\intermediate\FlatBox2D\Box2DTypes.pas',
  Test in 'Test.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm6, Form6);
  Application.Run;
end.
