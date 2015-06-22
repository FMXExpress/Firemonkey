program ToppleTheTower;

uses
  FMX.Forms,
  uFormDBox2DSample in 'uFormDBox2DSample.pas' {GameForm},
  UPhysics2D in '..\..\Physics2D\UPhysics2D.pas',
  UPhysics2DControllers in '..\..\Physics2D\UPhysics2DControllers.pas',
  UPhysics2DHelper in '..\..\Physics2D\UPhysics2DHelper.pas',
  UPhysics2DPolygonTool in '..\..\Physics2D\UPhysics2DPolygonTool.pas',
  UPhysics2DTypes in '..\..\Physics2D\UPhysics2DTypes.pas',
  uXCadencer in '..\..\Physics2D\uXCadencer.pas',
  uDebugDrawer in '..\TestBedShared\uDebugDrawer.pas',
  uDebugDrawerFM in '..\TestBedShared\uDebugDrawerFM.pas',
  uSimulation in 'uSimulation.pas',
  uMySimulation in 'uMySimulation.pas',
  uFMDrawUtils in '..\TestBedShared\uFMDrawUtils.pas',
  screenStart in 'screenStart.pas' {StartFrame: TFrame},
  screenGameOver in 'screenGameOver.pas' {GameOverFrame: TFrame},
  screenInstructions in 'screenInstructions.pas' {InstructionsFrame: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGameForm, GameForm);
  Application.Run;
end.
