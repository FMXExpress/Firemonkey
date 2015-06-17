program Model3D;

uses
  System.StartUpCopy,
  FMX.Forms,
  FrameMaterial3DDesigner in 'FrameMaterial3DDesigner.pas' {FrameMaterialDesigner: TFrame},
  Model3D_U in 'Model3D_U.pas' {Model3DTest};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TModel3DTest, Model3DTest);
  Application.Run;
end.
