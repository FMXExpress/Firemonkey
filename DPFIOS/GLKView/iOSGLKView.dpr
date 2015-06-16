program iOSGLKView;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FGLKView};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFGLKView, FGLKView);
  Application.Run;
end.
