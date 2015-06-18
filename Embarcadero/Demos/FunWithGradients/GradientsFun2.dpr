program GradientsFun2;

uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  uFormGradientsFun2 in 'uFormGradientsFun2.pas' {FormGradientsFun2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormGradientsFun2, FormGradientsFun2);
  Application.Run;
end.
