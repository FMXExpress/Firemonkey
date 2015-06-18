program GradientsFun1;

uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  uFormGradientsFun1 in 'uFormGradientsFun1.pas' {FormGradientsFun1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormGradientsFun1, FormGradientsFun1);
  Application.Run;
end.
