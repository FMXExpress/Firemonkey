program SignatureCaptureDemo;

uses
  FMX.Forms,
  UDemo in 'UDemo.pas' {Form98};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm98, Form98);
  Application.Run;
end.
