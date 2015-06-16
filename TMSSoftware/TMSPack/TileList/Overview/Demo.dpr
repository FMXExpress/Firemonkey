program Demo;

uses
  FMX.Forms, FMX.Types,
  UDemo in 'UDemo.pas' {Form633};

{$R *.res}

begin
  {$IFNDEF IOS}
  GlobalUseDirect2d := False;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TForm633, Form633);
  Application.Run;
end.
