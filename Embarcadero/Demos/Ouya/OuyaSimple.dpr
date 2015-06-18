program OuyaSimple;

uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  uSimpleGame in 'uSimpleGame.pas' {Form5};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm5, Form5);
  Application.Run;
end.
