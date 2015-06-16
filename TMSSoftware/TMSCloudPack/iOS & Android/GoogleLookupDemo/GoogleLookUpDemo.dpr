program GoogleLookUpDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  UGoogleLookUpDemo in 'UGoogleLookUpDemo.pas' {Form82};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm82, Form82);
  Application.Run;
end.
