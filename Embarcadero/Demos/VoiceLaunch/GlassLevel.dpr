program GlassLevel;

uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  GlassVoiceLaunchUnit1 in 'GlassVoiceLaunchUnit1.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
