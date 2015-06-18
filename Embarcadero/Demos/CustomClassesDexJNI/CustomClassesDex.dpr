program CustomClassesDex;

uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  MainUnit in 'MainUnit.pas' {Form36},
  android.JNI.Base64Coder in 'android.JNI.Base64Coder.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm36, Form36);
  Application.Run;
end.
