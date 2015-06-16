program PicasaDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  UPicasaDemo in 'UPicasaDemo.pas' {Form82};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.soInvertedLandscape];
  Application.CreateForm(TForm82, Form82);
  Application.Run;
end.
