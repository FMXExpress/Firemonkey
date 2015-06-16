program FaceBookAlbumDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  UFaceBookAlbumDemo in 'UFaceBookAlbumDemo.pas' {Form82};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.soLandscape, TFormOrientation.soInvertedLandscape];
  Application.CreateForm(TForm82, Form82);
  Application.Run;
end.
