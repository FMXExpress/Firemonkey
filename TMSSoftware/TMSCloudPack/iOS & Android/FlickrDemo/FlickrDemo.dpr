program FlickrDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  UFlickrDemo in 'UFlickrDemo.pas' {Form82};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.soLandscape];
  Application.CreateForm(TForm82, Form82);
  Application.Run;
end.
