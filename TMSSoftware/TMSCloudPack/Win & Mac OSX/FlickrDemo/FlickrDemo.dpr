program FlickrDemo;

uses
  FMX.Forms,
  UCloudFlickrDemo in 'UCloudFlickrDemo.pas' {Form816};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm816, Form816);
  Application.Run;
end.
