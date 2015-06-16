program CloudStorageDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  UCloudStorageDemo in 'UCloudStorageDemo.pas' {Form82};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm82, Form82);
  Application.Run;
end.
