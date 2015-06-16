program iCloudDocument;

uses
  System.StartUpCopy,
  FMX.Forms,
  UiCloudDocument in 'UiCloudDocument.pas' {Form1144};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1144, Form1144);
  Application.Run;
end.
