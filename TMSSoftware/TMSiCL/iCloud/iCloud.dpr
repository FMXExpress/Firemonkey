program iCloud;

uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  UiCloud in 'UiCloud.pas' {Form1144};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1144, Form1144);
  Application.Run;
end.
