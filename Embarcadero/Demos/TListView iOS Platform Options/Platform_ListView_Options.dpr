program Platform_ListView_Options;

uses
  System.StartUpCopy,
  FMX.Forms,
  PlatformListViewOptionsMain in 'PlatformListViewOptionsMain.pas' {Form24};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm24, Form24);
  Application.Run;
end.
