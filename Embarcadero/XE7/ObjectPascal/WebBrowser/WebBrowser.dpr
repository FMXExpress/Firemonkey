program WebBrowser;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {WebBrowserForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TWebBrowserForm, WebBrowserForm);
  Application.Run;
end.
