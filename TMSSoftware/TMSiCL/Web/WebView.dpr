program WebView;

uses
  System.StartUpCopy,
  FMX.Forms,
  UWebView in 'UWebView.pas' {Form1073};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1073, Form1073);
  Application.Run;
end.
