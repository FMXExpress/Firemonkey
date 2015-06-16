program LinkedInDemo;

uses
  FMX.Forms,
  ULinkedInDemo in 'ULinkedInDemo.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
