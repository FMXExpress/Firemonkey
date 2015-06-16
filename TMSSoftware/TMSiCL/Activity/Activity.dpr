program Activity;

uses
  System.StartUpCopy,
  FMX.Forms,
  UActivity in 'UActivity.pas' {Form1073};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1073, Form1073);
  Application.Run;
end.
