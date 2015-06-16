program Demo;

uses
  System.StartUpCopy,
  FMX.Forms,
  UDemo in 'UDemo.pas' {Form1024};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1024, Form1024);
  Application.Run;
end.
