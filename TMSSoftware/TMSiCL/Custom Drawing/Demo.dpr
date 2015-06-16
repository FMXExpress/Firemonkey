program Demo;

uses
  System.StartUpCopy,
  FMX.Forms,
  UDemo in 'UDemo.pas' {Form1034};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1034, Form1034);
  Application.Run;
end.
