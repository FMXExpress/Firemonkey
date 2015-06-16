program Demo;

uses
  System.StartUpCopy,
  FMX.Forms,
  UDemo in 'UDemo.pas' {Form1033};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1033, Form1033);
  Application.Run;
end.
