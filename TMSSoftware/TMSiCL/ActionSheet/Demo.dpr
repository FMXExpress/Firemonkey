program Demo;

uses
  System.StartUpCopy,
  FMX.Forms,
  UDemo in 'UDemo.pas' {Form1035};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1035, Form1035);
  Application.Run;
end.
