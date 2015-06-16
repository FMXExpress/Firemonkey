program Demo;

uses
  System.StartUpCopy,
  FMX.Forms,
  UDemo in 'UDemo.pas' {Form1027};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1027, Form1027);
  Application.Run;
end.
