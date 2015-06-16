program Demo;

uses
  System.StartUpCopy,
  FMX.Forms,
  UDemo in 'UDemo.pas' {Form1014};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1014, Form1014);
  Application.Run;
end.
