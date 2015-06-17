program Demo;

uses
  System.StartUpCopy,
  FMX.Forms,
  UDemo in 'UDemo.pas' {Form1166};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1166, Form1166);
  Application.Run;
end.
