program EditingDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  UDemo in 'UDemo.pas' {Form719};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm719, Form719);
  Application.Run;
end.
