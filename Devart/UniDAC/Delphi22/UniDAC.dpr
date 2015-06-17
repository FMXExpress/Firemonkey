program UniDAC;

uses
  System.StartUpCopy,
  FMX.Forms,
  u_main in '..\u_main.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
