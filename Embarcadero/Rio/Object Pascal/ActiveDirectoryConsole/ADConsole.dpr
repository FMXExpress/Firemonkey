program ADConsole;

uses
  System.StartUpCopy,
  FMX.Forms,
  ADLoginClientU in 'ADLoginClientU.pas' {EMSClientForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TEMSClientForm, EMSClientForm);
  Application.Run;
end.
