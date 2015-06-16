program AnonymousThread;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main_Form in 'Main_Form.pas' {MainForm},
  AnonThread in '..\AnonThread.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
