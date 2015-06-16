program AudioRecPlay;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {AudioRecPlayForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TAudioRecPlayForm, AudioRecPlayForm);
  Application.Run;
end.
