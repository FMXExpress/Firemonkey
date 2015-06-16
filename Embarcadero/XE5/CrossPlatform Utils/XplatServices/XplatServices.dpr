program XplatServices;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main_Form in 'Main_Form.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
