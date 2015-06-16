program UserIniFile;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main_Form in 'Main_Form.pas' {MainForm},
  Apple.Inifiles in '..\Apple.Inifiles.pas',
  Apple.Utils in '..\Apple.Utils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
