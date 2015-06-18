program Project8;

uses
  FMX.Forms,
  Unit2 in 'Unit2.pas' {Form2},
  HTMLHelpFMXViewer in 'HTMLHelpFMXViewer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
