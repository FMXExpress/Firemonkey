program customcontrol;

{$R 'fmx.customcontrol.win.res' 'fmx.customcontrol.win.rc'}

uses
  System.StartUpCopy,
  FMX.Forms,
  customcontrolfrm in 'customcontrolfrm.pas' {Form6},
  fmx.customcontrol in 'fmx.customcontrol.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm6, Form6);
  Application.Run;
end.
