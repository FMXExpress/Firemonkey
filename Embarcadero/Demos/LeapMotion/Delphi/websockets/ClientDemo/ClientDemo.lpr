program ClientDemo;

{$mode delphi}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainFormU, laz_synapse, LResources
  { you can add units after this };

{$IFDEF WINDOWS}{$R ClientDemo.rc}{$ENDIF}

begin
  {$I ClientDemo.lrs}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

