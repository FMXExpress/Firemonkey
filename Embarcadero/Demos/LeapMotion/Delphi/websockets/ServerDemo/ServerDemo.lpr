program ServerDemo;

{$mode DELPHI}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainFormU, LResources
  { you can add units after this };

{$IFDEF WINDOWS}{$R ServerDemo.rc}{$ENDIF}

begin
  {$I ServerDemo.lrs}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

