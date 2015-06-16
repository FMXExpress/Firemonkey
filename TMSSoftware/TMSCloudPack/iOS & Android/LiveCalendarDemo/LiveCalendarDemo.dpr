program LiveCalendarDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  ULiveCalendarDemo in 'ULiveCalendarDemo.pas' {Form82};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm82, Form82);
  Application.Run;
end.
