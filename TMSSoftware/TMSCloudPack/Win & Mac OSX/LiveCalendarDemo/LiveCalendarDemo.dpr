program LiveCalendarDemo;

uses
  FMX.Forms,
  ULiveCalendarDemo in 'ULiveCalendarDemo.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
