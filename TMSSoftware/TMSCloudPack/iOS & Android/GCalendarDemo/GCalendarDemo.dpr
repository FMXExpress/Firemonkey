program GCalendarDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  UGCalendarDemo in 'UGCalendarDemo.pas' {Form82};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm82, Form82);
  Application.Run;
end.
