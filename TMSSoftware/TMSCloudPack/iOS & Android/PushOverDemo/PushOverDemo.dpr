program PushOverDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  UPushOverDemo in 'UPushOverDemo.pas' {Form82};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm82, Form82);
  Application.Run;
end.
