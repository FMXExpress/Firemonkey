program PassLockDemo;

uses
  FMX.Forms,
  UPassLockDemo in 'UPassLockDemo.pas' {Form15};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm15, Form15);
  Application.Run;
end.
