program GTasksPhoneDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  UGTasksPhoneDemo in 'UGTasksPhoneDemo.pas' {Form98};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm98, Form98);
  Application.Run;
end.
