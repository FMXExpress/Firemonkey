program GTasksDemo;

uses
  FMX.Forms,
  UGTasksDemo in 'UGTasksDemo.pas' {Form98};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm98, Form98);
  Application.Run;
end.
