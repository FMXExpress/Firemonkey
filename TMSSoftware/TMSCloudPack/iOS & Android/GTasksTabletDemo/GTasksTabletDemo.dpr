program GTasksTabletDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  UGTasksTabletDemo in 'UGTasksTabletDemo.pas' {Form98};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm98, Form98);
  Application.Run;
end.
