program TaskDialogDemo;

uses
  FMX.Forms,
  UTaskDialogDemo in 'UTaskDialogDemo.pas' {Form4};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
