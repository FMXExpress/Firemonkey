program CheckRadioGroupDemo;

uses
  FMX.Forms,
  UCheckRadioGroupDemo in 'UCheckRadioGroupDemo.pas' {Form3};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
