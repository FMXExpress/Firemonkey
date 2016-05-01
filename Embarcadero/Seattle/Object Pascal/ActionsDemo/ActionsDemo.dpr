program ActionsDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  UMainForm in 'UMainForm.pas' {MainForm},
  UActiveForm in 'UActiveForm.pas' {ActiveForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TMainForm, MainForm);
  Application.RegisterFormFamily('TForm', [TMainForm, TActiveForm]);
  Application.Run;
end.
