program ANDJDatePickerDialog;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FTimePickerDialog};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFTimePickerDialog, FTimePickerDialog);
  Application.Run;
end.
