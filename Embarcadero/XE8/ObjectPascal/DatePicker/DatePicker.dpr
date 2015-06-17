program DatePicker;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {DatePickerForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDatePickerForm, DatePickerForm);
  Application.Run;
end.
