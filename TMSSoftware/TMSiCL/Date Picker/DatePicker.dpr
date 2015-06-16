program DatePicker;

uses
  System.StartUpCopy,
  FMX.Forms,
  UDatePicker in 'UDatePicker.pas' {Form935};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm935, Form935);
  Application.Run;
end.
