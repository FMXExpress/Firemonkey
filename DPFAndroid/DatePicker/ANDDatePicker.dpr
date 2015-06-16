program ANDDatePicker;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FDatePicker};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFDatePicker, FDatePicker);
  Application.Run;
end.
