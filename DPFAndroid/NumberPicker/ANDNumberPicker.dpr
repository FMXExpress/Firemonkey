program ANDNumberPicker;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFNumberPicker, FNumberPicker);
  Application.Run;
end.
