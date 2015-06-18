program CustomPicker;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {CustomPickerForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TCustomPickerForm, CustomPickerForm);
  Application.Run;
end.
