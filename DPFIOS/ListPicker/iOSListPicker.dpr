program iOSListPicker;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FListPicker};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFListPicker, FListPicker);
  Application.Run;
end.
