program iOSKeyboard;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FTextField};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFTextField, FTextField);
  Application.Run;
end.
