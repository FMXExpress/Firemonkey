program ANDEditText;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FEditText};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFEditText, FEditText);
  Application.Run;
end.
