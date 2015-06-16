program ANDRadioButton;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FButton};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFButton, FButton);
  Application.Run;
end.
