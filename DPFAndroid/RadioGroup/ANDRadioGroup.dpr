program ANDRadioGroup;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FRadioGroup};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFRadioGroup, FRadioGroup);
  Application.Run;
end.
