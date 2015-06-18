program CalcWheel;

uses
  System.StartUpCopy,
  FMX.Forms,
  UCalcWheel in 'UCalcWheel.pas' {WheelForm};

{$R *.res}

begin
  Application.Initialize;
{$if CompilerVersion >= 27.0}
  Application.FormFactor.Orientations := [TFormOrientation.Portrait, TFormOrientation.InvertedPortrait, TFormOrientation.Landscape, TFormOrientation.InvertedLandscape];
{$else}
  Application.FormFactor.Orientations := [TFormOrientation.soPortrait, TFormOrientation.soInvertedPortrait, TFormOrientation.soLandscape, TFormOrientation.soInvertedLandscape];
{$endif}
  Application.CreateForm(TWheelForm, WheelForm);
  Application.Run;
end.
