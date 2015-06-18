program FlexView;

uses
  System.StartUpCopy,
  FMX.Forms,
  UFlexView in 'UFlexView.pas' {FormFlexView},
  UPatchMissingOpenURLEvent in 'UPatchMissingOpenURLEvent.pas';

{$R *.res}

begin
  Application.Initialize;
{$if CompilerVersion >= 27.0}
  Application.FormFactor.Orientations := [TFormOrientation.Portrait, TFormOrientation.InvertedPortrait, TFormOrientation.Landscape, TFormOrientation.InvertedLandscape];
{$else}
  Application.FormFactor.Orientations := [TFormOrientation.soPortrait, TFormOrientation.soInvertedPortrait, TFormOrientation.soLandscape, TFormOrientation.soInvertedLandscape];
{$endif}
  Application.CreateForm(TFormFlexView, FormFlexView);
  Application.Run;
end.
