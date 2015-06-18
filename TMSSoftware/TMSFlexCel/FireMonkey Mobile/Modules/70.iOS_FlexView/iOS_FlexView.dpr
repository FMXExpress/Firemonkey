program iOS_FlexView;



uses
  System.StartUpCopy,
  FMX.Forms,
  UFlexView in 'UFlexView.pas' {FormFlexView};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Portrait, TFormOrientation.InvertedPortrait, TFormOrientation.Landscape, TFormOrientation.InvertedLandscape];
  Application.CreateForm(TFormFlexView, FormFlexView);
  Application.Run;
end.
