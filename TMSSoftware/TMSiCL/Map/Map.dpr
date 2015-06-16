program Map;

uses
  System.StartUpCopy,
  FMX.Forms,
  UMap in 'UMap.pas' {Form913};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.soLandscape, TFormOrientation.soInvertedLandscape];
  Application.CreateForm(TForm913, Form913);
  Application.Run;
end.
