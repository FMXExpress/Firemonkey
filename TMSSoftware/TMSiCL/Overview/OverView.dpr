program OverView;

uses
  System.StartUpCopy,
  FMX.Forms,
  UOverView in 'UOverView.pas' {Form925};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.soInvertedLandscape];
  Application.CreateForm(TForm925, Form925);
  Application.Run;
end.
