program FlappyFiremonkey;

uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  uMenu in 'uMenu.pas' {MenuForm},
  uGame in 'uGame.pas' {GameForm},
  uGame3D in 'uGame3D.pas' {GameForm3D};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.soPortrait];
  Application.CreateForm(TMenuForm, MenuForm);
  Application.CreateForm(TGameForm, GameForm);
  Application.CreateForm(TGameForm3D, GameForm3D);
  Application.Run;
end.
