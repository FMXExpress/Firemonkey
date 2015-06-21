program FlappyFiremonkey;

uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  uMenu in 'uMenu.pas' {MenuForm},
  uGame in 'uGame.pas' {GameForm};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.soPortrait];
  Application.CreateForm(TMenuForm, MenuForm);
  Application.CreateForm(TGameForm, GameForm);
  Application.Run;
end.
