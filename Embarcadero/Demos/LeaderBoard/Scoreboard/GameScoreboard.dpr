program GameScoreboard;

uses
  FMX.Forms,
  ScoreboardMain in 'ScoreboardMain.pas' {Form6},
  unitGamePlayer in '..\Client\unitGamePlayer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm6, Form6);
  Application.Run;
end.
