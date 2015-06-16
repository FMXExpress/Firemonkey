program iOSMoviePlayer;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FMoviePlayer};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFMoviePlayer, FMoviePlayer);
  Application.Run;
end.
