program MediaPlayerHD;

uses
  System.StartUpCopy,
  FMX.Forms,
  MediaPlayerForm in 'MediaPlayerForm.pas' {Form240};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm240, Form240);
  Application.Run;
end.
