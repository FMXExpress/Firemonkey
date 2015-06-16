program ANDVideoView;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FVideoView};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFVideoView, FVideoView);
  Application.Run;
end.
