program ANDSeekBar;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FProgressBar};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFProgressBar, FProgressBar);
  Application.Run;
end.
