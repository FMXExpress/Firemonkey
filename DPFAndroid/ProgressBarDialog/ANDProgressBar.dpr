program ANDProgressBar;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FProgressDialog};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFProgressDialog, FProgressDialog);
  Application.Run;
end.
