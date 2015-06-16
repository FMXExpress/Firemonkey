program iOSNotification;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FNotification};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFNotification, FNotification);
  Application.CreateForm(TFNotification, FNotification);
  Application.Run;
end.
