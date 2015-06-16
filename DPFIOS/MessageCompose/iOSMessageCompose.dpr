program iOSMessageCompose;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FMessageCompose};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFMessageCompose, FMessageCompose);
  Application.Run;
end.
