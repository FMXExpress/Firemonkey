program iOSMailCompose;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FMailCompose};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm( TFMailCompose, FMailCompose );
  Application.Run;

end.
