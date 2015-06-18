program RemotePush1;

uses
  System.StartUpCopy,
  FMX.Forms,
  Remotepush in 'Remotepush.pas' {Form30};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm30, Form30);
  Application.Run;
end.
