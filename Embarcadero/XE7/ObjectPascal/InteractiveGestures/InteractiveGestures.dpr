program InteractiveGestures;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form36};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm36, Form36);
  Application.Run;
end.
