program Multipeer;

uses
  System.StartUpCopy,
  FMX.Forms,
  UMultipeer in 'UMultipeer.pas' {Form1175};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1175, Form1175);
  Application.Run;
end.
