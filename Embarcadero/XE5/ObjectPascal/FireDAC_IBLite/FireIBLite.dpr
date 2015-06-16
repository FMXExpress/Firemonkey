program FireIBLite;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {IBLiteForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TIBLiteForm, IBLiteForm);
  Application.Run;
end.
