program CustomAdapter;

uses
  {$IFDEF WINDOWS}
  FastMM4,
  {$ENDIF }
  System.StartUpCopy,
  FMX.Forms,
  Customnik in 'Customnik.pas' {Form3};

{$APPTYPE CONSOLE}
{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
