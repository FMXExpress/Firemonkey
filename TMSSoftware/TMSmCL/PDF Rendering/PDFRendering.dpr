program PDFRendering;

uses
  FMX.Forms,
  UPDFRendering in 'UPDFRendering.pas' {Form1147};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1147, Form1147);
  Application.Run;
end.
