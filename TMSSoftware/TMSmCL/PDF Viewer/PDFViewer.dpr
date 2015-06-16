program PDFViewer;

uses
  FMX.Forms,
  UPDFViewer in 'UPDFViewer.pas' {Form1087};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1087, Form1087);
  Application.Run;
end.
