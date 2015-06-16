program PDFExport;

uses
  System.StartUpCopy,
  FMX.Forms,
  UDemo in 'UDemo.pas' {Form25};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm25, Form25);
  Application.Run;
end.
