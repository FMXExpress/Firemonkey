program FMXBarcodeDemo;

uses
  FMX.Forms,
  UBarcodeDemo in 'UBarcodeDemo.pas' {Form9};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm9, Form9);
  Application.Run;
end.
