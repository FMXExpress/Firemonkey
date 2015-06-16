program SignatureCaptureDemoMobile;

uses
  System.StartUpCopy,
  FMX.Forms,
  UDemoMobile in 'UDemoMobile.pas' {Form99};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm99, Form99);
  Application.Run;
end.
