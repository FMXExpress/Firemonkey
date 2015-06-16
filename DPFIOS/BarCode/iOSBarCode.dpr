program iOSBarCode;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {QRCode};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TQRCode, QRCode);
  Application.Run;
end.
