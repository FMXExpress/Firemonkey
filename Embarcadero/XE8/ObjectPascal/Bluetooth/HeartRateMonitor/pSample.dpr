program pSample;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainSample in 'uMainSample.pas' {Form4},
  uBluetoothComponent in 'uBluetoothComponent.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
