program PryvDemo;

uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  UPryvDemo in 'UPryvDemo.pas' {Form44};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm44, Form44);
  Application.Run;
end.
