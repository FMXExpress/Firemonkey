program iOSCollection;

uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  uMain in 'uMain.pas' {FCollection};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFCollection, FCollection);
  Application.Run;
end.
