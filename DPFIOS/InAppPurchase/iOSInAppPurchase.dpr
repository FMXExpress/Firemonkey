program iOSInAppPurchase;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FInAppPurchase},
  uRowFrame in 'uRowFrame.pas' {FrameRow: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFInAppPurchase, FInAppPurchase);
  Application.Run;
end.
