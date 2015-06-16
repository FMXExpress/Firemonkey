program iOSImageView;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FImageView};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFImageView, FImageView);
  Application.Run;
end.
