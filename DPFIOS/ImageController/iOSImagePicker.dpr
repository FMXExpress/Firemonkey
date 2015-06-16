program iOSImagePicker;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FImageController};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFImageController, FImageController);
  Application.Run;
end.
