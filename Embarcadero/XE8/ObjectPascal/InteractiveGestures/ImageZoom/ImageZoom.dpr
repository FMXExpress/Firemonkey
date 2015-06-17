program ImageZoom;

uses
  System.StartUpCopy,
  FMX.Forms,
  ImageZoomU in 'ImageZoomU.pas' {PinchZoom};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TPinchZoom, PinchZoom);
  Application.Run;
end.
