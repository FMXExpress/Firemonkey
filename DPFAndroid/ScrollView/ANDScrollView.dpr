program ANDScrollView;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FScrollView};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFScrollView, FScrollView);
  Application.Run;
end.
