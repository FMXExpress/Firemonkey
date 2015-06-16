program ANDListView;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FListView};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFListView, FListView);
  Application.Run;
end.
