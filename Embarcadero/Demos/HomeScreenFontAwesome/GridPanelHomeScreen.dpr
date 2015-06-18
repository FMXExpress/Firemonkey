program GridPanelHomeScreen;

uses
  System.StartUpCopy,
  FMX.Forms,
  HomeScreenNavigation_GridPanel in 'HomeScreenNavigation_GridPanel.pas' {Form20},
  FMX.FontGlyphs.Android in 'FMX.FontGlyphs.Android.pas',
  FontAwesomeCodes in 'FontAwesomeCodes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm20, Form20);
  Application.Run;
end.
