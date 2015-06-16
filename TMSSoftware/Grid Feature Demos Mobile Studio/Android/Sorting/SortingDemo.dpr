program SortingDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  UDemo in 'UDemo.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.soLandscape];
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
