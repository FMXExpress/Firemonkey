program WebOSMapsDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  UWebOSMapsDemo in 'UWebOSMapsDemo.pas' {Form65};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.soLandscape, TFormOrientation.soInvertedLandscape];
  Application.CreateForm(TForm65, Form65);
  Application.Run;
end.
