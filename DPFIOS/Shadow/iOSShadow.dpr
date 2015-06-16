program iOSShadow;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FLabels};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFLabels, FLabels);
  Application.Run;
end.
