program FlexCalc;

uses
  System.StartUpCopy,
  FMX.Forms,
  UFlexCalc in 'UFlexCalc.pas' {FFlexCalc};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFFlexCalc, FFlexCalc);
  Application.Run;
end.
