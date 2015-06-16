program ANDCheckBox;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FCheckBox};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFCheckBox, FCheckBox);
  Application.Run;
end.
