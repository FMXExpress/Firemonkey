program prAppList;

uses
  System.StartUpCopy,
  FMX.Forms,
  unitMain in 'unitMain.pas' {formMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TformMain, formMain);
  Application.Run;
end.
