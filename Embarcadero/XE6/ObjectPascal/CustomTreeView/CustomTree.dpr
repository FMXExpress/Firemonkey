program CustomTree;

uses
  FMX.Forms,
  CustomTreeFrm in 'CustomTreeFrm.pas' {CustomTreeViewFrm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TCustomTreeViewFrm, CustomTreeViewFrm);
  Application.RegisterFormFamily('TForm', [TCustomTreeViewFrm]);
  Application.Run;
end.
