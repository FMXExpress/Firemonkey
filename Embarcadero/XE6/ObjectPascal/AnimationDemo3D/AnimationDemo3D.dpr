program AnimationDemo3D;

uses
  FMX.Forms,
  anidemofrm in 'anidemofrm.pas' {frmAniDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmAniDemo, frmAniDemo);
  Application.Run;
end.
