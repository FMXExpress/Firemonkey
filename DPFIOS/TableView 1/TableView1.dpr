Program TableView1;

Uses
  System.StartUpCopy,
  FMX.Forms,
  uMain In 'uMain.pas' {Form3};

{$I DPF.iOS.Defs.inc}
{$R *.res}

Begin
  Application.Initialize;
  Application.CreateForm( TFTableView1, FTableView1 );
  Application.Run;

End.
