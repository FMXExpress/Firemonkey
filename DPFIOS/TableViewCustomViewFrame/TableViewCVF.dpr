program TableViewCVF;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FTableViewCVF},
  uCustomForm in 'uCustomForm.pas' {Form1},
  uFrame in 'uFrame.pas' {Frame1: TFrame};

{$I DPF.iOS.Defs.inc}
{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFTableViewCVF, FTableViewCVF);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
