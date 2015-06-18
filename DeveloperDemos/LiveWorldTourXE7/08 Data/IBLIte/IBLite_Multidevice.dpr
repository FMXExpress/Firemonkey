program IBLite_Multidevice;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainForm in 'uMainForm.pas' {Form1},
  InterBaseDM in 'InterBaseDM.pas' {dmInterBase: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TdmInterBase, dmInterBase);
  Application.Run;
end.
