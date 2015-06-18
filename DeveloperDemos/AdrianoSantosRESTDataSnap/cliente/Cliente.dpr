program Cliente;

uses
  System.StartUpCopy,
  FMX.Forms,
  UntMain in 'UntMain.pas' {Form2},
  Proxy in 'Proxy.pas',
  ClientModuleUnit1 in 'ClientModuleUnit1.pas' {ClientModule: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TClientModule, ClientModule);
  Application.Run;
end.
