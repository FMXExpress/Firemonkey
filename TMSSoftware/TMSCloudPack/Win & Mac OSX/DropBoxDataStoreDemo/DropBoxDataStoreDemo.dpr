program DropBoxDataStoreDemo;

uses
  FMX.Forms,
  UDropBoxDataStoreDemo in 'UDropBoxDataStoreDemo.pas' {Form12};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm12, Form12);
  Application.Run;
end.
