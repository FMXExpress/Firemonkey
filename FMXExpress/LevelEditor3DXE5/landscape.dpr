program landscape;

uses
  FMX.Forms,
  landscapeMainForm in 'landscapeMainForm.pas' {Main: TForm3D};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  try
    Application.Run;
  except

  end;
end.
