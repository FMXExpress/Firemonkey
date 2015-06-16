program CustomCells;

uses
  System.StartUpCopy,
  FMX.Forms,
  UCustomCells in 'UCustomCells.pas' {Form907};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm907, Form907);
  Application.Run;
end.
