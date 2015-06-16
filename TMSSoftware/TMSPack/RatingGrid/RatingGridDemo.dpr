program RatingGridDemo;

uses
  FMX.Forms,
  URatingGrid in 'URatingGrid.pas' {Form11};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm11, Form11);
  Application.Run;
end.
