program GPlacesTabletDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  UGPlacesTabletDemo in 'UGPlacesTabletDemo.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
