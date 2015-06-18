program ListView;

uses
  System.StartUpCopy,
  FMX.Forms,
  uFrmListView in 'uFrmListView.pas' {Form9},
  Unit2 in 'Unit2.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TForm9, Form9);
  Application.Run;
end.
