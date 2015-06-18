program ListBox01;

uses
  System.StartUpCopy,
  FMX.Forms,
  uFrmListBox01 in 'uFrmListBox01.pas' {Form9},
  uFrmListBox02pas in 'uFrmListBox02pas.pas' {Form1},
  Unit3 in 'Unit3.pas' {Form3};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm3, Form3);
  Application.CreateForm(TForm9, Form9);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
