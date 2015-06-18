program ProjectMultiTouchDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitTest in 'UnitTest.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
