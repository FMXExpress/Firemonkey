program GridFilterTest;

uses
  System.StartUpCopy,
  FMX.Forms,
  UGridFilter in 'UGridFilter.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
