program WebGMapsDemo;

uses
  FMX.Forms,
  UWebGMapsDemo in 'UWebGMapsDemo.pas' {Form90};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm90, Form90);
  Application.Run;
end.
