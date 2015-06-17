program WebOSMapsDemo;

uses
  FMX.Forms,
  UWebOSMapsDemo in 'UWebOSMapsDemo.pas' {Form90};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm90, Form90);
  Application.Run;
end.
