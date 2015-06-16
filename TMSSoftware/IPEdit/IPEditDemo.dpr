program IPEditDemo;

uses
  FMX.Forms,
  UIPEditDemo in 'UIPEditDemo.pas' {Form7};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm7, Form7);
  Application.Run;
end.
