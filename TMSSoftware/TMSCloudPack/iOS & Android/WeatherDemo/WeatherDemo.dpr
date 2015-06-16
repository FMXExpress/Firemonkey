program WeatherDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  UWeatherDemo in 'UWeatherDemo.pas' {Form82};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm82, Form82);
  Application.Run;
end.
