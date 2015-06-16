program YoutubeDemo;

uses
  FMX.Forms,
  UYoutubeDemo in 'UYoutubeDemo.pas' {Form1171};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1171, Form1171);
  Application.Run;
end.
