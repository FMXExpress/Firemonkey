program HorizontalScroll;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {HorizontalScrollForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(THorizontalScrollForm, HorizontalScrollForm);
  Application.Run;
end.
