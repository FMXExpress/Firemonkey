program TabSliding;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {TabSlidingForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTabSlidingForm, TabSlidingForm);
  Application.Run;
end.
