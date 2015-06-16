Program CalendarGetEvents;

Uses
//  System.StartUpCopy,
  FMX.Forms,
  uMain In 'uMain.pas' {Form3};

{$I DPF.iOS.Defs.inc}
{$R *.res}

Begin
  Application.Initialize;
  Application.CreateForm( TForm3, Form3 );
  Application.Run;

End.
