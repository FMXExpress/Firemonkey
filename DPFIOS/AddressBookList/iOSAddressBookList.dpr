Program iOSAddressBookList;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FPeoplePicker};

{$I DPF.iOS.Defs.inc}
{$R *.res}

Begin
  Application.Initialize;
  Application.CreateForm(TFPeoplePicker, FPeoplePicker);
  Application.Run;

End.
