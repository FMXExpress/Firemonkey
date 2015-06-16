program PhoneDialer;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {PhoneDialerForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TPhoneDialerForm, PhoneDialerForm);
  Application.Run;
end.
