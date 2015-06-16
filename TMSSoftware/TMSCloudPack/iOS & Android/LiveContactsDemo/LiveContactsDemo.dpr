program LiveContactsDemo;

uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  ULiveContactsDemo in 'ULiveContactsDemo.pas' {Form82};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm82, Form82);
  Application.Run;
end.
