program BlueChat;

uses
  System.StartUpCopy,
  FMX.Forms,
  uBlueChat in 'uBlueChat.pas' {FrmMainChatForm},
  uPairDevices in 'uPairDevices.pas' {FrmPairdevices},
  uChatManager in 'uChatManager.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmMainChatForm, FrmMainChatForm);
  Application.CreateForm(TFrmPairdevices, FrmPairdevices);
  Application.Run;
end.
