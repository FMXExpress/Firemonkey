program FmxJabber;

uses
  System.StartUpCopy,
  FMX.Forms,
  frmConnect in 'frmConnect.pas' {FormConnect},
  frmContacts in 'frmContacts.pas' {FormContacts},
  CommonHeader in 'CommonHeader.pas',
  frmConversation in 'frmConversation.pas' {FrameConversation: TFrame},
  frmAddContact in 'frmAddContact.pas' {FormAddContact},
  ConversationListComponent in 'ConversationListComponent.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormConnect, FormConnect);
  Application.CreateForm(TFormContacts, FormContacts);
  Application.CreateForm(TFormAddContact, FormAddContact);
  Application.Run;
end.
