unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,

  DPF.iOS.BaseControl,
  DPF.iOS.MFMailComposeViewController,
  DPF.iOS.MFMailCompose,
  DPF.iOS.UIButton,
  DPF.iOS.UITextField,
  DPF.iOS.UITextView,
  DPF.iOS.ABPeoplePicker;

type
  TFPeoplePicker = class( TForm )
    DPFButtonAddressBook: TDPFButton;
    DPFAddressBook1: TDPFAddressBook;
    procedure DPFButtonAddressBookClick( Sender: TObject );
    procedure DPFAddressBook1PersonSelected( Sender: TObject; Person: TPersonRecord; var ShowUserDetail: Boolean );
    procedure DPFAddressBook1PersonSelectedDetail( Sender: TObject; Person: TPersonRecord; SelectedPropertyName: string; var CloseAddressBook: Boolean );
    procedure DPFAddressBook1PersonSelectedCancel( Sender: TObject; var CloseAddressBook: Boolean );
    procedure DPFAddressBook1RequestAccess( Sender: TObject; const Granted: Boolean );
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FPeoplePicker: TFPeoplePicker;

implementation

{$R *.fmx}

procedure TFPeoplePicker.DPFAddressBook1PersonSelected( Sender: TObject; Person: TPersonRecord; var ShowUserDetail: Boolean );
begin
  ShowMessage( Person.FirstName + ' ' + Person.LastName );
  ShowUserDetail := MessageDlg( 'Show User Detail Info ?', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0 ) = mrYes
end;

procedure TFPeoplePicker.DPFAddressBook1PersonSelectedCancel( Sender: TObject; var CloseAddressBook: Boolean );
begin
  CloseAddressBook := MessageDlg( 'Are you sure to close AB ?', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0 ) = mrYes
end;

procedure TFPeoplePicker.DPFAddressBook1PersonSelectedDetail( Sender: TObject; Person: TPersonRecord; SelectedPropertyName: string; var CloseAddressBook: Boolean );
begin
  ShowMessage( 'Selected Property: ' + SelectedPropertyName );
  CloseAddressBook := MessageDlg( 'Do you want to close AB ?', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0 ) = mrYes
end;

procedure TFPeoplePicker.DPFAddressBook1RequestAccess( Sender: TObject; const Granted: Boolean );
var
  p: array [0 .. 2] of TPhoneNumbers;
begin
  P[0].PhoneNumber := '+98912xxxxx';
  P[0].PhoneLabel  := 'mobile';

  P[1].PhoneNumber := '+982188xxxxx';
  P[1].PhoneLabel  := 'work';

  P[2].PhoneNumber := '+982188xxxxx';
  P[2].PhoneLabel  := ''; // <-- contact’s primary number

  if Granted then
    DPFAddressBook1.AddAddressBook( 'Babak', 'Yaghoobi', p )
  else
    ShowMessage( 'Go to Setting->Privacy->Contacts and switch on this application' );
end;

procedure TFPeoplePicker.DPFButtonAddressBookClick( Sender: TObject );
begin
  DPFAddressBook1.RequestAccess;
end;

end.
