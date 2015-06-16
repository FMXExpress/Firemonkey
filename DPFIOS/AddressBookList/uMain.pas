unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  System.Math,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,

  DPF.iOS.BaseControl,
  DPF.iOS.MFMailComposeViewController,
  DPF.iOS.MFMailCompose,
  DPF.iOS.UIButton,
  DPF.iOS.UITextField,
  DPF.iOS.UITextView,
  DPF.iOS.ABPeoplePicker, DPF.iOS.UIView, FMX.StdCtrls, DPF.iOS.UIProgressView;

type
  TFPeoplePicker = class( TForm )
    DPFAddressBook1: TDPFAddressBook;
    DPFUIView1: TDPFUIView;
    DPFButtonAddressBook: TDPFButton;
    DPFTextView1: TDPFTextView;
    DPFProgress1: TDPFProgress;
    procedure DPFButtonAddressBookClick( Sender: TObject );
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

procedure TFPeoplePicker.DPFAddressBook1RequestAccess( Sender: TObject; const Granted: Boolean );
var
  c: TPersonRecordArray;
  i: integer;
  s: string;
begin
  if Granted then
  begin
    c := DPFAddressBook1.GetAddressBookList;
    s := 'Count : ' + IntToStr( Length( c ) ) + #10#13;

    for I := 0 to high( c ) do
    begin
      DPFProgress1.Progress := i / Length( c );
      Application.ProcessMessages;
      s := s + c[i].FirstName + ' ' + c[i].LastName + ' ';
      if Length( c[i].Phones ) > 0 then
        s := s + c[i].Phones[0];
      s   := s + #10#13;
    end;
    DPFTextView1.Text := s;
  end
  else
    ShowMessage( 'Go to Setting->Privacy->Contacts and switch on this application' );
end;

procedure TFPeoplePicker.DPFButtonAddressBookClick( Sender: TObject );
begin
  DPFAddressBook1.RequestAccess;
end;

end.
