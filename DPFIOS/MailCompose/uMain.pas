unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,


  DPF.iOS.Common,
  DPF.iOS.BaseControl,
  DPF.iOS.MFMailComposeViewController,
  DPF.iOS.MFMailCompose,
  DPF.iOS.UIButton,
  DPF.iOS.UITextField,
  DPF.iOS.UITextView,
  DPF.iOS.UIView,
  DPF.iOS.UILabel,
  DPF.iOS.UIActionSheet;

type
  TFMailCompose = class( TForm )
    DPFMailCompose1: TDPFMailCompose;
    DPFButtonSendMail: TDPFButton;
    DPFTextFieldSubject: TDPFTextField;
    DPFTextFieldRecipient: TDPFTextField;
    DPFTextViewBody: TDPFTextView;
    DPFUIView1: TDPFUIView;
    DPFLabel1: TDPFLabel;
    procedure DPFButtonSendMailClick( Sender: TObject );
    procedure DPFMailCompose1SendStatus( Sender: TObject; Result: Integer );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FMailCompose: TFMailCompose;

implementation

{$R *.fmx}

procedure TFMailCompose.DPFButtonSendMailClick( Sender: TObject );
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Add( 'This is a sample attachment file content. create by TDPFMailCompose' );
    SL.SaveToFile( GetTempDirectory + 'DPFSampleAttach.txt' );
    if not DPFMailCompose1.MailCompose( DPFTextFieldSubject.Text, DPFTextViewBody.Text, False, [DPFTextFieldRecipient.Text], [], [], [GetTempDirectory + 'DPFSampleAttach.txt'] ) then
      ShowMessage( 'Sorry' + #10#13 + 'You cant send eMail '#10#13 + '1) Your Device not support eMail Compose' + #10#13 + '2) You not have a Mailbox' );
  finally
    SL.Free;
  end;
end;

procedure TFMailCompose.DPFMailCompose1SendStatus( Sender: TObject; Result: Integer );
const
  StatusMessage: array [0 .. 3] of string = ( 'MailCompose Cancelled', 'MailCompose Saved', 'MailCompose Sent', 'MailCompose Failed' );
begin
  DPFLabel1.Text := StatusMessage[Result];
end;

procedure TFMailCompose.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
