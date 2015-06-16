unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, DPF.iOS.MFMessageCompose,
  DPF.iOS.UITextView, DPF.iOS.UITextField, DPF.iOS.BaseControl, DPF.iOS.UIButton,
  DPF.iOS.UIView;

type
  TFMessageCompose = class( TForm )
    DPFMessageCompose1: TDPFMessageCompose;
    DPFTextFieldRecipient: TDPFTextField;
    DPFTextViewBody: TDPFTextView;
    DPFButtonSendTextMessage: TDPFButton;
    DPFUIView1: TDPFUIView;
    procedure DPFButtonSendMessageClick( Sender: TObject );
    procedure DPFMessageCompose1SendStatus( Sender: TObject; Result: Integer );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FMessageCompose: TFMessageCompose;

implementation

{$R *.fmx}

procedure TFMessageCompose.DPFButtonSendMessageClick( Sender: TObject );
begin
  if not DPFMessageCompose1.MessageCompose( DPFTextViewBody.Text, [DPFTextFieldRecipient.Text] ) then
    ShowMessage( 'Sorry' + #10#13 + 'You cant send Message '#10#13 + '1) Your Device not support Message Compose' + #10#13 + '2) Text Messaging not available' );
end;

procedure TFMessageCompose.DPFMessageCompose1SendStatus( Sender: TObject; Result: Integer );
const
  StatusMessage: array [0 .. 3] of string = ( 'MessageCompose Cancelled', 'MessageCompose Saved', 'MessageCompose Sent', 'MessageCompose Failed' );
begin
  ShowMessage( StatusMessage[Result] );
end;

procedure TFMessageCompose.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
