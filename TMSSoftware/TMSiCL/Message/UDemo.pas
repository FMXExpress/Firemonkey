unit UDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TMSNativeUIBaseControl,
  FMX.TMSNativeUIButton, FMX.TMSNativeMFMailComposeViewController,
  FMX.TMSNativeMFMessageComposeViewController;

type
  TForm1033 = class(TForm)
    TMSFMXNativeUIButton1: TTMSFMXNativeUIButton;
    TMSFMXNativeMFMessageComposeViewController1: TTMSFMXNativeMFMessageComposeViewController;
    procedure TMSFMXNativeUIButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1033: TForm1033;

implementation

{$R *.fmx}

procedure TForm1033.TMSFMXNativeUIButton1Click(Sender: TObject);
begin
  TMSFMXNativeMFMessageComposeViewController1.Body := 'This is a sample message';
  TMSFMXNativeMFMessageComposeViewController1.Recipients.Clear;
  TMSFMXNativeMFMessageComposeViewController1.Recipients.Add('helloworld@mail.com');

  if TMSFMXNativeMFMessageComposeViewController1.CanSendText then
    TMSFMXNativeMFMessageComposeViewController1.Show;
end;

end.
