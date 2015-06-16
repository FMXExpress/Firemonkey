unit UDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TMSNativeUIBaseControl,
  FMX.TMSNativeUIButton, FMX.TMSNativeMFMailComposeViewController;

type
  TForm1033 = class(TForm)
    TMSFMXNativeMFMailComposeViewController1: TTMSFMXNativeMFMailComposeViewController;
    TMSFMXNativeUIButton1: TTMSFMXNativeUIButton;
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
  TMSFMXNativeMFMailComposeViewController1.Subject := 'Hello World';
  TMSFMXNativeMFMailComposeViewController1.Body := 'This is a sample mail';
  TMSFMXNativeMFMailComposeViewController1.ToRecipients.Clear;
  TMSFMXNativeMFMailComposeViewController1.ToRecipients.Add('helloworld@mail.com');
  TMSFMXNativeMFMailComposeViewController1.Attachments.Clear;
  TMSFMXNativeMFMailComposeViewController1.Attachments.Add(ExtractFilePath(ParamStr(0)) + '/TMS iCL.pdf');
  TMSFMXNativeMFMailComposeViewController1.Attachments.Add(ExtractFilePath(ParamStr(0)) + '/Tulips.jpg');
  TMSFMXNativeMFMailComposeViewController1.Attachments.Add(ExtractFilePath(ParamStr(0)) + '/cars.csv');

  if TMSFMXNativeMFMailComposeViewController1.CanSendMail then
    TMSFMXNativeMFMailComposeViewController1.Show;
end;

end.
