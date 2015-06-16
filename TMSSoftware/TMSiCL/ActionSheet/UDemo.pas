unit UDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TMSNativeUIBaseControl,
  FMX.TMSNativeUIButton, FMX.TMSNativeUIActionSheet, iOSApi.UIKit, iOSApi.Foundation;

type
  TForm1035 = class(TForm)
    TMSFMXNativeUIActionSheet1: TTMSFMXNativeUIActionSheet;
    TMSFMXNativeUIButton1: TTMSFMXNativeUIButton;
    procedure TMSFMXNativeUIButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TMSFMXNativeUIActionSheet1DidDismissWithButtonIndex(
      Sender: TObject; AButtonIndex: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1035: TForm1035;

implementation

{$R *.fmx}

procedure TForm1035.FormCreate(Sender: TObject);
begin
  TMSFMXNativeUIActionSheet1.CancelButtonTitle := 'Cancel';
  TMSFMXNativeUIActionSheet1.DestructiveButtonTitle := 'Delete';
  TMSFMXNativeUIActionSheet1.Buttons.Add('Hello World!');
end;

procedure TForm1035.TMSFMXNativeUIActionSheet1DidDismissWithButtonIndex(
  Sender: TObject; AButtonIndex: Integer);
var
  str: String;
  av: UIAlertView;
begin
  str := 'Clicked on ' + TMSFMXNativeUIActionSheet1.ButtonTitleAtIndex(AButtonIndex) + ' button';
  av := TUIAlertView.Wrap(TUIAlertView.Wrap(TUIAlertView.OCClass.alloc).initWithTitle(NSSTR('Alert'), NSSTR(str), nil, NSSTR('OK'), nil));
  av.show;
  av.release;
end;

procedure TForm1035.TMSFMXNativeUIButton1Click(Sender: TObject);
begin
  TMSFMXNativeUIActionSheet1.ShowFromControl(TMSFMXNativeUIButton1);
end;

end.
