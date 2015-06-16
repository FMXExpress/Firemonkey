unit UWebView;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TMSNativeUIButton,
  FMX.TMSNativeUITextField, FMX.TMSNativeUIView, FMX.TMSNativeUIWebView,
  FMX.TMSNativeUIBaseControl, FMX.TMSNativeUIToolBar;

type
  TForm1073 = class(TForm)
    TMSFMXNativeUIToolBar1: TTMSFMXNativeUIToolBar;
    TMSFMXNativeUIWebView1: TTMSFMXNativeUIWebView;
    TMSFMXNativeUITextField1: TTMSFMXNativeUITextField;
    TMSFMXNativeUIView1: TTMSFMXNativeUIView;
    procedure FormCreate(Sender: TObject);
    procedure TMSFMXNativeUIToolBar1ItemClick(ASender: TObject;
      AItem: TTMSFMXNativeUIToolBarItem);
    procedure TMSFMXNativeUIWebView1DidFinishLoad(Sender: TObject);
    procedure TMSFMXNativeUITextField1DidEndEditing(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure UpdateButtons;
  end;

var
  Form1073: TForm1073;

implementation

{$R *.fmx}

procedure TForm1073.FormCreate(Sender: TObject);
begin
  TMSFMXNativeUIWebView1.Navigate('http://www.google.com');
  UpdateButtons;
end;

procedure TForm1073.TMSFMXNativeUITextField1DidEndEditing(Sender: TObject);
begin
  TMSFMXNativeUIWebView1.Navigate(TMSFMXNativeUITextField1.Text);
  TMSFMXNativeUITextField1.TextField.resignFirstResponder;
end;

procedure TForm1073.TMSFMXNativeUIToolBar1ItemClick(ASender: TObject;
  AItem: TTMSFMXNativeUIToolBarItem);
begin
  case AItem.Index of
    1:
    begin
      TMSFMXNativeUIWebView1.Navigate(TMSFMXNativeUITextField1.Text);
      TMSFMXNativeUITextField1.TextField.resignFirstResponder;
    end;
    2: TMSFMXNativeUIWebView1.GoBack;
    3: TMSFMXNativeUIWebView1.GoForward;
    4: TMSFMXNativeUIWebView1.LoadHTMLString(
'<!DOCTYPE html>'+
'<html>'+
'<title>TTMSFMXNativeUIWebView Demo</title>'+
'<body>'+

'<form action="javascript:alert(fName.value + '' '' + lName.value);">'+
'First name: <input id="fName" type="text" name="FirstName" value="Hello"><br>'+
'Last name: <input id="lName" type="text" name="LastName" value="World"><br>'+
'<input type="submit" value="Submit">'+
'</form>'+
'</body>'+
'</html>');
  5: TMSFMXNativeUIWebView1.ExecuteJavaScript('alert("Hello World");');
  6: TMSFMXNativeUIWebView1.LoadFile(ExtractFilePath(ParamStr(0)) + 'sample.pdf');
  end;
end;

procedure TForm1073.TMSFMXNativeUIWebView1DidFinishLoad(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TForm1073.UpdateButtons;
begin
  TMSFMXNativeUIToolBar1.Items[2].Item.setEnabled(TMSFMXNativeUIWebView1.CanGoBack);
  TMSFMXNativeUIToolBar1.Items[3].Item.setEnabled(TMSFMXNativeUIWebView1.CanGoForward);
end;

end.
