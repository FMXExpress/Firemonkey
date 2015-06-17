unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Edit,
  FMX.WebBrowser, FMX.Layouts, FMX.Controls.Presentation;

type
  TWebBrowserForm = class(TForm)
    WebBrowser1: TWebBrowser;
    btnGO: TButton;
    btnBack: TButton;
    btnForward: TButton;
    ToolBar1: TToolBar;
    StatusBar1: TStatusBar;
    edtURL: TClearingEdit;
    procedure btnGOClick(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure btnForwardClick(Sender: TObject);
    procedure edtURLKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WebBrowserForm: TWebBrowserForm;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

{Android: The required permissions have been set under Project-Options}

procedure TWebBrowserForm.btnGOClick(Sender: TObject);
begin
  { Passing the URL entered in the edit-box to the Web Browser component. }
  WebBrowser1.URL := edtURL.Text;
end;

procedure TWebBrowserForm.edtURLKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkReturn then
  begin
    { navigate and hide the virtual keyboard when setting focus to GO button }
    WebBrowser1.URL := edtURL.Text;
    btnGO.SetFocus;
  end;
end;

procedure TWebBrowserForm.btnBackClick(Sender: TObject);
begin
  { move back one page in the history }
  WebBrowser1.GoBack;
end;

procedure TWebBrowserForm.btnForwardClick(Sender: TObject);
begin
  { move forward one page in the history }
  WebBrowser1.GoForward;
end;

end.
