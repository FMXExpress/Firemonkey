unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls;

type
  TMessageAlertsForm = class(TForm)
    ToolBar1: TToolBar;
    Label1: TLabel;
    procedure btnStandardAlertClick(Sender: TObject);
    procedure btnMultiButtonAlertClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MessageAlertsForm: TMessageAlertsForm;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

procedure ShowMessage(const TheMessage:String);
begin
  MessageDlg(TheMessage, TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOk], 0,
    procedure(const AResult: TModalResult)
      begin

      end
  );
end;

procedure TMessageAlertsForm.btnStandardAlertClick(Sender: TObject);
begin
  { Show a standard alert with a single OK button }
  ShowMessage('Hello World!');
end;


// In Rad Studio XE7, InputBox, InputQuery, and MessageDlg support a new
// optional parameter, <ACloseDialogProc>.

// Calls that include this new parameter work on all platforms, including
// Android. This new optional parameter allows you to provide an anonymous
// method that is called when the dialog box closes. When you call these
// methods using this new parameter, your call is blocking in desktop platforms
// and non-blocking in mobile platforms.

// If you need to execute code after your dialog box
// closes, use this new parameter to ensure that your application
// works as expected on all supported platforms.

procedure TMessageAlertsForm.btnMultiButtonAlertClick(Sender: TObject);
begin
  { Show a multiple-button alert that triggers different code blocks according to
    your input }
  MessageDlg('Choose a button:', System.UITypes.TMsgDlgType.mtInformation,
    [
      System.UITypes.TMsgDlgBtn.mbYes,
      System.UITypes.TMsgDlgBtn.mbNo,
      System.UITypes.TMsgDlgBtn.mbCancel
    ], 0,

      // Use an anonymous method to make sure the acknowledgment appears as expected.

      procedure(const AResult: TModalResult)
      begin
        case AResult
          of
          { Detect which button was pushed and show a different message }
          mrYES:
            ShowMessage('You chose Yes');
          mrNo:
            ShowMessage('You chose No');
          mrCancel:
            ShowMessage('You chose Cancel');
        end;

      end

    )
end;

end.
