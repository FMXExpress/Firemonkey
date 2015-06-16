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

procedure TMessageAlertsForm.btnStandardAlertClick(Sender: TObject);
begin
  { Show a standard alert with a single OK button }
  ShowMessage('Hello World!');
end;

procedure TMessageAlertsForm.btnMultiButtonAlertClick(Sender: TObject);
begin
  { Show a multiple-button alert that triggers different code blocks according to
    your input }
  case MessageDlg('Choose a button:', System.UITypes.TMsgDlgType.mtInformation,
    [
      System.UITypes.TMsgDlgBtn.mbYes,
      System.UITypes.TMsgDlgBtn.mbNo,
      System.UITypes.TMsgDlgBtn.mbCancel
    ], 0) of
    { Detect which button was pushed and show a different message }
    mrYES:
      ShowMessage('You chose Yes');
    mrNo:
      ShowMessage('You chose No');
    mrCancel:
      ShowMessage('You chose Cancel');
  end;
end;

end.
