unit uFormClient;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit;

type
  TFormClient = class(TForm)
    edtEmail: TEdit;
    btnVerify: TButton;
    procedure btnVerifyClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormClient: TFormClient;

implementation

{$R *.fmx}

uses uDMEmailChecker;

procedure TFormClient.btnVerifyClick(Sender: TObject);
begin
  if DMEmailChecker.IsValidEmail(edtEmail.Text) then
    ShowMessage('"' + edtEmail.Text + '" is a valid email address')
  else
    ShowMessage('"' + edtEmail.Text + '" is NOT a valid email address');
end;

end.
