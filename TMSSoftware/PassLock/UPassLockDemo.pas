unit UPassLockDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TMSBaseControl, FMX.TMSPassLock;

type
  TForm15 = class(TForm)
    TMSFMXPassLock1: TTMSFMXPassLock;
    Panel1: TPanel;
    Label1: TLabel;
    rbNumbers: TRadioButton;
    rbPattern: TRadioButton;
    rbEnter: TRadioButton;
    rbLearn: TRadioButton;
    Label2: TLabel;
    Label3: TLabel;
    procedure TMSFMXPassLock1PasswordConfirmed(Sender: TObject;
      Result: TTMSFMXPasswordConfirm);
    procedure TMSFMXPassLock1PasswordLearned(Sender: TObject);
    procedure TMSFMXPassLock1PasswordMatch(Sender: TObject);
    procedure TMSFMXPassLock1PasswordMismatch(Sender: TObject);
    procedure rbNumbersChange(Sender: TObject);
    procedure rbLearnChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Init;
  end;

var
  Form15: TForm15;

implementation

{$R *.fmx}

procedure TForm15.FormCreate(Sender: TObject);
begin
  Init;
end;

procedure TForm15.Init;
begin
  TMSFMXPassLock1.LearnMode := rbLearn.IsChecked;

  if rbNumbers.IsChecked then
    TMSFMXPassLock1.LockType := pltNumber
  else
    TMSFMXPassLock1.LockType := pltPattern;

  Label3.Text := 'Enter password:'
end;

procedure TForm15.rbLearnChange(Sender: TObject);
begin
  Init;
end;

procedure TForm15.rbNumbersChange(Sender: TObject);
begin
  Init;
  TMSFMXPassLock1.PassValue := '';
end;

procedure TForm15.TMSFMXPassLock1PasswordConfirmed(Sender: TObject;
  Result: TTMSFMXPasswordConfirm);
begin
  if Result = pcSuccess then
    Label3.Text := 'Result: Password confirmed.'
  else
    Label3.Text := 'Result: Confirm failed. Please try again.';
end;

procedure TForm15.TMSFMXPassLock1PasswordLearned(Sender: TObject);
begin
  Label3.Text := 'Result: Password learned. Please confirm password.';
end;

procedure TForm15.TMSFMXPassLock1PasswordMatch(Sender: TObject);
begin
  Label3.Text := 'Result: Password is correct';
end;

procedure TForm15.TMSFMXPassLock1PasswordMismatch(Sender: TObject);
begin
  Label3.Text := 'Result: Password is wrong. Please try again.';
end;

end.
