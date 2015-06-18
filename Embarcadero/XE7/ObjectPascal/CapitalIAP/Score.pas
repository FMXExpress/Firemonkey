unit Score;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.Objects, FMX.Layouts;

type
  TScoreForm = class(TForm)
    ToolBar1: TToolBar;
    Label1: TLabel;
    lResults: TLabel;
    Image1: TImage;
    lRight: TLabel;
    lWrong: TLabel;
    Button1: TButton;
    Layout1: TLayout;
    Layout2: TLayout;
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
  protected
    FRight: Integer;
    FWrong: Integer;
    procedure PrepareForm(Right, Wrong: Integer);
  end;

procedure CreateScore(const Form: TForm);
procedure ShowScore(const Form: TForm; TotalNumberOfQuestions, CorrectAnswers: Integer);

implementation

uses Option, Main;

{$R *.fmx}

var
  ScoreForm: TScoreForm = nil;

procedure CreateScore(const Form: TForm);
begin
  if not Assigned(ScoreForm) then
    ScoreForm := TScoreForm.Create(Form);
end;

procedure ShowScore(const Form: TForm; TotalNumberOfQuestions, CorrectAnswers: Integer);
begin
  CreateScore(Form);
  Form.Hide;
  ScoreForm.PrepareForm(CorrectAnswers, TotalNumberOfQuestions - CorrectAnswers);
  ScoreForm.Show;
end;

procedure TScoreForm.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TScoreForm.Button4Click(Sender: TObject);
begin
  // ShowOptions;
end;

procedure TScoreForm.FormActivate(Sender: TObject);
begin
  Log.d('Setting ad parent to score form');
  MainForm.TakeAdvertFromMainForm(Self);
end;

procedure TScoreForm.FormDeactivate(Sender: TObject);
begin
  Log.d('Setting ad parent back to main form');
  MainForm.PlaceAdvertOnMainForm;
end;

procedure TScoreForm.PrepareForm(Right, Wrong: Integer);
begin
  lRight.Text := IntToStr(Right);
  lWrong.Text := IntToStr(Wrong);
end;

end.
