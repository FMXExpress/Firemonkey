unit Answered;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.Objects, Quiz, FMX.Layouts;

type
  TAnsweredForm = class(TForm)
    ToolBar1: TToolBar;
    lTitle: TLabel;
    lAnswer: TLabel;
    iNo: TImage;
    Button1: TButton;
    bHome: TButton;
    iYes: TImage;
    bNext: TButton;
    Layout1: TLayout;
    procedure bHomeClick(Sender: TObject);
    procedure bNextClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
  protected
    FQuizForm: TQuizForm;
  public
    procedure Prepare(const QuizForm: TQuizForm; Correct: Boolean; const State, Answer: string );
  end;

var
  AnsweredForm: TAnsweredForm = nil;

procedure CreateAnswer(const Form: TForm);
procedure ShowAnswer(const Form: TForm; Correct: Boolean; const State,
  Answer: string);

implementation

{$R *.fmx}

uses Main;

procedure CreateAnswer(const Form: TForm);
begin
  if not Assigned(AnsweredForm) then
    AnsweredForm := TAnsweredForm.Create(Form);
end;

procedure ShowAnswer(const Form: TForm; Correct: Boolean; const State,
  Answer: string);
begin
  CreateAnswer(Form);

  AnsweredForm.Prepare(TQuizForm(Form), Correct, State, Answer);
  AnsweredForm.Show;
end;

{ TAnsweredForm }

procedure TAnsweredForm.bHomeClick(Sender: TObject);
begin
  Close;
  FQuizForm.GoHome;
end;

procedure TAnsweredForm.bNextClick(Sender: TObject);
begin
  Close;
  FQuizForm.ReviewedAnswer;
end;

procedure TAnsweredForm.FormActivate(Sender: TObject);
begin
  Log.d('Setting ad parent to answered form');
  MainForm.TakeAdvertFromMainForm(Self);
end;

procedure TAnsweredForm.FormDeactivate(Sender: TObject);
begin
  Log.d('Setting ad parent back to main form');
  MainForm.PlaceAdvertOnMainForm;
end;

procedure TAnsweredForm.Prepare(const QuizForm: TQuizForm; Correct: Boolean; const State, Answer: string);
const
  cCorrect = 'That is correct!';
  cWrong = 'Oh, wow, sorry';
  cAnswer = '%s is the capital of %s.';
begin
  FQuizForm := QuizForm;
  if Correct then
    lTitle.Text := cCorrect
  else
    lTitle.Text := cWrong;

  iYes.Visible := Correct;
  iNo.Visible := not Correct;

  lAnswer.Text := Format(cAnswer, [Answer, State]);
end;
end.
