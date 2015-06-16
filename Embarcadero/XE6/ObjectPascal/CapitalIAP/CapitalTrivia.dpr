program CapitalTrivia;

uses
  FMX.Forms,
  Answered in 'Answered.pas' {AnsweredForm},
  Main in 'Main.pas' {MainForm},
  Option in 'Option.pas' {OptionForm},
  Quiz in 'Quiz.pas' {QuizForm},
  Score in 'Score.pas' {ScoreForm};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Portrait];
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
