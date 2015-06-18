program CapitalTrivia;

uses
  System.StartUpCopy,
  FMX.Forms,
  Answered in 'Answered.pas' {AnsweredForm},
  Main in 'Main.pas' {MainForm},
  Option in 'Option.pas' {OptionFrame: TFrame},
  Quiz in 'Quiz.pas' {QuizForm},
  Score in 'Score.pas' {ScoreForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
