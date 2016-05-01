//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

program CapitalTrivia;

uses
  System.StartUpCopy,
  FMX.Forms,
  Answered in 'Answered.pas' {AnsweredForm},
  Main in 'Main.pas' {MainForm},
  Quiz in 'Quiz.pas' {QuizForm},
  Score in 'Score.pas' {ScoreForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
