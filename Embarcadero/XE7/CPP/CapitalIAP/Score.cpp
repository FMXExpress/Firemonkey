//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "Score.h"

#include "Option.h"
#include "Main.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TScoreForm *ScoreForm = NULL;
//---------------------------------------------------------------------------
void CreateScore(const TForm *Form)
{
	if(ScoreForm == NULL) {
		ScoreForm = new TScoreForm((TComponent*)Form);
	}
}
//---------------------------------------------------------------------------
void ShowScore(const TForm * Form, int TotalNumberOfQuestions, int CorrectAnswers)
{
	CreateScore(Form);
	const_cast<TForm*>(Form)->Hide();
	ScoreForm->PrepareForm(CorrectAnswers, TotalNumberOfQuestions - CorrectAnswers);
	ScoreForm->Show();
}
//---------------------------------------------------------------------------
void __fastcall TScoreForm::PrepareForm(int Right, int Wrong)
{
	lRight->Text = IntToStr(Right);
	lWrong->Text = IntToStr(Wrong);
}
//---------------------------------------------------------------------------
__fastcall TScoreForm::TScoreForm(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TScoreForm::Button1Click(TObject *Sender)
{
	Close();
}
//---------------------------------------------------------------------------

void __fastcall TScoreForm::FormActivate(TObject *Sender)
{
	Log::d("Setting ad parent to score form");
	MainForm->TakeAdvertFromMainForm(this);
}
//---------------------------------------------------------------------------

void __fastcall TScoreForm::FormDeactivate(TObject *Sender)
{
	Log::d("Setting ad parent back to main form");
	MainForm->PlaceAdvertOnMainForm();
}
//---------------------------------------------------------------------------

