//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "Answered.h"
#include "Main.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"

TAnsweredForm *AnsweredForm = NULL;

//---------------------------------------------------------------------------
void CreateAnswer(const TForm *Form)
{
	if(AnsweredForm == nullptr) {
		AnsweredForm = new TAnsweredForm(const_cast<TForm*>(Form));
	}
}
//---------------------------------------------------------------------------
void __fastcall TAnsweredForm::Prepare(TQuizForm *QuizForm,
	bool Correct, const String State, const String Answer)
{
	const String cCorrect = "That is correct!";
	const String cWrong = "Oh, wow, sorry";
	const String cAnswer = "%s is the capital of %s.";
	FQuizForm = QuizForm;
	if(Correct) {
		lTitle->Text = cCorrect;
	}
	else {
		lTitle->Text = cWrong;
	}
	iYes->Visible = Correct;
	iNo->Visible = !Correct;

	lAnswer->Text = Format(cAnswer, ARRAYOFCONST((Answer, State)));
}
//---------------------------------------------------------------------------
void ShowAnswer(const TForm * Form, bool Correct, const String State,
	const String Answer)
{
	CreateAnswer(Form);
	AnsweredForm->Prepare((TQuizForm*)Form, Correct, State, Answer);
	AnsweredForm->Show();
}
//---------------------------------------------------------------------------
__fastcall TAnsweredForm::TAnsweredForm(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TAnsweredForm::FormActivate(TObject *Sender)
{
	Log::d("Setting ad parent to answered form");
	MainForm->TakeAdvertFromMainForm(this);
}
//---------------------------------------------------------------------------

void __fastcall TAnsweredForm::FormDeactivate(TObject *Sender)
{
	Log::d("etting ad parent back to main form");
	MainForm->PlaceAdvertOnMainForm();
}
//---------------------------------------------------------------------------

void __fastcall TAnsweredForm::bHomeClick(TObject *Sender)
{
	Close();
  	FQuizForm->GoHome();
}
//---------------------------------------------------------------------------

void __fastcall TAnsweredForm::bNextClick(TObject *Sender)
{
	Close();
	FQuizForm->ReviewedAnswer();
}
//---------------------------------------------------------------------------

