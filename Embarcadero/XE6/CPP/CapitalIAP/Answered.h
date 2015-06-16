//---------------------------------------------------------------------------

#ifndef AnsweredH
#define AnsweredH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.Objects.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include "Quiz.h"
//---------------------------------------------------------------------------
class TAnsweredForm : public TForm
{
__published:	// IDE-managed Components
	TLabel *lAnswer;
	TButton *Button1;
	TImage *iNo;
	TImage *iYes;
	TLayout *Layout1;
	TButton *bNext;
	TToolBar *ToolBar1;
	TLabel *lTitle;
	TButton *bHome;
	void __fastcall FormActivate(TObject *Sender);
	void __fastcall FormDeactivate(TObject *Sender);
	void __fastcall bHomeClick(TObject *Sender);
	void __fastcall bNextClick(TObject *Sender);
private:	// User declarations
	TQuizForm * FQuizForm;
public:
	void __fastcall Prepare(TQuizForm *QuizForm, bool Correct,
		const String State, const String Answer);
public:		// User declarations
	__fastcall TAnsweredForm(TComponent* Owner);
};

void CreateAnswer(const TForm *Form);
void ShowAnswer(const TForm * Form, bool Correct, const String State,
	const String Answer);
//---------------------------------------------------------------------------
extern PACKAGE TAnsweredForm *AnsweredForm;
//---------------------------------------------------------------------------
#endif
