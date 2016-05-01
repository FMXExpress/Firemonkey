//---------------------------------------------------------------------------

#ifndef QuizH
#define QuizH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <vector>
#include <algorithm>
#include <memory>
//---------------------------------------------------------------------------

struct StateInfo {
	const char* name;
	const char* capital;
	const char* candidates[4];
};

struct Question {
	const StateInfo* state;
    const char* candidates[4];
};

class TQuizForm : public TForm
{
__published:	// IDE-managed Components
	TLabel *lQuestion;
	TButton *Answer1;
	TButton *Answer2;
	TButton *Answer3;
	TButton *Answer4;
	TToolBar *ToolBar1;
	TLabel *lCurrentQuestion;
	TButton *bHome;
	void __fastcall FormActivate(TObject *Sender);
	void __fastcall FormDeactivate(TObject *Sender);
	void __fastcall Answer1Click(TObject *Sender);
	void __fastcall bHomeClick(TObject *Sender);
protected:
	int FCurrentQuestion;
	int FCorrectAnswers;
	std::vector<Question> questions;

public:		// User declarations
	__fastcall TQuizForm(TComponent* Owner);
	__fastcall ~TQuizForm();
public:
	void __fastcall NewQuiz(int NumberOfQuestions);
	void __fastcall ShowQuestion(int QuestionNumber);
	void __fastcall GoHome();
	void __fastcall ReviewedAnswer();
};

void CreateQuiz();
void StartQuiz(int NumberOfQuestions);
//---------------------------------------------------------------------------
extern PACKAGE TQuizForm *QuizForm;
//---------------------------------------------------------------------------
#endif
