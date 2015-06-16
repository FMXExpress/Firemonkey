//---------------------------------------------------------------------------

#ifndef QuizH
#define QuizH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <vector>
#include <algorithm>
//---------------------------------------------------------------------------

struct TCapital {
	String State;
	String City;
	std::vector<String> OtherCities;
	String Region;
	bool Used;
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
	int FNumberOfStates;
	int FNumberOfQuestions;
	TCapital *FAnswerState;
	int FCurrentQuestion;
	int FCorrectAnswers;
	std::vector<TCapital*> FStateList;
private:	// User declarations
	void __fastcall GetStates(void);
public:		// User declarations
	__fastcall TQuizForm(TComponent* Owner);
public:
	void __fastcall NewQuiz(int NumberOfQuestions);
	void __fastcall ShowQuestion(int QuestionNumber);
	void __fastcall GoHome(void);
	void __fastcall ReviewedAnswer(void);
};

void CreateQuiz(void);
void StartQuiz(int NumberOfQuestions);
//---------------------------------------------------------------------------
extern PACKAGE TQuizForm *QuizForm;
//---------------------------------------------------------------------------
#endif
