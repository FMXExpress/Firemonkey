//---------------------------------------------------------------------------

#ifndef Unit2H
#define Unit2H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.Ani.hpp>
#include <FMX.Objects.hpp>
//---------------------------------------------------------------------------
class TForm2 : public TForm
{
__published:	// IDE-managed Components
	TLabel *Label1;
	TTimer *Timer1;
	TButton *Button1;
	TPanel *Panel1;
	TComboBox *ComboBox1;
	TFloatAnimation *SpinAnim;
	TFloatAnimation *MoveDownAnim;
	TCircle *Circle1;
	TPanel *Panel2;
	TFloatAnimation *MoveUpAnim;
	void __fastcall ComboBox1Change(TObject *Sender);
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall MoveUpAnimFinish(TObject *Sender);

private:	// User declarations
	std::map<String, System::WideChar> FontAwesomeIcons;
	int lValue;

public:		// User declarations
	__fastcall TForm2(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm2 *Form2;
//---------------------------------------------------------------------------
#endif
