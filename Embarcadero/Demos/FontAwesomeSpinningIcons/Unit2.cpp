//---------------------------------------------------------------------------

#include <fmx.h>
#include <map>
#include <System.Threading.hpp>

#include "FontAwesomeCodes.h"

#pragma hdrstop

#include "Unit2.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
#pragma resource ("*.NmXhdpiPh.fmx", _PLAT_ANDROID)
#pragma resource ("*.SmXhdpiPh.fmx", _PLAT_ANDROID)
#pragma resource ("*.iPhone.fmx", _PLAT_IOS)

TForm2 *Form2;

//---------------------------------------------------------------------------
class TCppSync : public TCppInterfacedObject<TThreadProcedure> {
  int sleepTime;//lValue;
  TFloatAnimation *FloatAnimation;
  TButton *Button;
public:
  TCppSync(int l, TFloatAnimation *fa, TButton *b) : sleepTime(l), FloatAnimation(fa), Button(b)
  {}
  void __fastcall Invoke() {
	//- fire off the animation passed in to this task
	FloatAnimation->Enabled 	= true;

	//- unlock the UI, simulated load state is finished.
	Button->Enabled 			= true;
  }
};

class TCppTask : public TCppInterfacedObject<TProc> {
  int sleepTime;//lValue;
  TFloatAnimation *FloatAnimation;
  TButton *Button;
public:
  TCppTask(int l, TFloatAnimation *fa, TButton *b) : sleepTime(l), FloatAnimation(fa), Button(b)
  {}
  void __fastcall Invoke() {

	//- simulate a load event that takes some amount of seconds
	Sleep(sleepTime);

	//- the task has finished, pass control back to the main thread with synchronize
	TThread::Synchronize(0, _di_TThreadProcedure(new TCppSync(sleepTime, FloatAnimation, Button)));
  }
};


//---------------------------------------------------------------------------
__fastcall TForm2::TForm2(TComponent* Owner)
	: TForm(Owner)
{
	//- map font awesome combo box items to the values in FontAwesomeCodes.h
	FontAwesomeIcons["Circle Notch"] 	= fa_circle_o_notch;
	FontAwesomeIcons["Cog"] 			= fa_cog;
	FontAwesomeIcons["Gear"] 			= fa_gear;
	FontAwesomeIcons["Refresh"] 		= fa_refresh;
	FontAwesomeIcons["Spinner"] 		= fa_spinner;

	//- Hide the spinner until we need it
	Circle1->Visible 			= false;

	//- set animation values relative to circle height
	MoveDownAnim->StartValue	= -1 * Circle1->Height;
	MoveDownAnim->StopValue		= Circle1->Height / 2;

	MoveUpAnim->StartValue		= Circle1->Height / 2;
	MoveUpAnim->StopValue   	= -1 * Circle1->Height;

	//- set inital spinner icon
	Label1->Text = FontAwesomeIcons[ ComboBox1->Selected->Text ];
}
//---------------------------------------------------------------------------
void __fastcall TForm2::ComboBox1Change(TObject *Sender)
{
	//- because this application uses threads, the combo box will remain
	//		active during the load state. It is possible to change the loading
	//		icon while the simulated load is running.
	Label1->Text = FontAwesomeIcons[ ComboBox1->Selected->Text ];
}
//---------------------------------------------------------------------------

void __fastcall TForm2::Button1Click(TObject *Sender)
{
	//- reset animations
	MoveDownAnim->Enabled		= false;
	MoveUpAnim->Enabled			= false;

	//- initiate UI state within the main thread and not in the task itself
	Button1->Enabled 			= false;
	Circle1->Visible  			= true;

	//- reveal spinning icon container
	MoveDownAnim->Enabled 		= true;

	//- fire off a new taask to simulate a load event
	//		here we pass in the animation to run at the end of
	//		the simulated loading task along with UI elements we want to
	//		disable.
	TTask::Run( _di_TProc(new TCppTask(5000, MoveUpAnim, Button1)) );
}
//---------------------------------------------------------------------------


void __fastcall TForm2::MoveUpAnimFinish(TObject *Sender)
{
	//- hide the circle and spinner icon after the animation is finished
	Circle1->Visible = false;
}
//---------------------------------------------------------------------------

