//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.Memo.hpp>
#include <FMX.ScrollBox.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <Androidapi.JNI.GraphicsContentViewText.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TToolBar *ToolBar1;
	TLayout *Layout1;
	TMemo *Memo1;
	TLabel *Label1;
	void __fastcall FormCreate(TObject *Sender);
private:	// User declarations
	bool __fastcall handleAppEvent(TApplicationEvent appEvent, TObject *context);
	void handleActivityMessage(TObject *sender, System::Messaging::TMessage *m);
	bool handleIntentAction(Androidapi::Jni::Graphicscontentviewtext::_di_JIntent data);
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
