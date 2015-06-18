//---------------------------------------------------------------------------

#ifndef MainFrmH
#define MainFrmH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Edit.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.Memo.hpp>
#include <FMX.Objects.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <FMX.MobilePreview.hpp>
#include <FMX.Controls.Presentation.hpp>
//---------------------------------------------------------------------------
class TFormMain : public TForm
{
__published:	// IDE-managed Components
	TVertScrollBox *VertScrollBox1;
	TLayout *MainLayout1;
	TEdit *Edit1;
	TClearEditButton *ClearEditButton1;
	TButton *Button2;
	TMemo *Memo1;
	TLabel *LabelTitle;
	TEdit *Edit2;
	TEdit *Edit3;
	TEdit *Edit4;
	TEdit *Edit5;
	TLine *Line2;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormVirtualKeyboardHidden(TObject *Sender, bool KeyboardVisible,
          const TRect &Bounds);
	void __fastcall FormVirtualKeyboardShown(TObject *Sender, bool KeyboardVisible,
          const TRect &Bounds);
	void __fastcall FormFocusChanged(TObject *Sender);
private:	// User declarations
	TRectF FKBBounds;
	bool FNeedOffset;
	void __fastcall CalcContentBoundsProc(TObject *Sender, TRectF &ContentBounds);
	void __fastcall RestorePosition();
	void __fastcall UpdateKBBounds();
public:		// User declarations
	__fastcall TFormMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormMain *FormMain;
//---------------------------------------------------------------------------
#endif
