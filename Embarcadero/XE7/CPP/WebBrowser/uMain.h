//---------------------------------------------------------------------------

#ifndef uMainH
#define uMainH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Edit.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <FMX.WebBrowser.hpp>
#include <FMX.MobilePreview.hpp>
//---------------------------------------------------------------------------
class TForm2 : public TForm
{
__published:	// IDE-managed Components
	TWebBrowser *WebBrowser1;
	TToolBar *ToolBar1;
	TButton *btnGO;
	TClearingEdit *edtURL;
	TStatusBar *StatusBar1;
	TButton *btnBack;
	TButton *btnForward;
	void __fastcall btnBackClick(TObject *Sender);
	void __fastcall btnForwardClick(TObject *Sender);
	void __fastcall btnGOClick(TObject *Sender);
	void __fastcall edtURLKeyUp(TObject *Sender, WORD &Key, System::WideChar &KeyChar,
          TShiftState Shift);
private:	// User declarations
public:		// User declarations
	__fastcall TForm2(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm2 *Form2;
//---------------------------------------------------------------------------
#endif
