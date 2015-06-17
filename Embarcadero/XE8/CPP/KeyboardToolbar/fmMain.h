//---------------------------------------------------------------------------

#ifndef fmMainH
#define fmMainH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Edit.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <FMX.MobilePreview.hpp>
#include <FMX.Controls.Presentation.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TToolBar *ToolBar1;
	TButton *btnAdd;
	TListBox *ListBox1;
	TListBoxGroupHeader *ListBoxGroupHeader1;
	TListBoxItem *ListBoxItem1;
	TSwitch *swToolbar;
	TListBoxItem *ListBoxItem2;
	TSwitch *swDoneButton;
	TListBoxGroupHeader *ListBoxGroupHeader2;
	TListBoxItem *ListBoxItem3;
	TEdit *Edit1;
	TSpeedButton *SpeedButton1;
	TTrackBar *TrackBar1;
	TListBoxHeader *ListBoxHeader2;
	TLabel *Label2;
	TListBox *lbButtons;
	TListBoxGroupHeader *ListBoxGroupHeader3;
	TButton *btnDelete;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall btnAddClick(TObject *Sender);
	void __fastcall swToolbarSwitch(TObject *Sender);
	void __fastcall swDoneButtonSwitch(TObject *Sender);
	void __fastcall btnDeleteClick(TObject *Sender);
private:	// User declarations
	_di_IFMXVirtualKeyboardToolbarService FService;
	void __fastcall customButtonExecute(TObject *Sender);
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
