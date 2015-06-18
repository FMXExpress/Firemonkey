//---------------------------------------------------------------------------

#ifndef VirtualKeyboardBaseH
#define VirtualKeyboardBaseH
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
class TVKBaseForm : public TForm
{
__published:	// IDE-managed Components
	TVertScrollBox *VertScrollBox1;
	TLayout *MainLayout1;
	TToolBar *ToolBar1;
	TLabel *Label1;
	TListBox *ListBox1;
	TListBoxItem *ListBoxItem1;
	TEdit *Edit1;
	TListBoxItem *ListBoxItem2;
	TEdit *Edit2;
	TListBoxItem *ListBoxItem3;
	TEdit *Edit3;
	TListBoxItem *ListBoxItem4;
	TEdit *Edit4;
	TListBoxItem *ListBoxItem5;
	TEdit *Edit5;
	TListBoxItem *ListBoxItem6;
	TEdit *Edit6;
	TListBoxItem *ListBoxItem7;
	TEdit *Edit7;
	TListBoxItem *ListBoxItem8;
	TEdit *Edit8;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormFocusChanged(TObject *Sender);
	void __fastcall FormVirtualKeyboardHidden(TObject *Sender, bool KeyboardVisible,
          const TRect &Bounds);
	void __fastcall FormVirtualKeyboardShown(TObject *Sender, bool KeyboardVisible,
		  const TRect &Bounds);
private:	// User declarations
	void __fastcall RestorePosition();
	void __fastcall UpdateKBBounds();
	void __fastcall CalcContentBoundsProc(TObject * Sender, System::Types::TRectF &ContentBounds);
	_di_IFMXVirtualKeyboardService FService;
	_di_IFMXVirtualKeyboardToolbarService FService1;
	System::Types::TRectF FKBBounds;
	bool FNeedOffset;
public:		// User declarations
	__fastcall TVKBaseForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TVKBaseForm *VKBaseForm;
//---------------------------------------------------------------------------
#endif
