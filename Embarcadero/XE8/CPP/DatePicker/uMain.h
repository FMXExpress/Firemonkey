// ---------------------------------------------------------------------------

#ifndef uMainH
#define uMainH
// ---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.DateTimeCtrls.hpp>
#include <FMX.Edit.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <FMX.MobilePreview.hpp>

// ---------------------------------------------------------------------------
class TForm2 : public TForm {
__published: // IDE-managed Components

	TListBox *ListBox1;
	TListBoxItem *ListBoxItem5;
	TListBoxItem *ListBoxItem6;
	TToolBar *ToolBar1;
	TLabel *Label1;
	TDateEdit *DateEdit1;
	void __fastcall DateEdit1Change(TObject *Sender);

private: // User declarations
		public : // User declarations
	__fastcall TForm2(TComponent* Owner);
};

// ---------------------------------------------------------------------------
extern PACKAGE TForm2 *Form2;
// ---------------------------------------------------------------------------
#endif
