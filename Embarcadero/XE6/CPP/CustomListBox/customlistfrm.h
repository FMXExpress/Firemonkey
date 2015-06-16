//---------------------------------------------------------------------------

#ifndef customlistfrmH
#define customlistfrmH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Dialogs.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.Types.hpp>
#include <FMX.Objects.hpp>
#include <FMX.StdCtrls.hpp>
//---------------------------------------------------------------------------
class TfrmCustomList : public TForm
{
__published:	// IDE-managed Components
	TButton *Button1;
	TButton *Button2;
	TButton *Button3;
	TCheckBox *CheckBox1;
	TLabel *InfoLabel;
	TLabel *Label1;
	TListBox *ListBox1;
	TOpenDialog *OpenDialog1;
	TImage *Image1;
	TImage *Image2;
	TStyleBook *Resources1;
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall Button2Click(TObject *Sender);
	void __fastcall CheckBox1Change(TObject *Sender);
	void __fastcall Button3Click(TObject *Sender);
private:	// User declarations
	void __fastcall DoInfoClick(TObject *Sender);
	void __fastcall DoVisibleChange(TObject *Sender);
	void __fastcall DoApplyStyleLookup(TObject *Sender);
public:		// User declarations
	__fastcall TfrmCustomList(TComponent* Owner);
};

long rndm(long max);

//---------------------------------------------------------------------------
extern PACKAGE TfrmCustomList *frmCustomList;
//---------------------------------------------------------------------------
#endif
