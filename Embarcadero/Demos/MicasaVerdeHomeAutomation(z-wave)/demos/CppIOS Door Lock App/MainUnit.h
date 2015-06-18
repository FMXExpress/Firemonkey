//---------------------------------------------------------------------------

#ifndef MainUnitH
#define MainUnitH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <Data.Bind.Components.hpp>
#include <Data.Bind.ObjectScope.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <IPPeerClient.hpp>
#include <REST.Client.hpp>
#include <FMX.Edit.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.Memo.hpp>
//---------------------------------------------------------------------------
class TForm5 : public TForm
{
__published:	// IDE-managed Components
	TRESTClient *RESTClient1;
	TRESTRequest *RESTRequestLockOperation;
	TToolBar *ToolBar1;
	TLabel *Label1;
	TSwitch *VeraLiteSwitch;
	TListBox *ListBox1;
	TListBoxItem *ListBoxItem1;
	TListBoxItem *ListBoxItem2;
	TEdit *VeraLiteEdit;
	TComboBox *ComboBox1;
	TListBoxItem *ListBoxItem3;
	TButton *DoItButton;
	TMemo *Memo1;
	void __fastcall DoItButtonClick(TObject *Sender);
	void __fastcall VeraLiteSwitchSwitch(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TForm5(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm5 *Form5;
//---------------------------------------------------------------------------
#endif
