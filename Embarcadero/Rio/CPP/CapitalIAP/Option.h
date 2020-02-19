//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef OptionH
#define OptionH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
//---------------------------------------------------------------------------
class TOptionForm : public TForm
{
__published:	// IDE-managed Components
	TListBox *ListBox1;
	TListBoxGroupHeader *ListBoxGroupHeader1;
	TListBoxItem *ListBoxItem1;
	TSwitch *sEast;
	TListBoxItem *ListBoxItem2;
	TSwitch *sCentral;
	TListBoxItem *ListBoxItem3;
	TSwitch *sWest;
	TListBoxItem *ListBoxItem4;
	TSwitch *sEurope;
	TListBoxGroupHeader *ListBoxGroupHeader2;
	TListBoxItem *DisableAdsListBoxItem;
	TListBoxItem *EuropeListBoxItem;
	TListBoxItem *RestoreAdsListBoxItem;
	TListBoxItem *ConsumeListBoxItem;
	TToolBar *ToolBar1;
	TLabel *Label1;
	TButton *Button1;
	void __fastcall Button1Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TOptionForm(TComponent* Owner);
};

void CreateOptions(void);
void ShowOptions(void);
//---------------------------------------------------------------------------
extern PACKAGE TOptionForm *OptionForm;
//---------------------------------------------------------------------------
#endif
