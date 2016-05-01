//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef OptionsH
#define OptionsH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
//---------------------------------------------------------------------------
class TfrOptions : public TFrame
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
	void __fastcall sEastSwitch(TObject *Sender);
	void __fastcall sCentralSwitch(TObject *Sender);
	void __fastcall sWestSwitch(TObject *Sender);
	void __fastcall sEuropeSwitch(TObject *Sender);
	void __fastcall DisableAdsListBoxItemClick(TObject *Sender);
	void __fastcall EuropeListBoxItemClick(TObject *Sender);
	void __fastcall RestoreAdsListBoxItemClick(TObject *Sender);
	void __fastcall ConsumeListBoxItemClick(TObject *Sender);
private:	// User declarations
	void __fastcall CheckState();
public:		// User declarations
	__fastcall TfrOptions(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrOptions *frOptions;
//---------------------------------------------------------------------------
#endif
