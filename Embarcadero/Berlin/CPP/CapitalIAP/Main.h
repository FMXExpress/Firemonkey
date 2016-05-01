//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef MainH
#define MainH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <FMX.Advertising.hpp>
#include <FMX.InAppPurchase.hpp>
#include "Answered.h"
#include "Quiz.h"
#include "Score.h"
#include <FMX.MultiView.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.Controls.Presentation.hpp>
//---------------------------------------------------------------------------
class TMainForm : public TForm
{
__published:	// IDE-managed Components
	TLayout *UILayout;
	TButton *bFiveQuestion;
	TButton *bTenQuestion;
	TButton *bTwentyQuestion;
	TLabel *Label1;
	TToolBar *ToolBar1;
	TButton *Button4;
	TLayout *ClientLayout;
	TMultiView *OptionsMultiView;
	TListBox *OptionsListBox;
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
	TListBoxItem *ConsumeListBoxItem;
	TListBoxItem *RestoreAdsListBoxItem;
	TToolBar *OptionsToolBar;
	TLabel *OptionsLabel;
	TButton *OptionsCloseButton;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormActivate(TObject *Sender);
	void __fastcall FormDeactivate(TObject *Sender);
	void __fastcall bFiveQuestionClick(TObject *Sender);
	void __fastcall bTenQuestionClick(TObject *Sender);
	void __fastcall bTwentyQuestionClick(TObject *Sender);
	void __fastcall OptionsMultiViewHidden(TObject *Sender);
	void __fastcall OptionFormButton1Click(TObject *Sender);
	void __fastcall OptionsListBoxItemClick(TCustomListBox * const Sender, TListBoxItem * const Item);
	void __fastcall sEastSwitch(TObject *Sender);
	void __fastcall sCentralSwitch(TObject *Sender);
	void __fastcall sWestSwitch(TObject *Sender);
	void __fastcall sEuropeSwitch(TObject *Sender);
private:	// User declarations
	bool FWest;
	bool FEast;
	bool FCentral;
	bool FEurope;
	TInAppPurchase * FInAppPurchase;
	bool FNoAdsProductIsValid;
	bool FEuropeProductIsValid;
	void __fastcall InAppPurchaseOnSetupComplete(TObject *Sender);
	void __fastcall InAppPurchaseProductsRequestResponse(System::TObject* Sender, TIAPProductList* const Products,
		System::Classes::TStrings* const InvalidProductIDs);
	void __fastcall InAppPurchaseError(System::TObject* Sender, TFailureKind ErrorKind,
		const System::UnicodeString ErrorMessage);
	void __fastcall InAppPurchasePurchaseCompleted(System::TObject* Sender, const System::UnicodeString ProductID,
		bool NewTransaction);
	void __fastcall DisableEuropePurchaseUI(bool BecauseOfPurchase);
	void __fastcall DisablePurchaseUI(bool BecauseOfPurchase);
	void __fastcall EnableEuropePurchaseUI();
	void __fastcall EnablePurchaseUI();
	void __fastcall BannerAdDidFail(TObject * Sender, const String Error);
	void __fastcall BannerAdDidLoad(TObject * Sender);
	void __fastcall BannerAdWillLoad(TObject * Sender);
	void __fastcall CheckState();
public:		// User declarations
	__fastcall TMainForm(TComponent* Owner);
public:
	void __fastcall TakeAdvertFromMainForm(TCustomForm *Form);
	void __fastcall PlaceAdvertOnMainForm();
	__property bool West = {read=FWest, write=FWest};
	__property bool East = {read=FEast, write=FEast};
	__property bool Central = {read=FCentral, write=FCentral};
	__property bool Europe = {read=FEurope, write=FEurope};
	void __fastcall OptionsDone();
	void __fastcall PurchaseEurope();
	void __fastcall RestorePurchase();
	void __fastcall DisableAdverts();
	void __fastcall ConsumeProducts();
private:
	TBannerAd * FBannerAd;
};
//---------------------------------------------------------------------------
extern PACKAGE TMainForm *MainForm;
//---------------------------------------------------------------------------
#endif
