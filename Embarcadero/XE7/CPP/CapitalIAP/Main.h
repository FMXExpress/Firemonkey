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
//#include "Option.h"
#include "Quiz.h"
#include "Score.h"
#include "Options.h"
#include <FMX.MultiView.hpp>
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
	TMultiView *OptionsMultiView;
	TfrOptions *OptionFrame;
	TLayout *ClientLayout;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormActivate(TObject *Sender);
	void __fastcall FormDeactivate(TObject *Sender);
	void __fastcall bFiveQuestionClick(TObject *Sender);
	void __fastcall bTenQuestionClick(TObject *Sender);
	void __fastcall bTwentyQuestionClick(TObject *Sender);
	void __fastcall OptionsMultiViewHidden(TObject *Sender);
	void __fastcall OptionFormButton1Click(TObject *Sender);
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
public:		// User declarations
	__fastcall TMainForm(TComponent* Owner);
public:
	void __fastcall TakeAdvertFromMainForm(TCustomForm *Form);
	void __fastcall PlaceAdvertOnMainForm(void);
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
