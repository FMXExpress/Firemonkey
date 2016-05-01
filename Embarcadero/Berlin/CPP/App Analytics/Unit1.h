//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Analytics.AppAnalytics.hpp>
#include <FMX.Analytics.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.Edit.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include "Unit2.h"
#include <System.IOUtils.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TToolBar *ToolBar1;
	TLabel *Label1;
	TGroupBox *GroupBoxConnection;
	TButton *EnableButton;
	TButton *ClearSaveStateButton;
	TLabel *Label2;
	TEdit *EdStatus;
	TEdit *EdUserID;
	TLabel *Label3;
	TGroupBox *GroupBoxActions;
	TEdit *MainFormEdit1;
	TButton *LaunchForm2Button;
	TButton *RaiseExceptionButton;
	TAppAnalytics *AppAnalytics1;
	TButton *RefreshButton;
	TButton *CustomEventButton;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormSaveState(TObject *Sender);
	void __fastcall EnableButtonClick(TObject *Sender);
	void __fastcall ClearSaveStateButtonClick(TObject *Sender);
	void __fastcall LaunchForm2ButtonClick(TObject *Sender);
	void __fastcall RaiseExceptionButtonClick(TObject *Sender);
	void __fastcall RefreshButtonClick(TObject *Sender);
	void __fastcall CustomEventButtonClick(TObject *Sender);
private:	// User declarations
	void __fastcall UpdateUI(void);
	bool FClearState;
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
