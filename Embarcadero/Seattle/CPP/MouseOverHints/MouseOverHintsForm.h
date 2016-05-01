//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef MouseOverHintsFormH
#define MouseOverHintsFormH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.ActnList.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.Edit.hpp>
#include <FMX.Menus.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <System.Actions.hpp>
//---------------------------------------------------------------------------
class TMouseoverHintForm : public TForm
{
__published:	// IDE-managed Components
	TActionList *ActionList1;
	TAction *Action1;
	TAction *Action2;
	TAction *Action3;
	TCheckBox *ApplicationShortCutsInHintsCheckBox;
	TCheckBox *ApplicationShowHint;
	TButton *Button1;
	TButton *Button2;
	TButton *Button3;
	TButton *Button4;
	TButton *Button6;
	TCheckBox *EnableActionOnHintCheckBox;
	TCheckBox *FormShowHint;
	TEdit *HintEdit;
	TLabel *Label1;
	TLabel *Label2;
	TMainMenu *MainMenu1;
	TMenuItem *MenuItem1;
	TMenuItem *MenuItem3;
	TMenuItem *MenuItem4;
	TMenuItem *MenuItem2;
	TMenuItem *MenuItem5;
	TMenuItem *MenuItem6;
	TPanel *Panel1;
	TButton *Button5;
	TStatusBar *StatusBar1;
	TCheckBox *StatusBarAutohintCheckBox;
	void __fastcall Action1Execute(TObject *Sender);
	void __fastcall Action2Execute(TObject *Sender);
	void __fastcall Action3Execute(TObject *Sender);
	void __fastcall Action2Hint(UnicodeString &HintStr, bool &CanShow);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall ApplicationShowHintChange(TObject *Sender);
	void __fastcall FormShowHintChange(TObject *Sender);
	void __fastcall ApplicationShortCutsInHintsCheckBoxChange(TObject *Sender);
	void __fastcall StatusBarAutohintCheckBoxChange(TObject *Sender);
	void __fastcall HintEditChange(TObject *Sender);
	void __fastcall Action3Hint(UnicodeString &HintStr, bool &CanShow);
	void __fastcall StatusBar1Hint(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TMouseoverHintForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TMouseoverHintForm *MouseoverHintForm;
//---------------------------------------------------------------------------
#endif
