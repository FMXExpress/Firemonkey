//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef formMainH
#define formMainH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Dialogs.hpp>
#include <FMX.Edit.hpp>
#include <FMX.Effects.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.Menus.hpp>
#include <FMX.Types.hpp>
#include "unitSearchMenuHelperCpp.h"
#include <FMX.Objects.hpp>
#include <FMX.StdCtrls.hpp>

//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
	TLayout *Layout1;
	TReflectionEffect *ReflectionEffect1;
	TPanel *Panel1;
	TLayout *Layout2;
	TClearingEdit *edtSearch;
	TOpenDialog *OpenDialog1;
	TVertScrollBox *VertScrollBox1;
	TMainMenu *MainMenu1;
	TMenuItem *MenuItem4;
	TMenuItem *miLoadData;
	TMenuItem *miLoadImages;
	TMenuItem *miManageData;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall LoadData(TObject *Sender);
	void __fastcall LoadImagesClick(TObject *Sender);
	void __fastcall ManageDataClick(TObject *Sender);
	void __fastcall edtSearchKeyUp(TObject *Sender, WORD &Key, System::WideChar &KeyChar,
		  TShiftState Shift);
	void __fastcall ItemSelected(TObject *Sender);
	void __fastcall edtSearchChange(TObject *Sender);
private:	// User declarations
	TSearchBandManager *SearchBandManager;
	TSearchBand* __fastcall InitializeBandManager();
public:		// User declarations
	__fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
