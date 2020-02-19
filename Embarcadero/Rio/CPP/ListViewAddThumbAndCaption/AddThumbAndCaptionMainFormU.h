//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef AddThumbAndCaptionMainFormUH
#define AddThumbAndCaptionMainFormUH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <Data.Bind.Components.hpp>
#include <Data.Bind.EngExt.hpp>
#include <Data.Bind.GenData.hpp>
#include <Data.Bind.ObjectScope.hpp>
#include <Fmx.Bind.DBEngExt.hpp>
#include <Fmx.Bind.Editors.hpp>
#include <Fmx.Bind.GenData.hpp>
#include <FMX.ListView.hpp>
#include <FMX.ListView.Types.hpp>
#include <FMX.ListView.Appearances.hpp>
#include <FMX.Objects.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <System.Bindings.Outputs.hpp>
#include <System.Rtti.hpp>
#include <FMX.MobilePreview.hpp>
#include <FMX.Controls.Presentation.hpp>
//---------------------------------------------------------------------------
class TForm594 : public TForm
{
__published:	// IDE-managed Components
	TToolBar *ToolBar1;
	TSpeedButton *ToggleEditMode;
	TListView *ListViewBottomDetail;
	TToolBar *ToolBar2;
	TSpeedButton *SpeedButtonLiveBindings;
	TSpeedButton *SpeedButtonFill;
	TImage *ImageRAD;
	TImage *ImageRatings;
	TPrototypeBindSource *PrototypeBindSource1;
	TBindingsList *BindingsList1;
	TLinkFillControlToField *LinkFillControlToField1;
	void __fastcall ListViewBottomDetailUpdateObjects(const TObject *Sender, const TListViewItem *AItem);
	void __fastcall SpeedButtonFillClick(TObject *Sender);
	void __fastcall SpeedButtonLiveBindingsClick(TObject *Sender);
	void __fastcall ToggleEditModeClick(TObject *Sender);
	void __fastcall LinkFillControlToField1FilledListItem(TObject *Sender, const IBindListEditorItem *AEditor);


private:	// User declarations
public:		// User declarations
	__fastcall TForm594(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm594 *Form594;
//---------------------------------------------------------------------------
#endif
