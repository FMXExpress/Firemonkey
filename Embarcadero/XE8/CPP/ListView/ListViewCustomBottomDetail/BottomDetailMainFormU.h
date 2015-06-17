//---------------------------------------------------------------------------

#ifndef BottomDetailMainFormUH
#define BottomDetailMainFormUH
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
#include <FMX.Objects.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <System.Bindings.Outputs.hpp>
#include <System.Rtti.hpp>
//#include "RatingsAppearanceU.h"
//---------------------------------------------------------------------------
class TForm594 : public TForm
{
__published:	// IDE-managed Components
	TToolBar *ToolBar1;
	TSpeedButton *ToggleEditMode;
	TSpeedButton *SpeedButtonSetProps;
	TListView *ListViewBottomDetail;
	TToolBar *ToolBar2;
	TSpeedButton *SpeedButtonLiveBindings;
	TSpeedButton *SpeedButtonFill;
	TImage *ImageRAD;
	TImage *ImageRatings;
	TPrototypeBindSource *PrototypeBindSource1;
	TBindingsList *BindingsList1;
	TLinkFillControlToField *LinkFillControlToField1;
	void __fastcall SpeedButtonSetPropsClick(TObject *Sender);
	void __fastcall ToggleEditModeClick(TObject *Sender);
	void __fastcall SpeedButtonFillClick(TObject *Sender);
	void __fastcall SpeedButtonLiveBindingsClick(TObject *Sender);
	void __fastcall ListViewBottomDetailUpdateObjects(const TObject *Sender, const TListViewItem *AItem);

private:	// User declarations
	void __fastcall SetTextProperties( TTextObjectAppearance * AText);
	void __fastcall SetDetailProperties( TTextObjectAppearance * ADetail);
	void __fastcall SetImageProperties( TImageObjectAppearance * AImage);
	void __fastcall SetCommonProperties( TItemAppearanceObjects * AObjects);
	void __fastcall SetEditItemProperties( TItemAppearanceObjects * AObjects);
public:		// User declarations
	__fastcall TForm594(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm594 *Form594;
//---------------------------------------------------------------------------
#endif
