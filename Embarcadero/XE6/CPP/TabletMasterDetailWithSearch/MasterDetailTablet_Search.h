//---------------------------------------------------------------------------

#ifndef MasterDetailTablet_SearchH
#define MasterDetailTablet_SearchH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <Data.Bind.Components.hpp>
#include <Data.Bind.EngExt.hpp>
#include <Data.Bind.GenData.hpp>
#include <Data.Bind.ObjectScope.hpp>
#include <FMX.ActnList.hpp>
#include <Fmx.Bind.DBEngExt.hpp>
#include <Fmx.Bind.Editors.hpp>
#include <Fmx.Bind.GenData.hpp>
#include <Fmx.Bind.Navigator.hpp>
#include <FMX.Edit.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListView.hpp>
#include <FMX.ListView.Types.hpp>
#include <FMX.Memo.hpp>
#include <FMX.Objects.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <System.Actions.hpp>
#include <System.Bindings.Outputs.hpp>
#include <System.Rtti.hpp>
#include <FMX.MobilePreview.hpp>
//---------------------------------------------------------------------------
class TTabletSearchForm : public TForm
{
__published:	// IDE-managed Components
	TLayout *LeftLayout;
	TToolBar *ToolBarList;
	TLabel *ListToolbarLabel;
	TListView *ListView1;
	TLayout *RightLayout;
	TToolBar *ToolBarDetail;
	TLabel *lblDetail;
	TSpeedButton *btnUp;
	TSpeedButton *btnDown;
	TImage *imgContact;
	TLabel *lblTitle;
	TLabel *lblName;
	TMemo *DetailMemo;
	TLine *Line1;
	TPrototypeBindSource *PrototypeBindSource1;
	TBindingsList *BindingsList1;
	TLinkPropertyToField *LinkPropertyToFieldBitmap;
	TLinkPropertyToField *LinkPropertyToFieldText;
	TLinkPropertyToField *LinkPropertyToFieldText2;
	TLinkControlToField *LinkControlToField1;
	TLinkFillControlToField *LinkFillControlToField1;
	TActionList *ActionList1;
	TFMXBindNavigatePrior *LiveBindingsBindNavigatePrior1;
	TFMXBindNavigateNext *LiveBindingsBindNavigateNext1;
	void __fastcall LiveBindingsBindNavigatePrior1Execute(TObject *Sender);
	void __fastcall LiveBindingsBindNavigatePrior1Update(TObject *Sender);
	void __fastcall LiveBindingsBindNavigateNext1Execute(TObject *Sender);
	void __fastcall LiveBindingsBindNavigateNext1Update(TObject *Sender);
	void __fastcall ListView1ItemClick(const TObject *Sender, const TListViewItem *AItem);

private:	// User declarations
	String Lower;
	bool __fastcall Filtered();
	bool __fastcall OnFiltered(String X);
public:		// User declarations
	__fastcall TTabletSearchForm(TComponent* Owner);
};

// TMyPredicate
class TMyPredicate : public TInterfacedObject, public TPredicate__1<UnicodeString>
{
	UnicodeString Value;
public:
	__inline __fastcall TMyPredicate(UnicodeString AValue): Value(AValue){}
	// TInterfacedObject methods
	HRESULT STDMETHODCALLTYPE QueryInterface (REFIID riid, void** ppvObject){
		return TInterfacedObject::QueryInterface (riid, ppvObject);
	}
	ULONG STDMETHODCALLTYPE AddRef() { return TInterfacedObject::_AddRef(); }
	ULONG STDMETHODCALLTYPE Release() { return TInterfacedObject::_Release(); }

	// TPredicate__1 method
	bool __fastcall Invoke(UnicodeString Arg1)
	{
		return (Value == EmptyStr) || ContainsStr(LowerCase(Arg1), Value);
	}
};

//---------------------------------------------------------------------------
extern PACKAGE TTabletSearchForm *TabletSearchForm;
//---------------------------------------------------------------------------
#endif
