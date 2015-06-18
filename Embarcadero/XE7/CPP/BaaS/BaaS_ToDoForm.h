//---------------------------------------------------------------------------

#ifndef BaaS_ToDoFormH
#define BaaS_ToDoFormH
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
#include <FMX.Edit.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListView.hpp>
#include <FMX.ListView.Types.hpp>
#include <FMX.Memo.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.TabControl.hpp>
#include <FMX.Types.hpp>
#include <System.Actions.hpp>
#include <System.Bindings.Outputs.hpp>
#include <System.Rtti.hpp>
#include "DataModuleUnit1.h"
#include <Data.Bind.Controls.hpp>
#include <Fmx.Bind.Navigator.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <stack>
//---------------------------------------------------------------------------
enum class TView {List = 0 , Details = 2, Add = 3, Edit = 4};

class TKinveyToDoList : public TForm
{
__published:	// IDE-managed Components
	TToolBar *ToolBar1;
	TButton *ButtonAdd;
	TButton *ButtonBack;
	TLabel *Label1;
	TSpeedButton *SpeedButton2;
	TSpeedButton *SpeedButton1;
	TTabControl *TabControl1;
	TTabItem *TabItemList;
	TListView *ListView1;
	TLayout *Layout5;
	TToolBar *ToolBar4;
	TButton *RefreshList;
	TTabItem *TabItemDetails;
	TEdit *EditTitle;
	TMemo *MemoDescription;
	TLayout *Layout3;
	TToolBar *ToolBar3;
	TButton *Button2;
	TButton *Button3;
	TTabItem *TabItemAdd;
	TLayout *Layout1;
	TToolBar *BottomToolbar;
	TButton *ButtonSaveAdd;
	TButton *ButtonCancelAdd;
	TMemo *MemoAddDescription;
	TEdit *EditAddTitle;
	TTabItem *TabItemEdit;
	TEdit *EditEditTitle;
	TLayout *Layout2;
	TToolBar *ToolBar2;
	TButton *ButtonEditCancel;
	TButton *ButtonEditSave;
	TMemo *MemoEditContent;
	TPrototypeBindSource *PrototypeBindSource1;
	TBindingsList *BindingsList1;
	TLinkControlToField *LinkControlToFieldTitle;
	TLinkControlToField *LinkControlToFieldDescription;
	TLinkListControlToField *LinkListControlToField1;
	TLinkControlToField *LinkControlToField1;
	TLinkControlToField *LinkControlToField2;
	TActionList *ActionList1;
	TAction *ActionAdd;
	TAction *ActionAddSave;
	TAction *ActionAddCancel;
	TAction *ActionEditSave;
	TAction *ActionEditCancel;
	TAction *ActionEdit;
	TAction *ActionBack;
	TAction *ActionDelete;
	TAction *ActionLabel;
	TAction *ActionRefresh;
	TAction *ActionNext;
	TAction *ActionPrior;
	void __fastcall ActionAddExecute(TObject *Sender);
	void __fastcall ActionAddUpdate(TObject *Sender);
	void __fastcall ActionAddSaveExecute(TObject *Sender);
	void __fastcall ActionEditSaveExecute(TObject *Sender);
	void __fastcall ActionEditSaveUpdate(TObject *Sender);
	void __fastcall ActionEditCancelExecute(TObject *Sender);
	void __fastcall ActionEditExecute(TObject *Sender);
	void __fastcall ActionEditUpdate(TObject *Sender);
	void __fastcall ActionBackExecute(TObject *Sender);
	void __fastcall ActionBackUpdate(TObject *Sender);
	void __fastcall ActionDeleteExecute(TObject *Sender);
	void __fastcall ActionLabelUpdate(TObject *Sender);
	void __fastcall ActionRefreshExecute(TObject *Sender);
	void __fastcall ActionNextExecute(TObject *Sender);
	void __fastcall ActionNextUpdate(TObject *Sender);
	void __fastcall ActionPriorExecute(TObject *Sender);
	void __fastcall ActionPriorUpdate(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall PrototypeBindSource1CreateAdapter(TObject *Sender, TBindSourceAdapter *&ABindSourceAdapter);
	void __fastcall ListView1ItemClick(const TObject *Sender, const TListViewItem *AItem);
	void __fastcall ActionAddCancelExecute(TObject *Sender);
private:
        std::stack <TView> FViewStack;
	TBindSourceAdapter * FBindSourceAdapter;
	TBindSourceAdapter * __fastcall GetAdapter();
	void __fastcall ShowView(TView AView);
	void __fastcall AddItem(const String ATitle, const String AContent);
	void __fastcall ShowNavigation(const TAction *AAction);
	TBindSourceAdapterField* __fastcall GetContentField();
	TBindSourceAdapterField* __fastcall GetTitleField();
	void __fastcall PushView(const TView AView);
	void __fastcall PopView();
	TView __fastcall CurrentView();
	void __fastcall DeleteEvent(const System::Uitypes::TModalResult AResult);
	void __fastcall CancelEvent(const System::Uitypes::TModalResult AResult);
public:		// User declarations
	__fastcall TKinveyToDoList(TComponent* Owner);
	__fastcall ~TKinveyToDoList();
};

typedef void __fastcall (__closure *TSimpleDialogEvent)(const System::Uitypes::TModalResult AResult);

class TMessageDlgCallback : public TInterfacedObject, public TInputCloseDialogProc
{
private:
	TSimpleDialogEvent dialogEvent;
public:
	TMessageDlgCallback(TSimpleDialogEvent event) {
		dialogEvent = event;
	}
	// TInterfacedObject methods
	HRESULT STDMETHODCALLTYPE QueryInterface(REFIID riid, void** ppvObject) {
		return TInterfacedObject::QueryInterface(riid, ppvObject);
	}

	ULONG STDMETHODCALLTYPE AddRef() {
		return TInterfacedObject::_AddRef();
	}

	ULONG STDMETHODCALLTYPE Release() {
		return TInterfacedObject::_Release();
	}
	// TInputCloseDialogProc
	void __fastcall Invoke(const System::Uitypes::TModalResult AResult) {
		if (dialogEvent != NULL)
			dialogEvent(AResult);
	}
};
//---------------------------------------------------------------------------
extern PACKAGE TKinveyToDoList *KinveyToDoList;
//---------------------------------------------------------------------------
#endif
