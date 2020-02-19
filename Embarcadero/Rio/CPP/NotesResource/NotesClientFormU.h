//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef NotesClientFormUH
#define NotesClientFormUH
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
#include <FMX.Controls.Presentation.hpp>
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
#include <FMX.MobilePreview.hpp>
#include <FMX.ScrollBox.hpp>
#include <FMX.ListView.Adapters.Base.hpp>
#include <FMX.ListView.Appearances.hpp>
#include <stack>
//---------------------------------------------------------------------------
enum TView {Login , List, Details, Add, Edit};

class TNotesClientForm : public TForm
{
__published:	// IDE-managed Components
	TToolBar *ToolBar1;
	TButton *ButtonAdd;
	TButton *ButtonBack;
	TLabel *Label1;
	TSpeedButton *SpeedButton2;
	TSpeedButton *SpeedButton1;
	TButton *ButtonNext;
	TButton *ButtonCancelAdd;
	TButton *ButtonSaveAdd;
	TButton *ButtonEditCancel;
	TButton *ButtonEditSave;
	TLayout *LayoutTabControl;
	TTabControl *TabControl1;
	TTabItem *TabItemLogin;
	TLayout *LayoutUserName;
	TEdit *EditPassword;
	TEdit *EditUserName;
	TLabel *Label2;
	TLabel *Label3;
	TButton *Button1;
	TButton *Button5;
	TLayout *LayoutLogout;
	TButton *Button4;
	TLabel *Label4;
	TLabel *LabelUserName;
	TTabItem *TabItemList;
	TListView *ListView1;
	TLayout *Layout5;
	TToolBar *ToolBar4;
	TButton *RefreshList;
	TTabItem *TabItemDetails;
	TEdit *EditTitle;
	TMemo *MemoDescription;
	TToolBar *ToolBar2;
	TButton *ButtonEdit;
	TButton *ButtonDelete;
	TTabItem *TabItemAdd;
	TMemo *MemoAddDescription;
	TEdit *EditAddTitle;
	TTabItem *TabItemEdit;
	TEdit *EditEditTitle;
	TMemo *MemoEditContent;
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
	TAction *ActionLogin;
	TAction *ActionLogout;
	TAction *ActionSignup;
	TAction *ActionForward;
	TPrototypeBindSource *BindSource1;
	TBindingsList *BindingsList1;
	TLinkControlToField *LinkControlToFieldTitle;
	TLinkControlToField *LinkControlToFieldDescription;
	TLinkListControlToField *LinkListControlToField1;
	TLinkControlToField *LinkControlToField1;
	TLinkControlToField *LinkControlToField2;
	TLinkControlToField *LinkControlToField3;
	TLinkControlToField *LinkControlToField4;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormVirtualKeyboardHidden(TObject *Sender, bool KeyboardVisible,
          const TRect &Bounds);
	void __fastcall FormVirtualKeyboardShown(TObject *Sender, bool KeyboardVisible,
		  const TRect &Bounds);
	void __fastcall ActionAddExecute(TObject *Sender);
	void __fastcall ActionAddUpdate(TObject *Sender);
	void __fastcall ActionAddSaveExecute(TObject *Sender);
	void __fastcall ActionAddSaveUpdate(TObject *Sender);
	void __fastcall ActionAddCancelExecute(TObject *Sender);
	void __fastcall ActionAddCancelUpdate(TObject *Sender);
	void __fastcall ActionEditSaveExecute(TObject *Sender);
	void __fastcall ActionEditSaveUpdate(TObject *Sender);
	void __fastcall ActionEditCancelExecute(TObject *Sender);
	void __fastcall ActionEditCancelUpdate(TObject *Sender);
	void __fastcall ActionEditExecute(TObject *Sender);
	void __fastcall ActionEditUpdate(TObject *Sender);
	void __fastcall ActionBackExecute(TObject *Sender);
	void __fastcall ActionBackUpdate(TObject *Sender);
	void __fastcall ActionDeleteExecute(TObject *Sender);
	void __fastcall ActionDeleteUpdate(TObject *Sender);
	void __fastcall ActionLabelExecute(TObject *Sender);
	void __fastcall ActionLabelUpdate(TObject *Sender);
	void __fastcall ActionRefreshExecute(TObject *Sender);
	void __fastcall ActionNextExecute(TObject *Sender);
	void __fastcall ActionNextUpdate(TObject *Sender);
	void __fastcall ActionPriorExecute(TObject *Sender);
	void __fastcall ActionPriorUpdate(TObject *Sender);
	void __fastcall ActionLoginExecute(TObject *Sender);
	void __fastcall ActionLoginUpdate(TObject *Sender);
	void __fastcall ActionLogoutExecute(TObject *Sender);
	void __fastcall ActionLogoutUpdate(TObject *Sender);
	void __fastcall ActionSignupExecute(TObject *Sender);
	void __fastcall ActionSignupUpdate(TObject *Sender);
	void __fastcall ActionForwardExecute(TObject *Sender);
	void __fastcall ActionForwardUpdate(TObject *Sender);
	void __fastcall BindSource1CreateAdapter(TObject *Sender, TBindSourceAdapter *&ABindSourceAdapter);
	void __fastcall ListView1ItemClick(TObject * const Sender, TListViewItem * const AItem);


private:	// User declarations
	TBindSourceAdapter * FBindSourceAdapter;
	std::stack<TView> FViewStack;
	void __fastcall ShowView(TView AView);
	TView __fastcall CurrentView(void);
	TBindSourceAdapterField* __fastcall GetTitleField(void);
	TBindSourceAdapterField* __fastcall GetContentField(void);
	void __fastcall PopView(void);
	void __fastcall PushView(const TView & AView);
	void __fastcall ShowNavigation(TAction * AAction);
	TBindSourceAdapter* __fastcall GetAdapter(void);
	void __fastcall UpdateLoginPage(void);

	void __fastcall ActionEditCancelExecuteCloseEvent(System::TObject* Sender, const System::Uitypes::TModalResult AResult);
	void __fastcall ActionDeleteExecuteCloseEvent(System::TObject* Sender, const System::Uitypes::TModalResult AResult);
public:		// User declarations
	__fastcall TNotesClientForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TNotesClientForm *NotesClientForm;
//---------------------------------------------------------------------------
#endif
