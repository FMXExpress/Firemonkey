//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "BaaS_ToDoForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TKinveyToDoList *KinveyToDoList;
//---------------------------------------------------------------------------
__fastcall TKinveyToDoList::TKinveyToDoList(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TKinveyToDoList::DeleteEvent(const System::Uitypes::TModalResult AResult)
{
	struct TCloseDialogHandler : public TCppInterfacedObject<TInputCloseDialogProc> {
	  System::Uitypes::TModalResult CallbackResult;
	  void __fastcall Invoke(const System::Uitypes::TModalResult AResult) {
	  //	  CallbackResult = AResult; - no return value needed here
	  }
	};
	_di_TInputCloseDialogProc handler = new TCloseDialogHandler();

	if (AResult == mrOk) {
		String LTitle = GetTitleField()->GetTValue().ToString();
		FBindSourceAdapter->Delete();
		MessageDlg(Format("\"%s\" deleted", ARRAYOFCONST((LTitle))), TMsgDlgType::mtInformation,
			TMsgDlgButtons() << TMsgDlgBtn::mbOK, 0, handler);
		if(FBindSourceAdapter->ItemIndex == -1) {
			// No more records
			ShowView(TView::List);
		}
	}
}
//---------------------------------------------------------------------------
void __fastcall TKinveyToDoList::CancelEvent(const System::Uitypes::TModalResult AResult)
{
	if (AResult == mrOk) {
		FBindSourceAdapter->Cancel();
		PopView();
	}
}
//---------------------------------------------------------------------------
void __fastcall TKinveyToDoList::ShowView(TView AView)
{
	switch (AView) {
		case TView::List:
			this->TabControl1->ActiveTab = TabItemList;
			break;
		case TView::Details:
			this->TabControl1->ActiveTab = TabItemDetails;
			break;
		case TView::Add:
			this->TabControl1->ActiveTab = TabItemAdd;
			break;
		case TView::Edit:
			this->TabControl1->ActiveTab = TabItemEdit;
			break;
	default:
		throw System::Sysutils::Exception("Unexpected");
		;
	}
}
//---------------------------------------------------------------------------
__fastcall TKinveyToDoList::~TKinveyToDoList()
{
}
//---------------------------------------------------------------------------
TView __fastcall TKinveyToDoList::CurrentView()
{
	TTabItem * actTab = this->TabControl1->ActiveTab;

	if(actTab == TabItemAdd) {
		return TView::Add;
	}
	else if(actTab == TabItemList){
		return TView::List;
	}
	else if(actTab == TabItemEdit){
		return TView::Edit;
	}
	else if(actTab == TabItemDetails){
		return TView::Details;
	}
	else {
		throw System::Sysutils::Exception("Unexpected");
	}
}
//---------------------------------------------------------------------------
void __fastcall TKinveyToDoList::PushView(const TView AView)
{
	FViewStack.push(CurrentView());
	ShowView(AView);
}
//---------------------------------------------------------------------------
void __fastcall TKinveyToDoList::PopView()
{
	if(!FViewStack.empty()) {
        	TView view = FViewStack.top();
                FViewStack.pop();
		ShowView(view);
	}
}
//---------------------------------------------------------------------------
TBindSourceAdapterField* __fastcall  TKinveyToDoList::GetContentField()
{

	TBindSourceAdapterField* _return = GetAdapter()->FindField(TToDoNames::ContentProperty);
	if(_return == NULL) {
		throw System::Sysutils::Exception("Field not found");
	}
	return _return;
}
//---------------------------------------------------------------------------
TBindSourceAdapterField* __fastcall TKinveyToDoList::GetTitleField()
{
	TBindSourceAdapterField* _return = GetAdapter()->FindField(TToDoNames::TitleProperty);
	if(_return == NULL) {
		throw System::Sysutils::Exception("Field not found");
	}
	return _return;
}
//---------------------------------------------------------------------------
void __fastcall TKinveyToDoList::ActionAddExecute(TObject *Sender)
{
	PushView(TView::Add);
}
//---------------------------------------------------------------------------

void __fastcall TKinveyToDoList::ActionAddUpdate(TObject *Sender)
{
	switch (CurrentView()) {
		case TView::List:
			static_cast<TAction*>(Sender)->Visible = true;
			break;
	default:
		static_cast<TAction*>(Sender)->Visible = false;
		;
	}
}
//---------------------------------------------------------------------------
void __fastcall TKinveyToDoList::AddItem(const String ATitle, const String AContent)
{
	TBindSourceAdapter * LAdapter = GetAdapter();
	LAdapter->Append();

	GetTitleField()->SetTValue(TValue::_op_Implicit(ATitle));
	GetContentField()->SetTValue(TValue::_op_Implicit(AContent));
	LAdapter->Post();
}
//---------------------------------------------------------------------------
void __fastcall TKinveyToDoList::ActionAddSaveExecute(TObject *Sender)
{
	struct TCloseDialogHandler : public TCppInterfacedObject<TInputCloseDialogProc> {
	  System::Uitypes::TModalResult CallbackResult;
	  void __fastcall Invoke(const System::Uitypes::TModalResult AResult) {
	  //	  CallbackResult = AResult; - no return value needed here
	  }
	};
	_di_TInputCloseDialogProc handler = new TCloseDialogHandler();

	AddItem(EditAddTitle->Text, MemoAddDescription->Text);
	MessageDlg(Format("\"%s\" added", ARRAYOFCONST((GetTitleField()->GetTValue().ToString()))),
		TMsgDlgType::mtInformation, TMsgDlgButtons() << TMsgDlgBtn::mbOK, 0, handler);
	PopView();
	EditAddTitle->Text = "";
	MemoAddDescription->Text = "";
}
//---------------------------------------------------------------------------

void __fastcall TKinveyToDoList::ActionEditSaveExecute(TObject *Sender)
{
	struct TCloseDialogHandler : public TCppInterfacedObject<TInputCloseDialogProc> {
	  System::Uitypes::TModalResult CallbackResult;
	  void __fastcall Invoke(const System::Uitypes::TModalResult AResult) {
	  //	  CallbackResult = AResult; - no return value needed here
	  }
	};
	_di_TInputCloseDialogProc handler = new TCloseDialogHandler();
	FBindSourceAdapter->Post();
	MessageDlg(Format("\"%s\" saved", ARRAYOFCONST((GetTitleField()->GetTValue().ToString()))),
		TMsgDlgType::mtInformation, TMsgDlgButtons() << TMsgDlgBtn::mbOK, 0, handler);
	PopView();
}
//---------------------------------------------------------------------------

void __fastcall TKinveyToDoList::ActionEditSaveUpdate(TObject *Sender)
{
	static_cast<TAction*>(Sender)->Enabled = GetAdapter()->Modified;
}
//---------------------------------------------------------------------------

void __fastcall TKinveyToDoList::ActionEditCancelExecute(TObject *Sender)
{
	if (FBindSourceAdapter->Modified) {
		MessageDlg("Cancel changes?", TMsgDlgType::mtConfirmation, TMsgDlgButtons() << TMsgDlgBtn::mbOK << TMsgDlgBtn::mbCancel,
			0, new TMessageDlgCallback(&CancelEvent));
	} else {
		CancelEvent(TModalResult(mrOk));
	}
}
//---------------------------------------------------------------------------

void __fastcall TKinveyToDoList::ActionEditExecute(TObject *Sender)
{
	PushView(TView::Edit);
}
//---------------------------------------------------------------------------

void __fastcall TKinveyToDoList::ActionEditUpdate(TObject *Sender)
{
	switch (CurrentView()) {
		case TView::Details:
			static_cast<TAction*>(Sender)->Visible = true;
			break;
	default:
		static_cast<TAction*>(Sender)->Visible = false;
		;
	}
}
//---------------------------------------------------------------------------

void __fastcall TKinveyToDoList::ActionBackExecute(TObject *Sender)
{
	if(!FViewStack.empty()) {
		PopView();
	} else {
            	ShowView(TView::List);
	}
}
//---------------------------------------------------------------------------

void __fastcall TKinveyToDoList::ActionBackUpdate(TObject *Sender)
{
	switch (CurrentView()) {
		case TView::Details:
			static_cast<TAction*>(Sender)->Text = "List";
			static_cast<TAction*>(Sender)->Visible = true;
			break;
	default:
		static_cast<TAction*>(Sender)->Visible = false;
		;
	}
}
//---------------------------------------------------------------------------

void __fastcall TKinveyToDoList::ActionDeleteExecute(TObject *Sender)
{
	String LTitle = GetTitleField()->GetTValue().ToString();
	MessageDlg(Format("Delete \"%s\"?", ARRAYOFCONST((LTitle))), TMsgDlgType::mtInformation,
		TMsgDlgButtons() << TMsgDlgBtn::mbOK << TMsgDlgBtn::mbCancel , 0, new TMessageDlgCallback(&DeleteEvent));
}
//---------------------------------------------------------------------------
void __fastcall TKinveyToDoList::ActionLabelUpdate(TObject *Sender)
{
	switch (CurrentView()) {
		case TView::List:
			static_cast<TAction*>(Sender)->Text = "To Do List";
			break;
		case TView::Details:
			static_cast<TAction*>(Sender)->Text = "To Do Item";
			break;
		case TView::Add:
			static_cast<TAction*>(Sender)->Text = "Add To Do List";
			break;
		case TView::Edit:
			static_cast<TAction*>(Sender)->Text = "Edit To Do List";
			break;
	}
}
//---------------------------------------------------------------------------

void __fastcall TKinveyToDoList::ActionRefreshExecute(TObject *Sender)
{
	DataModule1->RefreshAdapter();
	DataModule1->ItemAdapter->Active = true;
}
//---------------------------------------------------------------------------


void __fastcall TKinveyToDoList::ActionNextExecute(TObject *Sender)
{
	GetAdapter()->Next();
}
//---------------------------------------------------------------------------
void __fastcall TKinveyToDoList::ActionNextUpdate(TObject *Sender)
{
	TAction * act = static_cast<TAction*>(Sender);
	ShowNavigation(act);
	if(act->Visible) {
		act->Enabled = ! GetAdapter()->Eof;
    }
}
//---------------------------------------------------------------------------

void __fastcall TKinveyToDoList::ActionPriorExecute(TObject *Sender)
{
	GetAdapter()->Prior();
}
//---------------------------------------------------------------------------
TBindSourceAdapter* __fastcall TKinveyToDoList::GetAdapter()
{
	return this->PrototypeBindSource1->InternalAdapter;
}
//---------------------------------------------------------------------------
void __fastcall TKinveyToDoList::ShowNavigation(const TAction *AAction)
{
	switch (CurrentView()) {
		case TView::Details :
			const_cast<TAction*>(AAction)->Visible = true;
			break;
	default:
		const_cast<TAction*>(AAction)->Visible = false;
		;
	}
}
//---------------------------------------------------------------------------
void __fastcall TKinveyToDoList::ActionPriorUpdate(TObject *Sender)
{
	ShowNavigation(static_cast<TAction*>(Sender));
	if(static_cast<TAction*>(Sender)->Visible) {
		static_cast<TAction*>(Sender)->Enabled = !GetAdapter()->BOF;
	}
}
//---------------------------------------------------------------------------
void __fastcall TKinveyToDoList::FormCreate(TObject *Sender)
{
	TabControl1->ActiveTab = TabItemList;
	TabControl1->TabPosition = TTabPosition::None;
	DataModule1->RefreshAdapter();
	DataModule1->ItemAdapter->Active = true;
}
//---------------------------------------------------------------------------

void __fastcall TKinveyToDoList::PrototypeBindSource1CreateAdapter(TObject *Sender,
          TBindSourceAdapter *&ABindSourceAdapter)
{
	FBindSourceAdapter = DataModule1->ItemAdapter;
	ABindSourceAdapter = FBindSourceAdapter;
}
//---------------------------------------------------------------------------

void __fastcall TKinveyToDoList::ListView1ItemClick(const TObject *Sender, const TListViewItem *AItem)
{
	PushView(TView::Details);
}
//---------------------------------------------------------------------------


void __fastcall TKinveyToDoList::ActionAddCancelExecute(TObject *Sender)
{
	PopView();
	EditAddTitle->Text = "";
	MemoAddDescription->Text = "";
}
//---------------------------------------------------------------------------

