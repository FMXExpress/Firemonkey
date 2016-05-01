
//---------------------------------------------------------------------------

// This software is Copyright (c) 2014 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

#include <fmx.h>
#include <System.IOUtils.hpp>
#pragma hdrstop

#include "UMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TTIBLiteForm *TIBLiteForm;
//---------------------------------------------------------------------------
class TInputCloseQueryProcMyImpl : public TCppInterfacedObject<TInputCloseQueryProc>
{
public:
	TInputCloseQueryProcMyImpl(TTIBLiteForm *ibLiteForm) : m_ibLiteFrom(ibLiteForm) {}
	virtual void __fastcall Invoke(const System::Uitypes::TModalResult AResult, System::UnicodeString const *AValues, const int AValues_High)
	{
		String TaskName;
		if (AResult == System::Uitypes::mrOk)
			TaskName = AValues[0];
		else
			TaskName = "";
		if (!(TaskName.Trim() == "")) {
			m_ibLiteFrom->FDTableTask->Active = false;
			m_ibLiteFrom->FDQueryInsert->ParamByName("TASKNAME")->AsString = TaskName;
			m_ibLiteFrom->FDQueryInsert->ExecSQL();
			m_ibLiteFrom->FDTableTask->Active = true;
			m_ibLiteFrom->LinkFillControlToFieldTASKNAME->BindList->FillList();
		}
	}
private:
	TTIBLiteForm *m_ibLiteFrom;
};

__fastcall TTIBLiteForm::TTIBLiteForm(TComponent* Owner)
	: TForm(Owner), listQueryProc(new TInputCloseQueryProcMyImpl(this))
{
}
//---------------------------------------------------------------------------
void __fastcall TTIBLiteForm::FireTaskListBeforeConnect(TObject *Sender)
{
#if defined(TARGET_OS_IPHONE) || defined(__ANDROID__)
	try {
	FireTaskList->Params->Values["Database"] =
	  System::Ioutils::TPath::GetDocumentsPath() +
	  	PathDelim + "TASKS.GDB";
	}
	catch (Exception& E) {
		ShowMessage(E.ClassName() + ": " + E.Message);
	}
#endif
}
//---------------------------------------------------------------------------
void __fastcall TTIBLiteForm::DeleteButtonClick(TObject *Sender)
{
 String TaskName = ListViewTASKNAME->Selected->Text;
	try {
		FDTableTask->Active = false;
		FDQueryDelete->ParamByName("TASKNAME")->AsString = TaskName;
		FDQueryDelete->ExecSQL();
		FDTableTask->Active = true;
		LinkFillControlToFieldTASKNAME->BindList->FillList();
	} catch (Exception &e) {
		ShowMessage(e.Message);
	}
}
//---------------------------------------------------------------------------

void __fastcall TTIBLiteForm::AddButtonClick(TObject *Sender)
{
	String TaskName;

	try {
		String caption = "Enter New Task";
		String task[1] = { "Task" };
		const String task_name[1] = { "" };
		InputQuery(caption, task, 0, task_name, 0, listQueryProc);

	} catch (Exception &e) {
		ShowMessage(e.Message);
	}
}
//---------------------------------------------------------------------------
void __fastcall TTIBLiteForm::OnIdle(System::TObject* Sender, bool &Done)
{
	DeleteButton->Visible = (ListViewTASKNAME->Selected != NULL);
}

//---------------------------------------------------------------------------
void __fastcall TTIBLiteForm::FormCreate(TObject *Sender)
{
	Application->OnIdle = OnIdle;
}
//---------------------------------------------------------------------------

