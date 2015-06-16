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
__fastcall TTIBLiteForm::TTIBLiteForm(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TTIBLiteForm::FireTaskListBeforeConnect(TObject *Sender)
{
	try {
	FireTaskList->Params->Values["Database"] = System::Ioutils::TPath::GetDocumentsPath() +
		PathDelim + "TASKS.GDB";
	}
	catch (Exception& E) {
		ShowMessage(E.ClassName() + ": " + E.Message);
	}
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
		if (InputQuery("Enter New Task", "Task", TaskName) || !(TaskName.Trim() == "")) {
			FDTableTask->Active = false;
			FDQueryInsert->ParamByName("TASKNAME")->AsString = TaskName;
			FDQueryInsert->ExecSQL();
			FDTableTask->Active = true;
			LinkFillControlToFieldTASKNAME->BindList->FillList();
		}
	} catch (Exception &e) {
		ShowMessage(e.Message);
	}
}
//---------------------------------------------------------------------------

void __fastcall TTIBLiteForm::FormCreate(TObject *Sender)
{
  FDTableTask->Active = True;
}
//---------------------------------------------------------------------------

