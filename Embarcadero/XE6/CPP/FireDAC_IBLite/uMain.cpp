
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



void __fastcall TTIBLiteForm::Button1Click(TObject *Sender)
{
	try
	{
		ShowMessage(L"About to open connection");
		FireTaskList->Open();
		ShowMessage(L"Connection open");
		FDTableTask->Active = True;
		ShowMessage(L"Yeah, right ..");
	}
	catch(Exception &ex)
	{
		ShowMessage(ex.Message);
    }
}
//---------------------------------------------------------------------------

