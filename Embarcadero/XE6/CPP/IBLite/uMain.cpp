
//---------------------------------------------------------------------------

// This software is Copyright (c) 2014 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
// ---------------------------------------------------------------------------

#include <fmx.h>
#include <System.IOUtils.hpp>
#pragma hdrstop

#include "uMain.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TIBLiteForm *IBLiteForm;

// ---------------------------------------------------------------------------
__fastcall TIBLiteForm::TIBLiteForm(TComponent* Owner) : TForm(Owner) {
}

// ---------------------------------------------------------------------------
void __fastcall TIBLiteForm::OnIdle(TObject* Sender, bool &Done) {
	DeleteButton->Visible = ListView1->Selected;
}

// ---------------------------------------------------------------------------

void __fastcall TIBLiteForm::AddButtonClick(TObject *Sender) {
	String TaskName;
	try {
		if (InputQuery("Enter New Task", "Task", TaskName) && !(Trim(TaskName) == "")) {
			SQLQueryInsert->ParamByName("TaskName")->AsString = TaskName;
			SQLQueryInsert->ExecSQL();
			SQLDataSetTask->Refresh();
			LinkFillControlToField1->BindList->FillList();
		}
	}
	catch (Exception &e) {
		ShowMessage(e.Message);
	}
}

// ---------------------------------------------------------------------------
void __fastcall TIBLiteForm::DeleteButtonClick(TObject *Sender) {
	String TaskName = ListView1->Selected->Text;
	try {
		SQLQueryDelete->ParamByName("TaskName")->AsString = TaskName;
		SQLQueryDelete->ExecSQL();
		SQLDataSetTask->Refresh();
		LinkFillControlToField1->BindList->FillList();
		if ((ListView1->Selected) && (ListView1->Items->Count > 0))
			// Select last item
				ListView1->ItemIndex = ListView1->Items->Count - 1;
	}
	catch (Exception &e) {
		ShowMessage(e.Message);
	}
}

// ---------------------------------------------------------------------------
void __fastcall TIBLiteForm::FormCreate(TObject *Sender)
{
	LinkFillControlToField1->AutoActivate = false;
	LinkFillControlToField1->AutoFill = false;
	Application->OnIdle = OnIdle;
	TaskList->Connected = true;
	SQLDataSetTask->Active = true;
	LinkFillControlToField1->BindList->FillList();
}
//---------------------------------------------------------------------------

void __fastcall TIBLiteForm::TaskListBeforeConnect(TObject *Sender)
{
#if defined(TARGET_OS_IPHONE) || defined(TARGET_IPHONE_SIMULATOR)
	TaskList->Params->Values["Database"] =
		IncludeTrailingPathDelimiter(System::Ioutils::TPath::GetDocumentsPath()) +"TASKS.GDB";

#endif
	TaskList->Params->Values["Username"] = "sysdba";
	TaskList->Params->Values["Password"] = "masterkey";
	TaskList->Params->Values["ServerCharSet"] = "UTF8";
}
//---------------------------------------------------------------------------

