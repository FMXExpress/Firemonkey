
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
TSQLiteForm *SQLiteForm;

// ---------------------------------------------------------------------------
__fastcall TSQLiteForm::TSQLiteForm(TComponent* Owner) : TForm(Owner) {
}
// ---------------------------------------------------------------------------
void __fastcall TSQLiteForm::OnIdle(TObject * Sender, bool &ADone) {
	btnDelete->Visible = ListBox1->Selected;
}

// ---------------------------------------------------------------------------
void __fastcall TSQLiteForm::FormCreate(TObject *Sender) {
	try {
		// For unidirectional dataset, don't refill automatically when dataset is activated
		// because dataset is reactivated everytime use DataSet.First.
		LinkFillControlToField1->AutoActivate = false;
		LinkFillControlToField1->AutoFill = false;
		Application->OnIdle = OnIdle;
		TaskList->Connected = true;
		SQLDataSetTask->Active = true;
		LinkFillControlToField1->BindList->FillList();
	}
	catch (Exception &e) {
		ShowMessage(e.Message);
	}
}
// ---------------------------------------------------------------------------

void __fastcall TSQLiteForm::btnAddClick(TObject *Sender) {
	String TaskName;
	try {
		if ((InputQuery("Enter New Task", "Task", TaskName)) &&
			(!(Trim(TaskName) == ""))) {
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
void __fastcall TSQLiteForm::btnDeleteClick(TObject *Sender) {
	String TaskName = ListBox1->Selected->Text;
	try {
		SQLQueryDelete->ParamByName("TaskName")->AsString = TaskName;
		SQLQueryDelete->ExecSQL();
		SQLDataSetTask->Refresh();
		LinkFillControlToField1->BindList->FillList();
		if ((ListBox1->Selected) && (ListBox1->Count > 0))
			// Select last item
			ListBox1->ItemIndex = ListBox1->Count - 1;
	}
	catch (Exception &e) {
		ShowMessage(e.Message);
	}
}
// ---------------------------------------------------------------------------
void __fastcall TSQLiteForm::TaskListBeforeConnect(TObject *Sender)
{
#if defined(TARGET_OS_IPHONE) || defined(TARGET_IPHONE_SIMULATOR) || defined(__ANDROID__)
	TaskList->Params->Values["Database"] = IncludeTrailingPathDelimiter(
		System::Ioutils::TPath::GetDocumentsPath()) + "tasks.s3db";
#else
	TaskList->Params->Values["Database"] = "tasks.s3db";
#endif
}
//---------------------------------------------------------------------------


void __fastcall TSQLiteForm::TaskListAfterConnect(TObject *Sender)
{
	TaskList->ExecuteDirect("CREATE TABLE IF NOT EXISTS Task (TaskName TEXT NOT NULL)");
}
//---------------------------------------------------------------------------

