//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "MainForm.h"
#include "sysdyn.h"
#include "System.Classes.hpp"

// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
#pragma resource ("*.Windows.fmx", _PLAT_MSWINDOWS)

TFrmMain *FrmMain;

// ---------------------------------------------------------------------------
void __fastcall DirSearch(String Dir, String SearchPattern,
	TSearchOption Option, TTreeViewItem* Node) {


	TDirectory::_di_TFilterPredicate predicate(new TFlowFilterPredicate());
	System::TStringDynArray list =
		TDirectory::GetDirectories(Dir, Option, predicate);

	for (int i = 0; i < list.Length; i++) {
		TTreeViewItem *t = new TTreeViewItem(Node);
		t->Text = ExtractFileName(list[i]);
		t->TagString = list[i];
		t->Parent = Node;
		DirSearch(list[i], "*", TSearchOption::soTopDirectoryOnly, t);
	}
}

String __fastcall FormatByteSize(const Longint bytes) {
	int B = 1; // byte
	int KB = 1024 * B; // kilobyte
	int MB = 1024 * KB; // megabyte
	int GB = 1024 * MB; // gigabyte

	if (bytes > GB) {
		return FormatFloat("#.## GB", bytes / GB);
	}
	else {
		if (bytes > MB) {
			return FormatFloat("#.## MB", bytes / MB);
		}
		else {
			if (bytes > KB) {
				return FormatFloat("#.## KB", bytes / KB);
			}
			else {
				return FormatFloat("#.## bytes", bytes);
			}
		}
	}
}

__fastcall TFileInfo::TFileInfo() {

}

String __fastcall TFileInfo::ToString() {
	return Folder + FileName;
}

__fastcall TFileInfo::TFileInfo(String AFolder, String AFileName) {
	FFileName = AFileName;
	FFolder = AFolder;
}

__fastcall TFileInfo::TFileInfo(String AFolder, String AFileName,
	int AFolderSize) {
	FFileName = AFileName;
	FFolder = AFolder;
	FFolderSize = AFolderSize;
}

int __fastcall TFileInfo::Size() {
	TSearchRec sr;
	int filesize = -1;
	if (FindFirst(ToString(), faAnyFile, sr) == 0) {
		filesize = sr.Size;
	}
	FindClose(sr);
	return filesize;
}

// ---------------------------------------------------------------------------
__fastcall TFrmMain::TFrmMain(TComponent* Owner) : TForm(Owner) {
}

void __fastcall TFrmMain::UpdateFolderList() {
	Caption = "C++Builder FireMonkey Explorer - Loading...";
	TreeView1->Clear();
	TTreeViewItem *root = new TTreeViewItem(this);
	root->Text = "Root";
	root->TagString = edPath->Text;
	TreeView1->AddObject(root);

	if ( TDirectory::Exists(edPath->Text)) {
		TreeView1->BeginUpdate();
		DirSearch(root->TagString, "*", TSearchOption::soTopDirectoryOnly, root);
		TreeView1->SetFocus();
		TreeView1->Selected = TreeView1->Items[0];
		TreeView1->Selected->IsExpanded = true;
		TreeView1->EndUpdate();
	}
	Caption = "C++Builder FireMonkey Explorer";
}

void __fastcall TFrmMain::FileSearch(String Dir, String SearchPattern,
	TGrid* Grid) {

	UpdateGrid = false;
	TDirectory::_di_TFilterPredicate predicate(new TFlowFilterPredicate());
	System::TStringDynArray files = TDirectory::GetFiles(Dir, predicate);

	Grid->RowCount = files.Length;
	Data.Length = Grid1->RowCount;

	Longint totalsize = 0;
	for (int i = 0; i < files.Length; i++) {
		TFileInfo* f = new TFileInfo(ExtractFilePath(files[i]),
			ExtractFileName(files[i]));
		totalsize += f->Size();
		Data[i] = f;
	}

	// Set the directory size in each file
	for (int i = 0; i < Data.Length; i++) {
		Data[i]->FolderSize = totalsize;
    }

}

void __fastcall TFrmMain::edPathKeyUp(TObject *Sender, WORD &Key,
	System::WideChar &KeyChar, TShiftState Shift) {

	if (Key == vkReturn) {
		TPlatformServices* ps = TPlatformServices::Current;
		_di_IFMXCursorService cs;
		ps->GetInterface(cs);
		if (cs) {
		  cs->SetCursor(System::Uitypes::crHourGlass);
		}
		UpdateFolderList();
		if (cs) {
		  cs->SetCursor(System::Uitypes::crDefault);
		}

	}
}

// ---------------------------------------------------------------------------
void __fastcall TFrmMain::TreeView1Change(TObject *Sender) {
	if (TreeView1->Selected != NULL) {

		String folder = TreeView1->Selected->TagString;

		if (TDirectory::Exists(folder)) {
			Caption = "C++Builder FireMonkey Explorer | " + folder;
			TDirectory::_di_TFilterPredicate predicate
				(new TFlowFilterPredicate());
			System::TStringDynArray files =
				TDirectory::GetFiles(folder, predicate);

			UpdateGrid = true;
			Grid1->RowCount = files.Length;
		}
	}
}

// ---------------------------------------------------------------------------
void __fastcall TFrmMain::FormCreate(TObject *Sender) {


//	edPath->Text = Ioutils::TPath::GetHomePath(); //TDirectory::GetCurrentDirectory();
//	edPath->Text = TDirectory::GetCurrentDirectory();
	edPath->SetFocus();
	UpdateGrid = false;
	TreeView1->Clear();


	UIHook = &UpdateFolderList;
}

// ---------------------------------------------------------------------------
void __fastcall TFrmMain::Grid1SetValue(TObject *Sender, const int Col, const int Row,
		  const TValue &Value) {
	TFileInfo *f;
	f = dynamic_cast<TFileInfo*>(const_cast<TValue*>(&Value)->AsObject());
	Data[Row] = f;

}

// ---------------------------------------------------------------------------
void __fastcall TFrmMain::Grid1GetValue(TObject *Sender, const int Col, const int Row,
		  TValue &Value) {
	if (UpdateGrid) {
		FileSearch(TreeView1->Selected->TagString, "*", Grid1);
	}

	// Init Data arrays
	// we not use Col - because Col return Column - but Column may realigned
	if (Row < Data.Length) {

		if (Grid1->Columns[Col] == ColumnName) {
			Value = TValue::From<String>(Data[Row]->FileName);
		}
		else if (Grid1->Columns[Col] == ColumnCreatedDate) {
			if (TFile::Exists(Data[Row]->ToString())) {
				Value = TValue::From<TDateTime>(TFile::GetCreationTime(Data[Row]->ToString()));
			}
		}
		else if (Grid1->Columns[Col] == ColumnModifiedDate) {
			if (TFile::Exists(Data[Row]->ToString())) {
				Value = TValue::From<TDateTime>(TFile::GetLastWriteTime(Data[Row]->ToString()));
			}
		}
		else if (Grid1->Columns[Col] == ColumnSize) {
			if (TFile::Exists(Data[Row]->ToString())) {

				Value = TValue::From<UnicodeString>(FormatByteSize(Data[Row]->Size()));
			}
		}
		else if (Grid1->Columns[Col] == ColumnFolderSize) {
			Value = TValue::From<double>((double)(Data[Row]->Size() / (double)
				Data[Row]->FolderSize) * 100);
		}
	}

}
// ---------------------------------------------------------------------------

void __fastcall TFrmMain::SpeedButton1Click(TObject *Sender)
{
   if ( OpenDialog1->Execute() ) {
	   edPath->Text = ExtractFilePath(OpenDialog1->FileName);
	   edPath->SetFocus();
   }
}
//---------------------------------------------------------------------------


