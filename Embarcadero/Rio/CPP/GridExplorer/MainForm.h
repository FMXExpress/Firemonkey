//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef MainFormH
#define MainFormH
// ---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Dialogs.hpp>
#include <FMX.Edit.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.Objects.hpp>
#include <FMX.Types.hpp>
#include <FMX.ExtCtrls.hpp>
#include <FMX.TreeView.hpp>
#include <System.IOUtils.hpp>
#include <FMX.Grid.hpp>
#include <System.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.Menus.hpp>
#include <FMX.StdCtrls.hpp>
#include <System.Rtti.hpp>
#include <FMX.Controls.Presentation.hpp>

/// <summary> A no-arg procedure which will tell the UI to update its user list. </summary>
typedef void __fastcall(__closure * TUIHookCallback)(void);

class TFileInfo : public System::TObject {
private:
	String FFileName;
	String FFolder;
	Longint FFolderSize;

public:
	__fastcall TFileInfo();
	__fastcall TFileInfo(const String AFolder, const String AFileName, const int AFolderSize);
	__fastcall TFileInfo(const String AFolder, const String AFileName);
	String __fastcall ToString();
	int __fastcall Size();
	__property String FileName = {read = FFileName};
	__property String Folder = {read = FFolder};
	__property Longint FolderSize = {read = FFolderSize, write = FFolderSize};
};

// ---------------------------------------------------------------------------
class TFrmMain : public TForm {
__published: // IDE-managed Components

	TEdit *edPath;
	TOpenDialog *OpenDialog1;
	TScaledLayout *ScaledLayout1;
	TRectangle *Rectangle1;
	TStyleBook *StyleBook1;
	TTreeView *TreeView1;
	TTreeViewItem *TreeViewItem1;
	TTreeViewItem *TreeViewItem2;
	TTreeViewItem *TreeViewItem3;
	TTreeViewItem *TreeViewItem4;
	TTreeViewItem *TreeViewItem5;
	TTreeViewItem *TreeViewItem6;
	TSplitter *Splitter1;
	TGrid *Grid1;
	TColumn *ColumnName;
	TColumn *ColumnSize;
	TColumn *ColumnModifiedDate;
	TColumn *ColumnCreatedDate;
	TLayout *Layout1;
	TListBox *ListBox1;
	TListBoxItem *ListBoxItem1;
	TListBoxItem *ListBoxItem2;
	TListBoxItem *ListBoxItem3;
	TListBoxItem *ListBoxItem4;
	TListBoxItem *ListBoxItem5;
	TLayout *Layout2;
	TLabel *Label2;
	TLabel *Label3;
	TLabel *Label4;
	TLayout *Layout3;
	TLabel *Label5;
	TLabel *Label6;
	TLabel *Label7;
	TLayout *Layout4;
	TLabel *Label8;
	TLabel *Label9;
	TLabel *Label10;
	TLayout *Layout5;
	TLabel *Label11;
	TLabel *Label12;
	TLabel *Label13;
	TListBoxItem *ListBoxItem6;
	TStyleBook *StyleBook2;
	TPopupMenu *PopupMenu1;
	TMenuItem *MenuItem1;
	TMenuItem *MenuItem2;
	TProgressColumn *ColumnFolderSize;
	TText *Text1;
	TSpeedButton *SpeedButton1;

	void __fastcall edPathKeyUp(TObject *Sender, WORD &Key,
		System::WideChar &KeyChar, TShiftState Shift);
	void __fastcall TreeView1Change(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall Grid1SetValue(TObject *Sender, const int Col, const int Row,
		  const TValue &Value);
	void __fastcall Grid1GetValue(TObject *Sender, const int Col, const int Row,
		  TValue &Value);
	void __fastcall SpeedButton1Click(TObject *Sender);

private: // User declarations

	TUIHookCallback FQueueHook;

public: // User declarations
	DynamicArray<TFileInfo*>Data;
	bool UpdateGrid;

	__property TUIHookCallback UIHook = {read = FQueueHook, write = FQueueHook};

	__fastcall TFrmMain(TComponent* Owner);
	void __fastcall FileSearch(String Dir, String SearchPattern, TGrid* Grid);
	void __fastcall UpdateFolderList();

};

/*
 **************************************************************
 TFlowFilterPredicate - defines a class containing the
 TFilterPredicate callback to retrieve the OS image file
 extensions, used here to validate the user-selected file.
 **************************************************************
 */

class TFlowFilterPredicate
	: public TCppInterfacedObject<TDirectory::TFilterPredicate> {
public:
	bool __fastcall Invoke(const System::UnicodeString Path,
		const System::Sysutils::TSearchRec &SearchRec) {
		bool filterResult;
		return (!(SearchRec.Attr & faSymLink));
	}
};

// Interface for thread callback
class TExplorerQueueProc : public TCppInterfacedObject<TThreadProcedure> {
private:
	TUIHookCallback FUIHook;

public:
	TExplorerQueueProc(TUIHookCallback& AUIHook) : FUIHook(AUIHook) {
	}

	__fastcall ~TExplorerQueueProc() {
	}

	virtual void __fastcall Invoke() {
		FUIHook();
	}
};

// ---------------------------------------------------------------------------
extern PACKAGE TFrmMain *FrmMain;
// ---------------------------------------------------------------------------
#endif
