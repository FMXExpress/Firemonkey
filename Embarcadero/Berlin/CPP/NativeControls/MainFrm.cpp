// ---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "MainFrm.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TFormMain *FormMain;      
// ---------------------------------------------------------------------------

__fastcall TFormMain::TFormMain(TComponent* Owner) : TForm(Owner) {
	fMenu = new std::map<TListViewItem*, TTabItem*>();
}  
// ---------------------------------------------------------------------------

void __fastcall TFormMain::FormDestroy(TObject *Sender) {
	delete fMenu;
}   
// ---------------------------------------------------------------------------

void TFormMain::addItem(const UnicodeString text, TTabItem* tabItem, TListItemPurpose purpose) {       
	TListViewItem* item = ListViewMenu->Items->Add();
	item->Text = text;
	item->Purpose = purpose;
	if (item->Purpose == TListItemPurpose::Header) {
		item->Objects->TextObject->Font->Style =
			TFontStyles(TFontStyle::fsBold);
	}
	std::map<TListViewItem*, TTabItem*>::iterator it = fMenu->begin();
	fMenu->insert(it, std::pair<TListViewItem*, TTabItem*>(item, tabItem));
}   
// ---------------------------------------------------------------------------

void __fastcall TFormMain::FormCreate(TObject *Sender) {
	ListViewMenu->BeginUpdate();
	ListViewMenu->Items->Clear();
	try {
		addItem("Edit", NULL, TListItemPurpose::Header);
#if defined(__APPLE__)
		addItem("- Prompt", TabItemEditPrompt);
		addItem("- Color cursor", TabItemEditCursorColor);
		addItem("- Clear button", TabItemEditClearButton);
		addItem("- Custom background", TabItemEditCustomBackground);
#endif
		addItem("- Text alignment", TabItemEditTextAlign);
		addItem("- Font settings", TabItemEditFonstSettings);

		addItem("Memo", NULL, TListItemPurpose::Header);
#if defined(__APPLE__)
		addItem("- Color cursor", TabItemMemoCursorColor);
		addItem("- Detecting phones, links, address, events", TabItemMemoDetectingLinks);
		addItem("- Check spelling", TabItemMemoCheckSpelling);
		addItem("- Custom background", TabItemMemoCustomBackground);
#endif
		addItem("- Font settings", TabItemMemoFontSettings);

#if defined(__APPLE__)
		addItem("Calendar", NULL, TListItemPurpose::Header);
		addItem("- Calendar", TabItemCalendar);

		addItem("List View", NULL, TListItemPurpose::Header);
		addItem("- Headers and indicator", TabItemListView);
#endif

		addItem("ScrollBox", NULL, TListItemPurpose::Header);
		addItem("- Custom content size", TabItemScrollBox);

#if defined(__APPLE__)
		addItem("Switch", NULL, TListItemPurpose::Header);
		addItem("- Switch", TabItemSwitch);
#endif

#if defined(__APPLE__) || defined(__ANDROID__)
		addItem("Map", NULL, TListItemPurpose::Header);
		addItem("- Map", TabItemMap);
#endif

		addItem("Web Browser", NULL, TListItemPurpose::Header);
		addItem("- Web Browser", TabItemWebBrowser);
	} __finally {
		TabControl1->EndUpdate();
	}
}      
// ---------------------------------------------------------------------------

void __fastcall TFormMain::SpeedButtonBackToMenuClick(TObject *Sender) {
	TabControl1->ActiveTab = TabItemMenu;
}

// ---------------------------------------------------------------------------
void __fastcall TFormMain::ListViewMenuItemClickEx(TObject * const Sender, int ItemIndex, const TPointF &LocalClickPos,
	TListItemDrawable * const ItemObject)   
{     
	TListViewItem* item = ListViewMenu->Items->operator[](ItemIndex);
	TTabItem* tabItem = fMenu->operator[](item);
	if (tabItem != NULL) {
		TabControl1->ActiveTab = tabItem;
	}
	else {
		TabControl1->ActiveTab = TabItemMenu;
	}
}
// ---------------------------------------------------------------------------

void __fastcall TFormMain::TabControl1Change(TObject *Sender)
{
	SpeedButtonBackToMenu->Visible = TabControl1->ActiveTab != TabItemMenu;	
}
//---------------------------------------------------------------------------

