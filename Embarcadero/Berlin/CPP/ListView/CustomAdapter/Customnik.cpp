//---------------------------------------------------------------------------
// Copyright (c) 2016 Embarcadero Technologies, Inc. All rights reserved.  
//
// This software is the copyrighted property of Embarcadero Technologies, Inc. 
// ("Embarcadero") and its licensors. You may only use this software if you 
// are an authorized licensee of Delphi, C++Builder or RAD Studio 
// (the "Embarcadero Products").  This software is subject to Embarcadero's 
// standard software license and support agreement that accompanied your 
// purchase of the Embarcadero Products and is considered a Redistributable, 
// as such term is defined thereunder. Your use of this software constitutes 
// your acknowledgement of your agreement to the foregoing software license 
// and support agreement. 
//---------------------------------------------------------------------------
#include <fmx.h>
#pragma hdrstop

#include "Customnik.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TForm3 *Form3;

const wchar_t *cats[] = {L"SpCbHBI.jpg", L"aMlUpJB.jpg", L"fmXnXWn.png", L"IWSnWNt.jpg",
	L"QgA69dC.png", L"snowmain.jpg"};
const wchar_t *names[] = {L"Molly", L"Charlie", L"Tigger", L"Poppy", L"Oscar", L"Smudge",
	L"Millie", L"Daisy", L"Max", L"Jasper"};
//const UnicodeString Host = "http://10.211.55.4:8000/data/";
const wchar_t *Host = L"http://i.imgur.com/";

//---------------------------------------------------------------------------
__fastcall TForm3::TForm3(TComponent* Owner)
	: TForm(Owner)
{
	FTotal = 0;
	FStrings = new TStringList();
}

void __fastcall TForm3::AddItems(const int HowMany)
{
	for(int i = 0; i < HowMany; i++) {
		UnicodeString NextItemText;
		NextItemText.sprintf(L"[%d] [%ls] [%ls%ls]", FTotal, names[FTotal % 9], Host, cats[FTotal % 4]);
		FTotal++;
		FStrings->Add(NextItemText);
	}
	FTotal++;
}
void __fastcall TForm3::ButtonClicked(TObject * Sender)
{
	TListItemTextButton *button = static_cast<TListItemTextButton*>(Sender);
	TListItem *item = static_cast<TListItem*>(button->TagObject);
	if(!item)
		return;
	TListItemText *itemText = static_cast<TListItemText*>(item->View->FindDrawable("title"));
	if(!itemText)
		return;
	ShowMessage(UnicodeString().sprintf(L"%s says Hi!", itemText->Text.c_str()));
}
//---------------------------------------------------------------------------
void __fastcall TForm3::Button1Click(TObject *Sender)
{
	AddItems(30);
}
//---------------------------------------------------------------------------
void __fastcall TForm3::FormCreate(TObject *Sender)
{
	FListView = new TPresentedListView(this);
	FListView->ControlType = TControlType::Platform;
	FStrings = new TStringList();
	FAdapter = new TCustomAdapter(FListView, FStrings);
	FAdapter->BackdropImage = BackdropSource;
	FAdapter->OnButtonClicked = &ButtonClicked;
	FListView->Adapter = FAdapter;
	FListView->Parent = Layout1;
	FListView->Align = TAlignLayout::Client;
	FListView->PullToRefresh = true;
	FListView->PullRefreshWait = true;
	FListView->OnPullRefresh = &PullRefresh;
}
//---------------------------------------------------------------------------
void __fastcall TForm3::PullRefresh(TObject *Sender)
{
	AddItems(30);
}

void __fastcall TForm3::FormDestroy(TObject *Sender)
{
	FAdapter = NULL;
	delete FListView;
	delete FStrings;
}
//---------------------------------------------------------------------------



