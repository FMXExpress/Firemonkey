//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "Unit1.h"
#include "RandomTextUtils.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
#pragma resource ("*.LgXhdpiPh.fmx", _PLAT_ANDROID)

TForm1 *Form1;

const int TotalListItems = 20;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ListView1PullRefresh(TObject *Sender)
{
	TListViewItem * Item = ListView1->Items->Insert(0);
	Item->Text = this->getRandomText();
	Item->Height = 56;

	if(ListView1->Items->Count > TotalListItems) {
        ListView1->Items->Delete(ListView1->Items->Count - 1);
    }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
	for (int i = 0; i < TotalListItems; i++) {
		TListViewItem * item = ListView1->Items->Add();
		item->Text = this->getRandomText();
		item->Height = 56;
	}
}
//---------------------------------------------------------------------------
String __fastcall TForm1::getRandomText()
{
	return CommonNames[Random(20)] + " " + CommonSurNames[Random(20)] +
		" (" + SampleTopics[Random(10)] + ")";
}
//---------------------------------------------------------------------------
