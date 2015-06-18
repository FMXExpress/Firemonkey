//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "UFrmHome.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
#pragma resource ("*.NmXhdpiPh.fmx", _PLAT_ANDROID)

TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::PushEvents1DeviceTokenReceived(TObject *Sender)
{
//ListBox1->Items->Add("El token se ha recibido");
}
//---------------------------------------------------------------------------

void __fastcall TForm1::PushEvents1DeviceTokenRequestFailed(
TObject *Sender, const UnicodeString AErrorMessage)

{
 ListBox1->Items->Add("Error:" + AErrorMessage);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::PushEvents1PushReceived(TObject *Sender,
 TPushData * const AData)

{
  TListBoxItem *i = new TListBoxItem(ListBox1);
  i->Text =  AData->Message;

  ListBox1->AddObject(i);
  // ListBox1->Items->Add("Mensaje Recibido:" + AData->Message);

}
//---------------------------------------------------------------------------
