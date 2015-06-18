//---------------------------------------------------------------------------

#include <fmx.h>
#include <System.JSON.hpp>
#pragma hdrstop

#include "Unit2.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
#pragma resource ("*.NmXhdpiPh.fmx", _PLAT_ANDROID)
#pragma resource ("*.Windows.fmx", _PLAT_MSWINDOWS)

TForm2 *Form2;
//---------------------------------------------------------------------------
__fastcall TForm2::TForm2(TComponent* Owner)
	: TForm(Owner)
{
	//- set inital state, locked or unlocked
	RESTRequest2->Execute();
	TJSONObject * json		= (TJSONObject * )	RESTResponse2->JSONValue;
	TabControl1->TabIndex	= StrToInt( json->GetValue( "value" )->Value() );

}
//---------------------------------------------------------------------------
void __fastcall TForm2::TabControl1Change(TObject *Sender)
{
	if ( TabControl1->ActiveTab->Name == "TabItemUnlock" )
		RESTRequest1->Params->Items[4]->Value = 0;
	else
		RESTRequest1->Params->Items[4]->Value = 1;

	RESTRequest1->Execute();
}
//---------------------------------------------------------------------------
