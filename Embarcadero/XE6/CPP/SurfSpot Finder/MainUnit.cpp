
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
#pragma hdrstop

#include "MainUnit.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TForm5 *Form5;
//---------------------------------------------------------------------------
__fastcall TForm5::TForm5(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm5::FormCreate(TObject *Sender)
{
	RESTRequest1->Execute;
	TabControl1->ActiveTab = TabItem1;
}
//---------------------------------------------------------------------------

void __fastcall TForm5::ListView1ItemClick(const TObject *Sender, const TListViewItem *AItem)
{
	String LGoogleMapsURL = "https://maps.google.com/maps?q=%s,%s";
	TVarRec vr[] = {LatitudeItem->Text,LongitudeItem->Text};
	System::WideChar LDecimalSeparator = FormatSettings.DecimalSeparator;
	FormatSettings.DecimalSeparator = '.';
	// Displays location on the map based on listbox item’s latitude and longitude value from REST service
	WebBrowser1->Navigate(Format(LGoogleMapsURL, vr, 2));
	FormatSettings.DecimalSeparator = LDecimalSeparator;
	//Animate from tab 1 to tab 2 and set tab 2 as the active tab
	ChangeTabAction1->Tab = TabItem2;
	ChangeTabAction1->ExecuteTarget(this);
}
//---------------------------------------------------------------------------

void __fastcall TForm5::FormShow(TObject *Sender)
{
	//Execute REST request at runtime
	RESTRequest1->Execute;
}
//---------------------------------------------------------------------------

