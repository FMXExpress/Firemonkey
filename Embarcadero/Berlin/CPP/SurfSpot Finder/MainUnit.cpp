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

#include "MainUnit.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TForm4 *Form4;
//---------------------------------------------------------------------------
__fastcall TForm4::TForm4(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm4::FormShow(TObject *Sender)
{
	//Execute REST request at runtime
	RESTRequest1->Execute();
}
//---------------------------------------------------------------------------
void __fastcall TForm4::UpdateWebBrowserVisibility()
{
    WebBrowser1->Visible = ! MultiView1->MasterContent->AbsoluteRect.IntersectsWith(WebBrowser1->AbsoluteRect);
}

void __fastcall TForm4::ListView1ItemClick(TObject * const Sender, TListViewItem * const AItem)
{
	String LGoogleMapsURL = "https://maps.google.com/maps?q=%s,%s";
	TVarRec vr[] = {LatitudeItem->Text, LongitudeItem->Text};
	System::WideChar LDecimalSeparator = FormatSettings.DecimalSeparator;
	FormatSettings.DecimalSeparator = '.';
	// Displays location on the map based on listbox item's latitude and longitude value from REST service
	WebBrowser1->Navigate(Format(LGoogleMapsURL, vr, 2));
	FormatSettings.DecimalSeparator = LDecimalSeparator;
	//Animate from tab 1 to tab 2 and set tab 2 as the active tab
	MultiView1->HideMaster();
}
//---------------------------------------------------------------------------
void __fastcall TForm4::MultiView1Paint(TObject *Sender, TCanvas *Canvas, const TRectF &ARect)

{
    UpdateWebBrowserVisibility();
}
//---------------------------------------------------------------------------
void __fastcall TForm4::MultiView1Hidden(TObject *Sender)
{
    WebBrowser1->Visible = true;
}
//---------------------------------------------------------------------------
