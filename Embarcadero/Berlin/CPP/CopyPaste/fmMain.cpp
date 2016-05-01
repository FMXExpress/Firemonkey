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

#include "fmMain.h"
#include <memory>
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
void __fastcall TForm4::CopyButtonClick(TObject *Sender)
{
	_di_IFMXClipboardService service;
	if(TPlatformServices::Current->SupportsPlatformService(__uuidof(IFMXClipboardService)) &&
		(service = TPlatformServices::Current->GetPlatformService(__uuidof(IFMXClipboardService)))) {
		if(TextRadioButton->IsChecked) {
			service->SetClipboard(TValue::_op_Implicit(Edit1->Text));
		}
		else {
			std::unique_ptr<TBitmap> Image(TextBorder->MakeScreenshot());
			service->SetClipboard(TValue::_op_Implicit(Image.get()));
        }
	}
}
//---------------------------------------------------------------------------
void __fastcall TForm4::Button1Click(TObject *Sender)
{
	_di_IFMXClipboardService service;
	if(TPlatformServices::Current->SupportsPlatformService(__uuidof(IFMXClipboardService)) &&
		(service = TPlatformServices::Current->GetPlatformService(__uuidof(IFMXClipboardService)))) {
		TValue value = service->GetClipboard();
		if(!value.IsEmpty) {
			if(value.IsType<String>()) {
				PasteLabel->Text = value.ToString();
				PasteImage->Bitmap = NULL;
			}
			else if (value.IsType<TBitmapSurface*>()) {
				PasteLabel->Text = EmptyStr;
				std::unique_ptr<TBitmap> bitmap(new TBitmap());
				bitmap->Assign(value.AsType<TBitmapSurface*>());
				PasteImage->Bitmap = bitmap.get();
				value.AsType<TBitmapSurface*>()->Free();
			}
		}
	}
}
//---------------------------------------------------------------------------
void __fastcall TForm4::Edit1ChangeTracking(TObject *Sender)
{
	CopyButton->Enabled = !Edit1->Text.IsEmpty();
	TextControl->Text = Edit1->Text;
}
//---------------------------------------------------------------------------
