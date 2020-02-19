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
void __fastcall TForm4::TextEditChangeTracking(TObject *Sender)
{
	TextControl->Text = TextEdit->Text;
}
//---------------------------------------------------------------------------
void __fastcall TForm4::TextControlMouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, float X, float Y)
{
	_di_IFMXDragDropService service;
	if(!TextEdit->Text.IsEmpty() &&
		(TPlatformServices::Current->SupportsPlatformService(__uuidof(IFMXDragDropService)) &&
		(service = TPlatformServices::Current->GetPlatformService(__uuidof(IFMXDragDropService)))))
	{
		TDragObject dragData;
		dragData.Source = Sender;
		std::unique_ptr<TBitmap> dragImage(Rectangle1->MakeScreenshot());
		if(TextRadioButton->IsChecked) {
			dragData.Data = TValue::_op_Implicit(TextEdit->Text);
		}
		else {
			dragData.Data = dragImage.get();
		}
		service->BeginDragDrop(this, dragData, dragImage.get());
    }
}
//---------------------------------------------------------------------------
