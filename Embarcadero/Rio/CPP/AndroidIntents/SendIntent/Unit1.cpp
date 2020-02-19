//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

#include <fmx.h>
#include <FMX.Platform.Android.hpp>
#include <Androidapi.JNI.JavaTypes.hpp>
#include <Androidapi.JNI.GraphicsContentViewText.hpp>
#include <Androidapi.Helpers.hpp>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
{
	sendTextViaIntent(
		"int main(int argc, void *argv)\n{\n     printf(\"Hello World\");\n     return(0)\n}\n");
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button2Click(TObject *Sender)
{
	sendTextViaIntent("#ifndef Unit1H\n#define Unit1H\n#include <System.Classes.hpp>\n\nclass Foo\n{\n};\n\n#endif\n");
}
//---------------------------------------------------------------------------

void TForm1::sendTextViaIntent(System::UnicodeString text)
{
	Androidapi::Jni::Graphicscontentviewtext::_di_JIntent intent = TJIntent::Create();
	intent->setType(StringToJString("text/cpp"));
	intent->setAction(TJIntent::JavaClass->ACTION_VIEW);
	intent->putExtra(TJIntent::JavaClass->EXTRA_TEXT, StringToJString(text));
	if (MainActivity()->getPackageManager()->queryIntentActivities(intent, TJPackageManager::JavaClass->MATCH_DEFAULT_ONLY)->size() > 0) {
		MainActivity()->startActivity(intent);
	} else {
		ShowMessage("Receiver not found");
	}
}
