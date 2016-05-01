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
#include <Androidapi.JNI.Os.hpp>
#include <FMX.Platform.Android.hpp>
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
void __fastcall TForm1::FormCreate(TObject *Sender)
{
	_di_IFMXApplicationEventService appsvc;
	if ((TPlatformServices::Current->SupportsPlatformService(__uuidof(IFMXApplicationEventService)) &&
		(appsvc = TPlatformServices::Current->GetPlatformService( __uuidof(IFMXApplicationEventService))))) {
		appsvc->SetApplicationEventHandler(handleAppEvent);
	}
  // Register the type of intent action that we want to be able to receive.
  // Note: A corresponding <action> tag must also exist in the <intent-filter> section of AndroidManifest.template.xml.
  MainActivity()->registerIntentAction(TJIntent::JavaClass->ACTION_VIEW);
  TMessageManager::DefaultManager->SubscribeToMessage(__classid(TMessageReceivedNotification), handleActivityMessage);
}
//---------------------------------------------------------------------------

bool __fastcall TForm1::handleAppEvent(TApplicationEvent appEvent, TObject *context)
{
	Androidapi::Jni::Graphicscontentviewtext::_di_JIntent startupIntent = MainActivity()->getIntent();
	if (startupIntent != NULL) {
		handleIntentAction(startupIntent);
	}
}

void TForm1::handleActivityMessage(System::TObject* const Sender, TMessageBase* const m)
{
	if (dynamic_cast<TMessageReceivedNotification*>(m)) {
		handleIntentAction(dynamic_cast<TMessageReceivedNotification*>(m)->Value);
	}
}

bool TForm1::handleIntentAction(Androidapi::Jni::Graphicscontentviewtext::_di_JIntent data)
{
	if (data != NULL) {
		Memo1->ClearContent();
		Androidapi::Jni::Os::_di_JBundle extras = data->getExtras();
		if (extras != NULL) {
			Memo1->Text = JStringToString(extras->getString(TJIntent::JavaClass->EXTRA_TEXT));
		}
		Invalidate();
		return(true);
	} else {
		return(false);
	}
}
