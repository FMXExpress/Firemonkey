
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
#include <System.ioutils.hpp>
#pragma hdrstop

#include "VideoForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
#pragma resource ("*.LgXhdpiPh.fmx", _PLAT_ANDROID)

TVideoPlayBackForm *VideoPlayBackForm;
//---------------------------------------------------------------------------
__fastcall TVideoPlayBackForm::TVideoPlayBackForm(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TVideoPlayBackForm::videoBtnClick(TObject *Sender)
{
  /*Under Project-Deployment, we added the media file and set the remote path.
  When the program starts, everything in the directory that is set under remote path is
  copied over to the Documents folder on the device. The MediaPlayer loads the file from
  the Documents folder*/
#if defined(TARGET_OS_IPHONE) || defined(TARGET_IPHONE_SIMULATOR) || defined(__ANDROID__)
	MediaPlayer1->FileName =
		IncludeTrailingPathDelimiter(System::Ioutils::TPath::GetDocumentsPath()) + "Ocean.mp4";
#endif
	MediaPlayer1->Play();
}
//---------------------------------------------------------------------------
