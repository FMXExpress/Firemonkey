// ---------------------------------------------------------------------------

#include <fmx.h>
#include <System.IOUtils.hpp>
#include <System.SysUtils.hpp>
#pragma hdrstop

#include "PlayAudioFile.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TAudioPlayBackForm *AudioPlayBackForm;

// ---------------------------------------------------------------------------
__fastcall TAudioPlayBackForm::TAudioPlayBackForm(TComponent* Owner)
	: TForm(Owner) {
}

// ---------------------------------------------------------------------------
void __fastcall TAudioPlayBackForm::btnPlayClick(TObject *Sender) {
	/* Under Project-Deployment, we added the media file and set the remote path.
	 When the program starts, everything in the directory that is set under remote path is
	 copied over to the Documents folder on the device. The MediaPlayer loads the file from
	 the Documents folder */
#if defined(TARGET_OS_IPHONE) || defined(TARGET_IPHONE_SIMULATOR)
	MediaPlayer1->FileName =
		IncludeTrailingPathDelimiter(System::Ioutils::TPath::GetDocumentsPath()) +
		"soundsample.mp3";
#else
	MediaPlayer1->FileName = "soundsample.mp3";
#endif
	MediaPlayer1->Play();
}
// ---------------------------------------------------------------------------
void __fastcall TAudioPlayBackForm::btnStopClick(TObject *Sender)
{
	MediaPlayer1->Stop();
}
//---------------------------------------------------------------------------

