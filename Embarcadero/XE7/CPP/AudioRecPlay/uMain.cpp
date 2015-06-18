
//---------------------------------------------------------------------------

// This software is Copyright (c) 2014 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
// ---------------------------------------------------------------------------

#include <fmx.h>
#include <System.IOUtils.hpp>
#pragma hdrstop

#include "uMain.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
#pragma resource ("*.LgXhdpiPh.fmx", _PLAT_ANDROID)

TAudioRecPlayForm *AudioRecPlayForm;

String __fastcall GetAudioFileName(const String AFileName)
{
#if defined(__ANDROID__)
	return  IncludeTrailingPathDelimiter(System::Ioutils::TPath::GetTempPath()) +  AFileName;
#else
	#if defined(TARGET_OS_IPHONE) || defined(TARGET_IPHONE_SIMULATOR)
		return IncludeTrailingPathDelimiter(System::Ioutils::TPath::GetDocumentsPath()) +
		AFileName;
	#else
		return  AFileName;
	#endif
#endif
}

// ---------------------------------------------------------------------------
__fastcall TAudioRecPlayForm::TAudioRecPlayForm(TComponent* Owner)
	: TForm(Owner)
{
  FMicrophone = TCaptureDeviceManager::Current->DefaultAudioCaptureDevice;
}
// ---------------------------------------------------------------------------
bool __fastcall TAudioRecPlayForm::HasMicrophone()
{
	return FMicrophone;
}
// ---------------------------------------------------------------------------
bool __fastcall TAudioRecPlayForm::IsMicrophoneRecording()
{
	return HasMicrophone() && (FMicrophone->State == TCaptureDeviceState::Capturing);
}

// ---------------------------------------------------------------------------
void __fastcall TAudioRecPlayForm::ActionListUpdate(TBasicAction *Action,
	bool &Handled)
{
	imgOn->Visible = HasMicrophone() && (FMicrophone->State == TCaptureDeviceState::Capturing);
	actStartRecording->Enabled = !IsMicrophoneRecording() && HasMicrophone();
	actStopRecording->Enabled = IsMicrophoneRecording();
	actStop->Enabled = (MediaPlayer->Media != NULL) && (MediaPlayer->State == TMediaState::Playing);
	actPlay->Enabled = FileExists(GetAudioFileName(AUDIO_FILENAME)) && (MediaPlayer->State != TMediaState::Playing);
}
// ---------------------------------------------------------------------------
void __fastcall TAudioRecPlayForm::actPlayExecute(TObject *Sender)
{
	if (IsMicrophoneRecording()) actStopRecording->Execute();
	MediaPlayer->FileName = GetAudioFileName(AUDIO_FILENAME);
	MediaPlayer->Play();
}
//---------------------------------------------------------------------------

void __fastcall TAudioRecPlayForm::actStopExecute(TObject *Sender)
{
	MediaPlayer->Stop();
}
//---------------------------------------------------------------------------

void __fastcall TAudioRecPlayForm::actStartRecordingExecute(TObject *Sender)
{
	actStop->Execute();
	// get the microphone device
	FMicrophone = TCaptureDeviceManager::Current->DefaultAudioCaptureDevice;
	if (HasMicrophone()){
		// and attempt to record to 'test.caf' file
		FMicrophone->FileName = GetAudioFileName(AUDIO_FILENAME);
	   	try {
			FMicrophone->StartCapture();
		}
		catch(Exception &e) {
			ShowMessage("StartCapture: Operation not supported by this device");
		}
	}
	else
		ShowMessage("No microphone is available.");
}
//---------------------------------------------------------------------------

void __fastcall TAudioRecPlayForm::actStopRecordingExecute(TObject *Sender)
{
	// stop capturing audio from the microphone
	if (IsMicrophoneRecording())
	{
		try {
			FMicrophone->StopCapture();
		} catch (Exception &e) {
			ShowMessage("Get state: Operation not supported by this device");
		}
	}
}
//---------------------------------------------------------------------------


