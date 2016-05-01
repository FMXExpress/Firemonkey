//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef uMainH
#define uMainH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.ActnList.hpp>
#include <FMX.Media.hpp>
#include <FMX.Objects.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <System.Actions.hpp>
#include <FMX.MobilePreview.hpp>
#include <FMX.Controls.Presentation.hpp>
//---------------------------------------------------------------------------
class TAudioRecPlayForm : public TForm
{
__published:	// IDE-managed Components
	TActionList *ActionList;
	TAction *actStartRecording;
	TAction *actStopRecording;
	TAction *actPlay;
	TAction *actStop;
	TMediaPlayer *MediaPlayer;
	TToolBar *ToolBar1;
	TLabel *Label1;
	TButton *btnStartPlay;
	TButton *btnStopPlay;
	TToolBar *ToolBar2;
	TButton *btnStopRec;
	TButton *btnStartRec;
	TImage *imgOff;
	TImage *imgOn;
	void __fastcall ActionListUpdate(TBasicAction *Action, bool &Handled);
	void __fastcall actPlayExecute(TObject *Sender);
	void __fastcall actStopExecute(TObject *Sender);
	void __fastcall actStartRecordingExecute(TObject *Sender);
	void __fastcall actStopRecordingExecute(TObject *Sender);
	void __fastcall imgOffClick(TObject *Sender);
	void __fastcall imgOnClick(TObject *Sender);
private:	// User declarations
	TAudioCaptureDevice *FMicrophone;
	const String AUDIO_FILENAME;
	bool __fastcall HasMicrophone();
	bool __fastcall IsMicrophoneRecording();
public:		// User declarations
	__fastcall TAudioRecPlayForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TAudioRecPlayForm *AudioRecPlayForm;
//---------------------------------------------------------------------------
#endif
