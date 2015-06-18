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
private:	// User declarations
	TAudioCaptureDevice *FMicrophone;
	const String AUDIO_FILENAME = "test.caf";
	bool __fastcall HasMicrophone();
	bool __fastcall IsMicrophoneRecording();
public:		// User declarations
	__fastcall TAudioRecPlayForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TAudioRecPlayForm *AudioRecPlayForm;
//---------------------------------------------------------------------------
#endif
