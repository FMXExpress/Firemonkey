//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef PlayAudioFileH
#define PlayAudioFileH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Media.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <FMX.MobilePreview.hpp>
//---------------------------------------------------------------------------
class TAudioPlayBackForm : public TForm
{
__published:	// IDE-managed Components
	TToolBar *ToolBar1;
	TLabel *Title;
	TSpeedButton *btnStop;
	TSpeedButton *btnPlay;
	TMediaPlayer *MediaPlayer1;
	void __fastcall btnPlayClick(TObject *Sender);
	void __fastcall btnStopClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TAudioPlayBackForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TAudioPlayBackForm *AudioPlayBackForm;
//---------------------------------------------------------------------------
#endif
