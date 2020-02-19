//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef VideoFormH
#define VideoFormH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Media.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <FMX.MobilePreview.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.Objects.hpp>
//---------------------------------------------------------------------------
class TVideoPlayBackForm : public TForm
{
__published:	// IDE-managed Components
	TButton *videoBtn;
	TToolBar *ToolBar1;
	TLabel *Label1;
	TMediaPlayer *MediaPlayer1;
	TRectangle *Rectangle1;
	TMediaPlayerControl *MediaPlayerControl1;
	void __fastcall videoBtnClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TVideoPlayBackForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TVideoPlayBackForm *VideoPlayBackForm;
//---------------------------------------------------------------------------
#endif
