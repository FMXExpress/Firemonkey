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
//---------------------------------------------------------------------------
class TVideoPlayBackForm : public TForm
{
__published:	// IDE-managed Components
	TButton *videoBtn;
	TToolBar *ToolBar1;
	TLabel *Label1;
	TMediaPlayer *MediaPlayer1;
	void __fastcall videoBtnClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TVideoPlayBackForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TVideoPlayBackForm *VideoPlayBackForm;
//---------------------------------------------------------------------------
#endif
