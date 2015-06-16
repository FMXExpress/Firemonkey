//---------------------------------------------------------------------------

#ifndef Unit2H
#define Unit2H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Dialogs.hpp>
#include <FMX.Media.hpp>
#include <FMX.Objects.hpp>
#include <FMX.Types.hpp>
#include <FMX.StdCtrls.hpp>
//---------------------------------------------------------------------------
class TForm2 : public TForm
{
__published:	// IDE-managed Components
	TRectangle *Rectangle1;
	TSpeedButton *PauseButton;
	TSpeedButton *OpenButton;
	TLabel *Label1;
	TCheckBox *CheckBox1;
	TTrackBar *VolumeTrack;
	TSpeedButton *ClearButton;
	TOpenDialog *OpenDialog1;
	TTrackBar *TrackBar1;
	TTimer *Timer1;
	TMediaPlayerControl *MediaPlayerControl1;
	TMediaPlayer *MediaPlayer1;
	void __fastcall OpenButtonClick(TObject *Sender);
	void __fastcall PauseButtonClick(TObject *Sender);
	void __fastcall TrackBar1Change(TObject *Sender);
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall CheckBox1Change(TObject *Sender);
	void __fastcall VolumeTrackChange(TObject *Sender);
	void __fastcall ClearButtonClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TForm2(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm2 *Form2;
//---------------------------------------------------------------------------
#endif
