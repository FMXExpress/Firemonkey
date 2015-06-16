//---------------------------------------------------------------------------

#ifndef MainUnitH
#define MainUnitH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Dialogs.hpp>
#include <FMX.Media.hpp>
#include <FMX.Types.hpp>
#include <FMX.StdCtrls.hpp>
//---------------------------------------------------------------------------
class TForm4 : public TForm
{
__published:	// IDE-managed Components
	TMediaPlayer *MediaPlayer1;
	TOpenDialog *OpenDialog1;
	TButton *SelectButton;
	TLabel *Label1;
	TLabel *DurationLabel;
	TTrackBar *VolumeTrackBar;
	TTrackBar *DurationTrackBar;
	TLabel *Label3;
	TLabel *Label4;
	TTimer *Timer1;
	TButton *PauseButton;
	TButton *ClearButton;
	TLabel *PlayingLabel;
	TLabel *Label2;
	TLabel *FileNameLabel;
	void __fastcall SelectButtonClick(TObject *Sender);
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall VolumeTrackBarChange(TObject *Sender);
	void __fastcall DurationTrackBarChange(TObject *Sender);
	void __fastcall PauseButtonClick(TObject *Sender);
	void __fastcall ClearButtonClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TForm4(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm4 *Form4;
//---------------------------------------------------------------------------
#endif
