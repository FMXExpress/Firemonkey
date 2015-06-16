//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "MainUnit.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TForm4 *Form4;
//---------------------------------------------------------------------------
__fastcall TForm4::TForm4(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm4::SelectButtonClick(TObject *Sender)
{
  OpenDialog1->Filter = TMediaCodecManager::GetFilterStringByType(TMediaType::Audio);
  if (OpenDialog1->Execute()) {
	MediaPlayer1->FileName = OpenDialog1->FileName;
	if (MediaPlayer1->Media != NULL)
	{
	  FileNameLabel->Text = ExtractFileName(OpenDialog1->FileName);
	  SelectButton->Enabled = false;
	  PauseButton->Enabled = true;
	  ClearButton->Enabled = true;
	  DurationLabel->Text = IntToStr((__int64) MediaPlayer1->Media->Duration / MediaTimeScale) + " s";
	  DurationTrackBar->Max = MediaPlayer1->Media->Duration;
	  VolumeTrackBar->Value = 1 - MediaPlayer1->Media->Volume;
	  PlayingLabel->Text = "0 s";
	  MediaPlayer1->CurrentTime = 0;  // set to start of song
	  MediaPlayer1->Play();
	  Timer1->Enabled = true;
	}
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm4::Timer1Timer(TObject *Sender)
{
  DurationTrackBar->Tag = 1;
  DurationTrackBar->Value = MediaPlayer1->CurrentTime;
  if (MediaPlayer1->Media != 0) {
	PlayingLabel->Text = IntToStr((__int64) MediaPlayer1->Media->CurrentTime / MediaTimeScale) + " s";
  }
  else
    return;
  DurationTrackBar->Tag = 0;
  // if at the end of the song - then stop
  if (MediaPlayer1->CurrentTime >= MediaPlayer1->Media->Duration) {
	MediaPlayer1->Clear();
	PauseButton->Enabled = false;
	ClearButton->Enabled = false;
	PlayingLabel->Text = "0 s";
	FileNameLabel->Text = "<none>";
	DurationLabel->Text = "0 s";
	DurationTrackBar->Value = 0;
	SelectButton->Enabled = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm4::VolumeTrackBarChange(TObject *Sender)
{
  MediaPlayer1->Volume = 1 - VolumeTrackBar->Value;
}
//---------------------------------------------------------------------------
void __fastcall TForm4::DurationTrackBarChange(TObject *Sender)
{
  if (DurationTrackBar->Tag == 0)
  {
	MediaPlayer1->CurrentTime = (__int64) DurationTrackBar->Value;
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm4::PauseButtonClick(TObject *Sender)
{
  if (MediaPlayer1->State == TMediaState::Playing)
  {
	PauseButton->Text = "Play";
	MediaPlayer1->Stop();
  }
  else
  {
	PauseButton->Text = "Pause";
	MediaPlayer1->Play();
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm4::ClearButtonClick(TObject *Sender)
{
  Timer1->Enabled = false;
  MediaPlayer1->Clear();
  PauseButton->Enabled = false;
  ClearButton->Enabled = false;
  PlayingLabel->Text = "0 s";
  FileNameLabel->Text = "<none>";
  DurationLabel->Text = "0 s";
  DurationTrackBar->Value = 0;
  SelectButton->Enabled = true;
}
//---------------------------------------------------------------------------
