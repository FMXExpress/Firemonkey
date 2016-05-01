//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "LifeUnitFmx.h"
#include <cmath>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TLifeForm *LifeForm;
///---------------------------------------------------------------------------
__fastcall TLifeForm::TLifeForm(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
int __fastcall TLifeForm::MulDiv(int val, int num, int denom)
{
    	long retval = val;
	retval *= num;
	retval /= denom;
	return (int) retval;
}
//---------------------------------------------------------------------------
void __fastcall TLifeForm::Button1Click(TObject *Sender)
{
	if(!FLifeEngine->Running) {
		FLifeEngine->Start();
		Button1->Text = "&Stop";
	}
	else {
		FLifeEngine->Stop();
		Button1->Text = "&Start";
    }
}
//---------------------------------------------------------------------------

void __fastcall TLifeForm::Button2Click(TObject *Sender)
{
	if(!FLifeEngine->Running) {
		FLifeEngine->Clear();
		FLifeBoard = FLifeEngine->LifeBoard;
		FormResize(Sender);
		PaintBox1->InvalidateRect(PaintBox1->BoundsRect);
	}
}
//---------------------------------------------------------------------------

void __fastcall TLifeForm::FormCloseQuery(TObject *Sender, bool &CanClose)
{
	if(FLifeEngine->Running) {
		Button1Click(Sender);
	}
  	CanClose = !FLifeEngine->Running;
}
//---------------------------------------------------------------------------
void __fastcall TLifeForm::LifeEngineUpdate(TObject * Sender)
{
	FLifeBoard = FLifeEngine->LifeBoard;
	FGensPerSecond = FLifeEngine->GensPerSecond;
	FMaxGensPerSecond = FLifeEngine->MaxGensPerSecond;
	Label2->Text = Format("%f Generations Per Second", ARRAYOFCONST((FGensPerSecond)));
	Label3->Text = Format("%f Max Generations Per Second", ARRAYOFCONST((FMaxGensPerSecond)));
	PaintBox1->InvalidateRect(PaintBox1->BoundsRect);
}
//---------------------------------------------------------------------------

void __fastcall TLifeForm::FormCreate(TObject *Sender)
{
	BoardSize = TSize(500,500);
	FLifeEngine = new TLifeEngine(BoardSize);
	FLifeEngine->OnUpdate = LifeEngineUpdate;
	FLifeBoard = FLifeEngine->LifeBoard;
	FLifeEngine->UpdateRate = 30;
}
//---------------------------------------------------------------------------

void __fastcall TLifeForm::FormDestroy(TObject *Sender)
{
	FLifeEngine->Free();
}
//---------------------------------------------------------------------------

void __fastcall TLifeForm::FormResize(TObject *Sender)
{
	FViewSize = System::Classes::Point((PaintBox1->Width - 10) / 10, (PaintBox1->Height - 10) / 10);
	HorzScrollBar->Max = FLifeBoard.Length;
	VertScrollBar->Max = FLifeBoard[0].Length;
}
//---------------------------------------------------------------------------

void __fastcall TLifeForm::Button4Click(TObject *Sender)
{
	HorzScrollBar->Value = (FLifeBoard.Length - FViewSize.X) / 2;
	VertScrollBar->Value = (FLifeBoard[0].Length - FViewSize.Y) / 2;
}
//---------------------------------------------------------------------------

void __fastcall TLifeForm::Button3Click(TObject *Sender)
{
	if((!FLifeEngine->Running) && (OpenDialog1->Execute()))
	{
		FLifeEngine->LoadPattern(OpenDialog1->FileName);
	}
}
//---------------------------------------------------------------------------
void __fastcall TLifeForm::HorzScrollBarChange(TObject *Sender)
{
	FViewOffset.X = HorzScrollBar->Value;
	PaintBox1->InvalidateRect(PaintBox1->BoundsRect);
}
//---------------------------------------------------------------------------
void __fastcall TLifeForm::VertScrollBarChange(TObject *Sender)
{
	FViewOffset.Y = VertScrollBar->Value;
	PaintBox1->InvalidateRect(PaintBox1->BoundsRect);
}
//---------------------------------------------------------------------------
void __fastcall TLifeForm::PaintBox1MouseDown(TObject *Sender, TMouseButton Button,
		  TShiftState Shift, float X, float Y)
{
	if((!FLifeEngine->Running) && (Button == TMouseButton::mbLeft)) {
		int Row = Y / 10;
		int Column = X / 10;
		if((Row >= 0) && (Row <= FViewSize.Y) && (Column >= 0) && (Column <= FViewSize.X)) {
			FLifeBoard[FViewOffset.X + Column][FViewOffset.Y + Row] =
				FLifeBoard[FViewOffset.X + Column][FViewOffset.Y + Row] ^ 1;
			PaintBox1->InvalidateRect(TRectF(Column + 10, Row * 10, Column * 10 + 11, Row * 10 + 11));
		}
		Label1->Text = String().sprintf(L"%d, %d", FViewSize.X + Column, FViewSize.Y + Row);
	}
}
//---------------------------------------------------------------------------
void __fastcall TLifeForm::PaintBox1Paint(TObject *Sender, TCanvas *Canvas)
{
	PaintBox1->Canvas->BeginScene();
    PaintBox1->Canvas->Stroke->Kind = TBrushKind::Solid;
	PaintBox1->Canvas->Stroke->Color = static_cast<TAlphaColor>(TAlphaColorRec::Gray);
	if(FLifeBoard.Length > 0) {
		for(int i = 0; i < FViewSize.X; i++) {
			for(int j = 0; j < FViewSize.Y; j++) {
				if(FLifeBoard[Min(FViewOffset.X + i, FLifeBoard.High)][Min(FViewOffset.Y + j, FLifeBoard[0].High)] != 0)
				{

					PaintBox1->Canvas->Fill->Color = static_cast<TAlphaColor>(TAlphaColorRec::Black);
					PaintBox1->Canvas->FillRect(TRectF(i * 10, j * 10, i * 10 + 11, j * 10 + 11), 0, 0, TCorners(), 100);
				}
				else {
					PaintBox1->Canvas->Fill->Color = this->Canvas->Fill->Color;
					PaintBox1->Canvas->DrawRect(TRectF(i * 10, j * 10, i * 10 + 11, j * 10 + 11), 0, 0, TCorners(), 100);
				}
			}
		}
	}
	int Scale = 1000000;
	while(Scale > 10) {
		if((FMaxGensPerSecond * 10) < Scale) {
			Scale = Scale / 10;
		}
		else  {
			break;
		}
		int Gens = this->MulDiv(FGensPerSecond, PaintBox1->Height, Scale);
		int Max =  this->MulDiv(FMaxGensPerSecond, PaintBox1->Height, Scale);
		PaintBox1->Canvas->Fill->Color = static_cast<TAlphaColor>(TAlphaColorRec::Green);
		PaintBox1->Canvas->FillRect(TRectF(PaintBox1->Width - 4, PaintBox1->Height - Gens, PaintBox1->Width, PaintBox1->Height),
			0,0, TCorners(), 100);
		PaintBox1->Canvas->Stroke->Color = static_cast<TAlphaColor>(TAlphaColorRec::Red);
		PaintBox1->Canvas->DrawLine(TPointF(PaintBox1->Width - 4, PaintBox1->Height - Max),
			TPointF(PaintBox1->Width, PaintBox1->Height - Max), 100);
	}
	PaintBox1->Canvas->EndScene();
}
//---------------------------------------------------------------------------

void __fastcall TLifeForm::PaintBox1MouseMove(TObject *Sender, TShiftState Shift,
          float X, float Y)
{
	Label1->Text = String().sprintf(L"%d, %d", static_cast<int>(FViewOffset.X + X / 10),
		static_cast<int>(FViewOffset.Y + Y / 10));
}
//---------------------------------------------------------------------------

void __fastcall TLifeForm::CheckBox1Change(TObject *Sender)
{
	if(FLifeEngine != NULL) {
		FLifeEngine->Parallel = CheckBox1->IsChecked;
	}
}
//---------------------------------------------------------------------------

