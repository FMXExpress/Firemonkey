// ---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

// ---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "LifeUnitFmx.h"
#include <cmath>
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TLifeForm *LifeForm;

///---------------------------------------------------------------------------
__fastcall TLifeForm::TLifeForm(TComponent* Owner) : TForm(Owner) {
}

// ---------------------------------------------------------------------------
int __fastcall TLifeForm::MulDiv(int val, int num, int denom) {
	long retval = val;
	retval *= num;
	retval /= denom;
	return (int) retval;
}

// ---------------------------------------------------------------------------
void __fastcall TLifeForm::Button1Click(TObject *Sender) {
	if (!FLifeEngine->Running) {
		FLifeEngine->Start();
		Button1->Text = "&Stop";
	}
	else {
		FLifeEngine->Stop();
		Button1->Text = "&Start";
	}
}
// ---------------------------------------------------------------------------

void __fastcall TLifeForm::Button2Click(TObject *Sender) {
	if (!FLifeEngine->Running) {
		FLifeEngine->Clear();
		FLifeBoard = FLifeEngine->LifeBoard;
		FormResize(Sender);
		PaintBox1->Repaint();
	}
}
// ---------------------------------------------------------------------------

void __fastcall TLifeForm::FormCloseQuery(TObject *Sender, bool &CanClose) {
	if (FLifeEngine->Running) {
		Button1Click(Sender);
	}
	CanClose = !FLifeEngine->Running;
}

// ---------------------------------------------------------------------------
void __fastcall TLifeForm::LifeEngineUpdate(TObject * Sender) {
	FLifeBoard = FLifeEngine->LifeBoard;
	FGensPerSecond = FLifeEngine->GensPerSecond;
	FMaxGensPerSecond = FLifeEngine->MaxGensPerSecond;
	Label2->Text = Format("%f Generations Per Second",
		ARRAYOFCONST((FGensPerSecond)));
	Label3->Text = Format("%f Max Generations Per Second",
		ARRAYOFCONST((FMaxGensPerSecond)));
	PaintBox1->InvalidateRect(PaintBox1->BoundsRect);
}
// ---------------------------------------------------------------------------

void __fastcall TLifeForm::FormCreate(TObject *Sender) {
	BoardSize = TSize(500, 500);
	FLifeEngine = new TLifeEngine(BoardSize);
	FLifeEngine->OnUpdate = LifeEngineUpdate;
	FLifeBoard = FLifeEngine->LifeBoard;
	FLifeEngine->UpdateRate = 30;
}
// ---------------------------------------------------------------------------

void __fastcall TLifeForm::FormDestroy(TObject *Sender) {
	FLifeEngine->Free();
}
// ---------------------------------------------------------------------------

void __fastcall TLifeForm::FormResize(TObject *Sender) {
	FViewSize = System::Classes::Point((PaintBox1->Width - 10) / 10,
		(PaintBox1->Height - 10) / 10);
	HorzScrollBar->Max = FLifeBoard.Length - FViewSize.X;
	VertScrollBar->Max = FLifeBoard[0].Length - FViewSize.Y;
}
// ---------------------------------------------------------------------------

void __fastcall TLifeForm::Button4Click(TObject *Sender) {
	HorzScrollBar->Value = (FLifeBoard.Length - FViewSize.X) / 2;
	VertScrollBar->Value = (FLifeBoard[0].Length - FViewSize.Y) / 2;
}
// ---------------------------------------------------------------------------

void __fastcall TLifeForm::Button3Click(TObject *Sender) {
	if ((!FLifeEngine->Running) && (OpenDialog1->Execute())) {
		FLifeEngine->LoadPattern(OpenDialog1->FileName);
	}
}

// ---------------------------------------------------------------------------
void __fastcall TLifeForm::HorzScrollBarChange(TObject *Sender) {
	FViewOffset.X = HorzScrollBar->Value;
	PaintBox1->Repaint();
}

// ---------------------------------------------------------------------------
void __fastcall TLifeForm::VertScrollBarChange(TObject *Sender) {
	FViewOffset.Y = VertScrollBar->Value;
	PaintBox1->Repaint();
}

// ---------------------------------------------------------------------------
void __fastcall TLifeForm::PaintBox1MouseDown(TObject *Sender,
	TMouseButton Button, TShiftState Shift, float X, float Y) {
	if ((!FLifeEngine->Running) && (Button == TMouseButton::mbLeft)) {
		int Row = Y / 10;
		int Column = X / 10;
		if (InRange(Row, 0, FViewSize.Y - 1) && InRange(Column, 0, FViewSize.X - 1)) {
			FLifeBoard[FViewOffset.X + Column][FViewOffset.Y + Row] =
				FLifeBoard[FViewOffset.X + Column][FViewOffset.Y + Row] ^ 1;
			PaintBox1->InvalidateRect(TRectF(Column * 10, Row * 10,
				Column * 10 + 11, Row * 10 + 11));
		}
		Label1->Text = String().sprintf(L"%d, %d", FViewSize.X + Column,
			FViewSize.Y + Row);
	}
}

// ---------------------------------------------------------------------------
TRectF TLifeForm::GetCellRect(int row, int col) {
    return TRectF(row * 10, col * 10, row * 10 + 11, col * 10 + 11);
}

// ---------------------------------------------------------------------------
void TLifeForm::DrawLifeCells(TCanvas* aCanvas) {
	if (FLifeBoard.Length > 0) {
		aCanvas->Fill->Color = TAlphaColorRec::Black;
		for (int row = 0; row < FViewSize.X; row++)
			for (int col = 0; col < FViewSize.Y; col++) {
				TRectF cellRect = GetCellRect(row, col);
				cellRect.Offset(-aCanvas->StrokeThickness / 2, -aCanvas->StrokeThickness / 2);
				if (FLifeBoard[Min(FViewOffset.X + row, FLifeBoard.High)][Min(FViewOffset.Y + col, FLifeBoard[0].High)] != 0) {
					aCanvas->FillRect(cellRect, 0, 0, AllCorners, 1);
				}
			}
	}
}

// ---------------------------------------------------------------------------
void TLifeForm::DrawGrid(TCanvas* aCanvas) {
	aCanvas->Stroke->Kind = TBrushKind::Solid;
	aCanvas->Stroke->Color = static_cast<TAlphaColor>(TAlphaColorRec::Gray);
	for (int row = 0; row < FViewSize.X; row++)
		for (int col = 0; col < FViewSize.Y; col++) {
			TRectF cellRect = GetCellRect(row, col);
			cellRect.Offset(-aCanvas->StrokeThickness / 2, -aCanvas->StrokeThickness / 2);
			aCanvas->DrawRect(cellRect, 0, 0, AllCorners, 1);
		}
}

// ---------------------------------------------------------------------------
void __fastcall TLifeForm::PaintBox1Paint(TObject *Sender, TCanvas *Canvas) {
	DrawLifeCells(Canvas);
	DrawGrid(Canvas);
	int Scale = 1000000;
	while (Scale > 10) {
		if ((FMaxGensPerSecond * 10) < Scale) {
			Scale = Scale / 10;
		}
		else {
			break;
		}
		int Gens = this->MulDiv(FGensPerSecond, PaintBox1->Height, Scale);
		int Max = this->MulDiv(FMaxGensPerSecond, PaintBox1->Height, Scale);
		Canvas->Fill->Color =
			static_cast<TAlphaColor>(TAlphaColorRec::Green);
		Canvas->FillRect(TRectF(PaintBox1->Width - 4,
			PaintBox1->Height - Gens, PaintBox1->Width, PaintBox1->Height), 0,
			0, TCorners(), 100);
		Canvas->Stroke->Color =
			static_cast<TAlphaColor>(TAlphaColorRec::Red);
		Canvas->DrawLine(TPointF(PaintBox1->Width - 4,
			PaintBox1->Height - Max), TPointF(PaintBox1->Width,
			PaintBox1->Height - Max), 100);
	}
}
// ---------------------------------------------------------------------------

void __fastcall TLifeForm::PaintBox1MouseMove(TObject *Sender,
	TShiftState Shift, float X, float Y) {
	Label1->Text = String().sprintf(L"%d, %d",
		static_cast<int>(FViewOffset.X + X / 10),
		static_cast<int>(FViewOffset.Y + Y / 10));
}
// ---------------------------------------------------------------------------

void __fastcall TLifeForm::CheckBox1Change(TObject *Sender) {
	if (FLifeEngine != NULL) {
		FLifeEngine->Parallel = CheckBox1->IsChecked;
	}
}
// ---------------------------------------------------------------------------
