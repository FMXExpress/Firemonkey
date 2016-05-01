//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef LifeUnitFmxH
#define LifeUnitFmxH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Dialogs.hpp>
#include <FMX.Objects.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include "LifeEngine.h"
#include <FMX.Controls.Presentation.hpp>
//---------------------------------------------------------------------------
class TLifeForm : public TForm
{
__published:	// IDE-managed Components
	TPaintBox *PaintBox1;
	TScrollBar *HorzScrollBar;
	TScrollBar *VertScrollBar;
	TOpenDialog *OpenDialog1;
	TButton *Button1;
	TButton *Button2;
	TButton *Button3;
	TButton *Button4;
	TCheckBox *CheckBox1;
	TLabel *Label1;
	TLabel *Label2;
	TLabel *Label3;
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall Button2Click(TObject *Sender);
	void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormDestroy(TObject *Sender);
	void __fastcall FormResize(TObject *Sender);
	void __fastcall Button4Click(TObject *Sender);
	void __fastcall Button3Click(TObject *Sender);
	void __fastcall HorzScrollBarChange(TObject *Sender);
	void __fastcall VertScrollBarChange(TObject *Sender);
	void __fastcall PaintBox1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
		  float X, float Y);
	void __fastcall PaintBox1Paint(TObject *Sender, TCanvas *Canvas);
	void __fastcall PaintBox1MouseMove(TObject *Sender, TShiftState Shift, float X,
          float Y);
	void __fastcall CheckBox1Change(TObject *Sender);
private:	// User declarations
	TSize BoardSize;
	TLifeEngine * FLifeEngine;
	TLifeBoard FLifeBoard;
	double FGensPerSecond;
	double FMaxGensPerSecond;
	TPoint FViewOffset, FViewSize;
	void __fastcall LifeEngineUpdate(TObject * Sender);
	int __fastcall MulDiv(int val, int num, int denom);
	void DrawLifeCells(TCanvas* aCanvas);
	void DrawGrid(TCanvas* aCanvas);
	TRectF GetCellRect(int row, int col);
public:		// User declarations
	__fastcall TLifeForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TLifeForm *LifeForm;
//---------------------------------------------------------------------------
#endif
