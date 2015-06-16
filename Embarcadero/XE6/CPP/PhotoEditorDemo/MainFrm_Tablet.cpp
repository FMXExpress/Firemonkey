//---------------------------------------------------------------------------

// This software is Copyright (c) 2013 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "MainFrm_Tablet.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "MainFrm"
#pragma resource "*.fmx"
TTabletMainForm *TabletMainForm;
//---------------------------------------------------------------------------
__fastcall TTabletMainForm::TTabletMainForm(TComponent* Owner)
	: TBaseMainForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TTabletMainForm::ActionWaveEffectExecute(TObject *Sender)
{
	SetEffect("Wave");
}
//---------------------------------------------------------------------------
void __fastcall TTabletMainForm::ActionContrastEffectExecute(TObject *Sender)
{
	SetEffect("Contrast");
}
//---------------------------------------------------------------------------
void __fastcall TTabletMainForm::ActionPaperSketchEffectExecute(TObject *Sender)
{
	SetEffect("PaperSketch");
}
//---------------------------------------------------------------------------
void __fastcall TTabletMainForm::ActionListUpdate(TBasicAction *Action, bool &Handled)
{
	TBaseMainForm::ActionListUpdate(Action, Handled);
	TopHelp->Visible = FRawBitmap->IsEmpty();
	ActionWaveEffect->Enabled = !FRawBitmap->IsEmpty();
	ActionContrastEffect->Enabled = !FRawBitmap->IsEmpty();
	ActionPaperSketchEffect->Enabled = !FRawBitmap->IsEmpty();

}
//---------------------------------------------------------------------------
