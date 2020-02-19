//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#include <fmx.h>
#include <FMX.Utils.hpp>

#pragma hdrstop

#include "MouseOverHintsForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TMouseoverHintForm *MouseoverHintForm;
//---------------------------------------------------------------------------
__fastcall TMouseoverHintForm::TMouseoverHintForm(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TMouseoverHintForm::Action1Execute(TObject *Sender)
{
//
}
//---------------------------------------------------------------------------
void __fastcall TMouseoverHintForm::Action2Execute(TObject *Sender)
{
//
}
//---------------------------------------------------------------------------
void __fastcall TMouseoverHintForm::Action3Execute(TObject *Sender)
{
//
}
//---------------------------------------------------------------------------
void __fastcall TMouseoverHintForm::Action2Hint(UnicodeString &HintStr, bool &CanShow)
{
  HintStr = "Hint by Code";
  CanShow = EnableActionOnHintCheckBox->IsChecked;
}
//---------------------------------------------------------------------------
void __fastcall TMouseoverHintForm::FormCreate(TObject *Sender)
{
  HintEdit->Text = Button1->Hint;
  ApplicationShowHint->IsChecked = Application->ShowHint;
  FormShowHint->IsChecked = ShowHint;
  StatusBarAutohintCheckBox->IsChecked = StatusBar1->AutoHint;
  ApplicationShortCutsInHintsCheckBox->IsChecked = Application->HintShortCuts;
}
//---------------------------------------------------------------------------
void __fastcall TMouseoverHintForm::ApplicationShowHintChange(TObject *Sender)
{
  Application->ShowHint = ApplicationShowHint->IsChecked;
}
//---------------------------------------------------------------------------
void __fastcall TMouseoverHintForm::FormShowHintChange(TObject *Sender)
{
  ShowHint = FormShowHint->IsChecked;
}
//---------------------------------------------------------------------------
void __fastcall TMouseoverHintForm::ApplicationShortCutsInHintsCheckBoxChange(TObject *Sender)
{
  Application->HintShortCuts = ApplicationShortCutsInHintsCheckBox->IsChecked;
}
//---------------------------------------------------------------------------
void __fastcall TMouseoverHintForm::StatusBarAutohintCheckBoxChange(TObject *Sender)
{
  StatusBar1->AutoHint = StatusBarAutohintCheckBox->IsChecked;
}
//---------------------------------------------------------------------------
void __fastcall TMouseoverHintForm::HintEditChange(TObject *Sender)
{
  Button1->Hint = HintEdit->Text;
}
//---------------------------------------------------------------------------
void __fastcall TMouseoverHintForm::Action3Hint(UnicodeString &HintStr, bool &CanShow)

{
  HintStr = HintStr + " a tail";
  CanShow = EnableActionOnHintCheckBox->IsChecked;
}
//---------------------------------------------------------------------------
void __fastcall TMouseoverHintForm::StatusBar1Hint(TObject *Sender)
{
  Label1->Text = GetLongHint(Application->Hint);
}
//---------------------------------------------------------------------------

