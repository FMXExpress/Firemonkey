
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "ctrlsdemofrm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"

long rndm(long max)
{
#ifdef __APPLE__
	return (random() % ((max + 1) + max));
#else
	return random(max);
#endif
}

TfrmCtrlsDemo *frmCtrlsDemo;
//---------------------------------------------------------------------------
__fastcall TfrmCtrlsDemo::TfrmCtrlsDemo(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmCtrlsDemo::MenuItem6Click(TObject *Sender)
{
  TfrmAbout *frmAbout =  new TfrmAbout(Application);
  frmAbout->ShowModal();
  frmAbout->Free();
}
//---------------------------------------------------------------------------
void __fastcall TfrmCtrlsDemo::MenuItem4Click(TObject *Sender)
{
//
}
//---------------------------------------------------------------------------
void __fastcall TfrmCtrlsDemo::MenuItem2Click(TObject *Sender)
{
//
}
//---------------------------------------------------------------------------
void __fastcall TfrmCtrlsDemo::MenuItem1Click(TObject *Sender)
{
//
}
//---------------------------------------------------------------------------
void __fastcall TfrmCtrlsDemo::CheckBox2Change(TObject *Sender)
{
  StringListBox1->MultiSelect = CheckBox2->IsChecked;
}
//---------------------------------------------------------------------------

void __fastcall TfrmCtrlsDemo::calloutBottomChange(TObject *Sender)
{
  if (calloutLeft->IsChecked)
	CalloutPanel1->CalloutPosition = TCalloutPosition::cpLeft;
  if (calloutRight->IsChecked)
	CalloutPanel1->CalloutPosition = TCalloutPosition::cpRight;
  if (calloutTop->IsChecked)
	CalloutPanel1->CalloutPosition = TCalloutPosition::cpTop;
  if (calloutBottom->IsChecked)
	CalloutPanel1->CalloutPosition = TCalloutPosition::cpBottom;
}
//---------------------------------------------------------------------------


void __fastcall TfrmCtrlsDemo::MenuItem3Click(TObject *Sender)
{
  Application->Terminate();
}
//---------------------------------------------------------------------------

void __fastcall TfrmCtrlsDemo::AngleButton1Change(TObject *Sender)
{
  Label17->Text = IntToStr((int)ceil(AngleButton1->Value));
}

void __fastcall TfrmCtrlsDemo::ScaleTrackChange(TObject *Sender)
{
  // change scale
  ControlRoot->Scale->X = ScaleTrack->Value;
  ControlRoot->Scale->Y = ScaleTrack->Value;
  TextScale->Text = IntToStr((int)(ScaleTrack->Value * 100)) + "%";
}
//---------------------------------------------------------------------------

void __fastcall TfrmCtrlsDemo::AngleButtonl1Change(TObject *Sender)
{
  Label17->Text = IntToStr((int)(AngleButton1->Value));
}
//---------------------------------------------------------------------------

void __fastcall TfrmCtrlsDemo::AngleButton3Change(TObject *Sender)
{
  Label17->Text = IntToStr((int)(AngleButton3->Value));
}
//---------------------------------------------------------------------------
// todo: reimplement 3d switch
void __fastcall TfrmCtrlsDemo::Button1Click(TObject *Sender)
{
//  TViewport3D *V = new TViewport3D(this);
//  TLayer3D *L = new TLayer3D(this);
//
//  // Create 3D viewport and layer
//  V->Parent = this;
//  V->Align = TAlignLayout::alClient;
//  V->Color = claNull;
//  L->Parent = V;
//  L->Projection = TProjection::pjScreen;
//  L->Align = TAlignLayout::alClient;

  // Move object to 3D scene

//  ScaleRoot->Parent = L;

  // Animate

//  L->AnimateFloat("Position.Z", 500, 1);
//  L->AnimateFloatDelay("Position.Z", 0, 1, 1);
//  L->AnimateFloatWait("RotationAngle.X", 360, 2, TAnimationType::atInOut, TInterpolationType::itBack);

  // Back to 2D

//  ScaleRoot->Parent = this;

  // Free 3D

//  L->Free();
//  V->Free();
}
//---------------------------------------------------------------------------

void __fastcall TfrmCtrlsDemo::Button4Click(TObject *Sender)
{
  MenuItem6Click(this);
}
//---------------------------------------------------------------------------

void __fastcall TfrmCtrlsDemo::AngleButton2Change(TObject *Sender)
{
  Label17->Text = IntToStr((int)(AngleButton2->Value));
}
//---------------------------------------------------------------------------

void __fastcall TfrmCtrlsDemo::TrackBar2Change(TObject *Sender)
{
  ListTransform->RotationAngle = TrackBar2->Value;
  TextBox4->RotationAngle = TrackBar2->Value;
}
//---------------------------------------------------------------------------

void __fastcall TfrmCtrlsDemo::TrackBar3Change(TObject *Sender)
{
  ListTransform->Opacity = TrackBar3->Value;
  TextBox4->Opacity = TrackBar3->Value;
}
//---------------------------------------------------------------------------

void __fastcall TfrmCtrlsDemo::TrackBar4Change(TObject *Sender)
{
  CornerButton1->XRadius = TrackBar4->Value;
  CornerButton1->YRadius = TrackBar4->Value;

  CornerButton2->XRadius = TrackBar4->Value;
  CornerButton2->YRadius = TrackBar4->Value;
  CornerButton3->XRadius = TrackBar4->Value;
  CornerButton3->YRadius = TrackBar4->Value;
  CornerButton4->XRadius = TrackBar4->Value;
  CornerButton4->YRadius = TrackBar4->Value;
  CornerButton5->XRadius = TrackBar4->Value;
  CornerButton5->YRadius = TrackBar4->Value;
  CornerButton6->XRadius = TrackBar4->Value;
  CornerButton6->YRadius = TrackBar4->Value;

}
//---------------------------------------------------------------------------

void __fastcall TfrmCtrlsDemo::CheckBox6Change(TObject *Sender)
{
  if (CheckBox6->IsChecked)
	CornerButton1->Corners = CornerButton1->Corners  + TCorners() << TCorner::crTopLeft;
  else
	CornerButton1->Corners = CornerButton1->Corners  + TCorners() >> TCorner::crTopLeft;
}
//---------------------------------------------------------------------------

void __fastcall TfrmCtrlsDemo::CheckBox5Change(TObject *Sender)
{
  if (CheckBox5->IsChecked)
	CornerButton1->Corners = CornerButton1->Corners + TCorners() << TCorner::crBottomLeft;
  else
	CornerButton1->Corners = CornerButton1->Corners + TCorners() >> TCorner::crBottomLeft;

}
//---------------------------------------------------------------------------

void __fastcall TfrmCtrlsDemo::CheckBox4Change(TObject *Sender)
{
  if (CheckBox4->IsChecked)
	CornerButton1->Corners = CornerButton1->Corners  + TCorners() << TCorner::crTopRight;
  else
	CornerButton1->Corners = CornerButton1->Corners  + TCorners() >> TCorner::crTopRight;
}
//---------------------------------------------------------------------------

void __fastcall TfrmCtrlsDemo::CheckBox3Change(TObject *Sender)
{
  if (CheckBox3->IsChecked)
	CornerButton1->Corners = CornerButton1->Corners  + TCorners() << TCorner::crBottomRight;
  else
	CornerButton1->Corners = CornerButton1->Corners  + TCorners() >> TCorner::crBottomRight;
}

void __fastcall TfrmCtrlsDemo::CheckBox8Change(TObject *Sender)
{
  ListBox1->ShowCheckboxes = CheckBox8->IsChecked;
  TreeView1->ShowCheckboxes = CheckBox8->IsChecked;
}
//---------------------------------------------------------------------------

void __fastcall TfrmCtrlsDemo::DropTarget1DragOver(TObject *Sender, const TDragObject &Data,
		  const TPointF &Point, bool &Accept)
{
  Accept = true;
}
//---------------------------------------------------------------------------


void __fastcall TfrmCtrlsDemo::FormCreate(TObject *Sender)
{
  int i;
  TRectangle *ARect = new TRectangle(frmCtrlsDemo);
  for (i=0; i < 50; i++) {



   ARect->Parent= ScrollBox1;
	  ARect->Width = (30 + rndm(150));
	  ARect->Height = (30 + rndm(150));
	  ARect->HitTest = false;
	  ARect->Position->X = rndm(1600);
	  ARect->Position->Y = rndm(1600);
	  ARect->XRadius = rndm(20);
	  ARect->YRadius = ARect->XRadius;
	  ARect->Fill->Color = ((50 + rndm(205)) << 24) | rndm(0xFFFFFF);
  }

}
//---------------------------------------------------------------------------

void __fastcall TfrmCtrlsDemo::MenuItem7Click(TObject *Sender)
{
  if (OpenDialog1->Execute())
    TStyleManager::SetStyle(TStyleManager::LoadFromFile(OpenDialog1->FileName));
}
//---------------------------------------------------------------------------





