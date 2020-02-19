//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.
//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "MainFrm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TFormMain *FormMain;
//---------------------------------------------------------------------------
__fastcall TFormMain::TFormMain(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormMain::FormShow(TObject *Sender)
{
	// Uncomment, if you would like to look at the performance of scrolling
	//  FillGridControls();
	FillDemoControls();
	spContentWidth->Value = PresentedScrollBox1->ContentSize->Width;
	spContentHeight->Value = PresentedScrollBox1->ContentSize->Height;
	swAutoSize->IsChecked = PresentedScrollBox1->AutoCalculateContentSize;
	swShowScrollBars->IsChecked = PresentedScrollBox1->ShowScrollBars;
	swShowSizeGrip->IsChecked = PresentedScrollBox1->ShowSizeGrip;
	cbScrollDirection->ItemIndex = static_cast<int>(PresentedScrollBox1->ScrollDirections);
	cbTouchTracking->ItemIndex = static_cast<int>(PresentedScrollBox1->TouchTracking);
	cbScrollAnimation->ItemIndex = static_cast<int>(PresentedScrollBox1->ScrollAnimation);
	cbBounces->ItemIndex = static_cast<int>(PresentedScrollBox1->Bounces);
	cbAutoHide->ItemIndex = static_cast<int>(PresentedScrollBox1->AutoHide);
	swDisableMouseWheel->IsChecked = !PresentedScrollBox1->DisableMouseWheel;
	swScrollEnabled->IsChecked = PresentedScrollBox1->EnabledScroll;
}
//---------------------------------------------------------------------------
void __fastcall TFormMain::FillDemoControls()
{
	Randomize();
	TSizeF CellSize;
	CellSize.Width = PresentedScrollBox1->ContentSize->Width / 20;
	CellSize.Height = PresentedScrollBox1->ContentSize->Height / 20;
	PresentedScrollBox1->BeginUpdate();
	try {
		for(int col = 1; col <= 20; col++) {
			for(int row = 1; row <= 20; row++) {
				TMetaClass *metaClass = DemoControlsClasses[Random(DemoControlsCount)];
				TControl * cell;
				// Using Delphi code!!..
				Application->CreateForm(metaClass,&cell);
				if(cell) {
					TSizeF ControlSize = TSizeF(Random(CellSize.Width-50) + 50, Random(CellSize.Height-50) +50);
					cell->Size->Size = ControlSize;
					cell->HitTest= false;
					cell->Position->Point = TPointF(CellSize.Width * (col - 1), CellSize.Height * (row - 1));
					cell->Parent = PresentedScrollBox1;
				}
            }
        }
	}
	__finally {
        PresentedScrollBox1->EndUpdate();
	}
}

void __fastcall TFormMain::SetChildrenControlType(TFmxObject * const ARoot, TControlType const AControlType)
{
	TPresentedControl *ctrl;
	for(int i =0; i < ARoot->ChildrenCount; i++) {
		TPresentedControl *ctrl = dynamic_cast<TPresentedControl*>(ARoot->Children->Items[i]);
		if(ctrl)
			ctrl->ControlType = AControlType;
		else
			SetChildrenControlType(ARoot->Children->Items[i], AControlType);
	}
}

void __fastcall TFormMain::swControlTypeSwitch(TObject *Sender)
{
	if (swControlType->IsChecked)
		PresentedScrollBox1->ControlType = TControlType::Platform;
	else
		PresentedScrollBox1->ControlType = TControlType::Styled;

	SetChildrenControlType(PresentedScrollBox1, PresentedScrollBox1->ControlType);
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::swScrollEnabledClick(TObject *Sender)
{
	PresentedScrollBox1->EnabledScroll = swScrollEnabled->IsChecked;
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::swDisableMouseWheelClick(TObject *Sender)
{
	PresentedScrollBox1->DisableMouseWheel = !swDisableMouseWheel->IsChecked;
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::cbScrollAnimationChange(TObject *Sender)
{
  if(cbScrollAnimation->ItemIndex != -1)
	PresentedScrollBox1->ScrollAnimation = TBehaviorBoolean(cbScrollAnimation->ItemIndex);
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::cbBouncesChange(TObject *Sender)
{
	if (cbBounces->ItemIndex != -1)
		PresentedScrollBox1->Bounces = TBehaviorBoolean(cbBounces->ItemIndex);
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::cbScrollDirectionChange(TObject *Sender)
{
	if (cbScrollDirection->ItemIndex != -1)
		PresentedScrollBox1->ScrollDirections = TScrollDirections(cbScrollDirection->ItemIndex);
	UpdateViewportSize();
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::cbTouchTrackingChange(TObject *Sender)
{
	if (cbTouchTracking->ItemIndex != -1)
	    PresentedScrollBox1->TouchTracking = TBehaviorBoolean(cbTouchTracking->ItemIndex);
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::cbAutoHideChange(TObject *Sender)
{
	if (cbAutoHide->ItemIndex != -1)
		PresentedScrollBox1->AutoHide = TBehaviorBoolean(cbAutoHide->ItemIndex);
	UpdateViewportSize();
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::swShowScrollBarsSwitch(TObject *Sender)
{
	PresentedScrollBox1->ShowScrollBars = swShowScrollBars->IsChecked;
	UpdateViewportSize();
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::swShowSizeGripSwitch(TObject *Sender)
{
	PresentedScrollBox1->ShowSizeGrip = swShowSizeGrip->IsChecked;
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::swAutoSizeSwitch(TObject *Sender)
{
	PresentedScrollBox1->AutoCalculateContentSize = swAutoSize->IsChecked;
	if (!swAutoSize->IsChecked)
		PresentedScrollBox1->ContentSize->Size = TSizeF(spContentWidth->Value, spContentHeight->Value);
	else
	{
		spContentWidth->Value = PresentedScrollBox1->ContentSize->Width;
		spContentHeight->Value = PresentedScrollBox1->ContentSize->Height;
	}
	UpdateViewportSize();
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::spContentWidthChange(TObject *Sender)
{
	PresentedScrollBox1->ContentSize->Width = spContentWidth->Value;
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::spContentHeightChange(TObject *Sender)
{
	PresentedScrollBox1->ContentSize->Height = spContentHeight->Value;
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::Button4Click(TObject *Sender)
{
	PresentedScrollBox1->ScrollToTop(true);
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::Button5Click(TObject *Sender)
{
	PresentedScrollBox1->ScrollBy(100, 100);
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::Button6Click(TObject *Sender)
{
	// Right bottom corner, Size less ViewPortSize
	PresentedScrollBox1->InViewRect(TRectF(900, 900, 1500, 1500));
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::Button2Click(TObject *Sender)
{
	PresentedScrollBox1->ScrollToCenter(true);
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::spViewPortPositionXChange(TObject *Sender)
{
	PresentedScrollBox1->ViewportPosition = TPointF(spViewPortPositionX->Value, spViewPortPositionY->Value);
}
//---------------------------------------------------------------------------

void __fastcall TFormMain::spViewPortPositionYChange(TObject *Sender)
{
	PresentedScrollBox1->ViewportPosition = TPointF(spViewPortPositionX->Value, spViewPortPositionY->Value);
}
//---------------------------------------------------------------------------
void __fastcall TFormMain::UpdateViewportPositionWithoutOnChange(void)
{
  TNotifyEvent OldChange = spViewPortPositionX->OnChange;
  try{
	spViewPortPositionX->OnChange = NULL;
	spViewPortPositionY->OnChange = NULL;
	spViewPortPositionX->Value = PresentedScrollBox1->ViewportPosition.X;
	spViewPortPositionY->Value = PresentedScrollBox1->ViewportPosition.Y;
  }
  __finally {
	spViewPortPositionX->OnChange = OldChange;
	spViewPortPositionY->OnChange = OldChange;
  }
}

void __fastcall TFormMain::UpdateViewportSize(void)
{
  spViewPortWidth->Value = PresentedScrollBox1->ViewportSize.Width;
  spViewPortHeight->Value = PresentedScrollBox1->ViewportSize.Height;
}
