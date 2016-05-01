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

#include "MainForm.h"
#include "DebugDraw.h"
#include "Test.h"
#include "DrawUtil.h"
#include <System.Character.hpp>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TTestBedForm *TestBedForm;


namespace {
  int32 testIndex = 0;
  int32 testSelection = 0;
  int32 testCount = 0;
  TestEntry* entry;
  Test* test;
  Settings settings;
  bool rightMouseDown;
  b2Vec2 lastp;
}

//---------------------------------------------------------------------------
__fastcall TTestBedForm::TTestBedForm(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TTestBedForm::FormCreate(TObject *Sender)
{
  LoadTests();
}
//---------------------------------------------------------------------------

void __fastcall TTestBedForm::FormDestroy(TObject *Sender)
{
  Timer1->Enabled = false;
  Test* t = test;
  test = 0;
  delete t;
}
//---------------------------------------------------------------------------

void TTestBedForm::LoadTests()
{
  testCount = 0;
  while (g_testEntries[testCount].createFcn != NULL)
  {
    TestEntry& entry = g_testEntries[testCount];
    TestList->Items->Add(entry.name);
    ++testCount;
  }

  TestList->ItemIndex = 0;
  testSelection = 0;
  testIndex = -1;
  test = 0;
}
//---------------------------------------------------------------------------

void TTestBedForm::TestChanged()
{
  if (testSelection != testIndex)
  {
    testIndex = testSelection;
    delete test;
    entry = g_testEntries + testIndex;
    test = entry->createFcn();
  }
}
//---------------------------------------------------------------------------

void TTestBedForm::ResetView()
{
  g_debugDraw.ScaleX = DEFAULT_SCALE;
  g_debugDraw.ScaleY = DEFAULT_SCALE;
  g_debugDraw.CanvasHeight = PaintBox->Height;
  g_camera.m_width = PaintBox->Width;
  g_camera.m_height = PaintBox->Height;
  g_camera.m_center.Set(PaintBox->Width/2, PaintBox->Height/2);
}
//---------------------------------------------------------------------------

void __fastcall TTestBedForm::PaintBoxPaint(TObject *Sender, TCanvas *Canvas)
{
  Canvas->BeginScene();
  __try
  {
	Canvas->Fill->Color = b2Color2TColor(b2Color(0.3, 0.3, 0.3, 1));
	TRectF rect = PaintBox->BoundsRect;
	Canvas->FillRect(rect, 0, 0, TCorners(), DEFAULT_OPACITY);

	if (test)
	{
	  g_debugDraw.Canvas = Canvas;
	  g_debugDraw.Canvas->Fill->Color = TAlphaColorRec::Yellow;
	  test->DrawTitle(entry->name);
	  g_debugDraw.Canvas->Fill->Color = TAlphaColorRec::Aqua;
	  test->Step(&settings);
	}
  }
  __finally
  {
	Canvas->EndScene();
  }
}
//---------------------------------------------------------------------------

void __fastcall TTestBedForm::FormShow(TObject *Sender)
{
  ResetView();
  DrawLayoutChanged(0);
  TestChanged();
}
//---------------------------------------------------------------------------

void __fastcall TTestBedForm::FormResize(TObject *Sender)
{
  ResetView();
}
//---------------------------------------------------------------------------

void __fastcall TTestBedForm::Timer1Timer(TObject *Sender)
{
  if (test)
  {
    Invalidate();
  }
}
//---------------------------------------------------------------------------

void __fastcall TTestBedForm::TestListChange(TObject *Sender)
{
  if (TestList->ItemIndex != testSelection)
  {
    testSelection = TestList->ItemIndex;
    TestChanged();
  }
}
//---------------------------------------------------------------------------

void __fastcall TTestBedForm::FormKeyDown(TObject *Sender, WORD &Key, System::WideChar &KeyChar,
          TShiftState Shift)
{
  bool modCtrl = Shift.Contains(ssCtrl);
  switch(Key)
  {
  case vkEscape:
    // Quit
    Close();
    break;

  case vkLeft:
    // Pan left
    if (modCtrl && test)
    {
      b2Vec2 newOrigin(2.0f, 0.0f);
      test->ShiftOrigin(newOrigin);
    }
    else
    {
      g_camera.m_center.x -= 0.5f;
    }
    break;

  case vkRight:
    // Pan right
    if (modCtrl && test)
    {
      b2Vec2 newOrigin(-2.0f, 0.0f);
      test->ShiftOrigin(newOrigin);
    }
    else
    {
      g_camera.m_center.x += 0.5f;
    }
    break;

  case vkDown:
    // Pan down
    if (modCtrl && test)
    {
      b2Vec2 newOrigin(0.0f, 2.0f);
      test->ShiftOrigin(newOrigin);
    }
    else
    {
      g_camera.m_center.y -= 0.5f;
    }
    break;

  case vkUp:
    // Pan up
    if (modCtrl && test)
    {
      b2Vec2 newOrigin(0.0f, -2.0f);
      test->ShiftOrigin(newOrigin);
    }
    else
    {
      g_camera.m_center.y += 0.5f;
    }
    break;

  case vkHome:
    // Reset view
    g_camera.m_zoom = 1.0f;
    g_camera.m_center.Set(0.0f, 20.0f);
    break;

  case vkSpace:
    // Launch a bomb.
    if (test)
    {
      test->LaunchBomb();
    }
    break;

  default:
    switch (KeyChar)
    {
    case 'P':
    case 'p':
      // Pause
      settings.pause = !settings.pause;
      break;

    case ' ':
      // Launch a bomb.
      if (test)
      {
        test->LaunchBomb();
      }
      break;

    case 'R':
    case 'r':
      // Reset test
      delete test;
      test = entry->createFcn();
      break;

    default:
      if (test)
      {
        test->Keyboard(static_cast<int>(ToUpper(KeyChar)));
      }
    }
  }
}
//---------------------------------------------------------------------------

void __fastcall TTestBedForm::FormKeyUp(TObject *Sender, WORD &Key, System::WideChar &KeyChar,
          TShiftState Shift)
{
  if (test)
  {
    test->KeyboardUp(static_cast<int>(ToUpper(KeyChar)));
  }
}
//---------------------------------------------------------------------------

void __fastcall TTestBedForm::PaintBoxMouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, float X, float Y)
{
  if (Button == TMouseButton::mbLeft)
  {
    if (test)
    {
      b2Vec2 pt = g_debugDraw.ScreenToWorld(X, Y);
      if (Shift.Contains(ssShift))
        test->ShiftMouseDown(pt);
      else
        test->MouseDown(pt);
    }
  }
}
//---------------------------------------------------------------------------

void __fastcall TTestBedForm::PaintBoxMouseMove(TObject *Sender, TShiftState Shift,
          float X, float Y)
{
  if (test)
  {
    b2Vec2 pt = g_debugDraw.ScreenToWorld(X, Y);
    test->MouseMove(pt);
  }
}
//---------------------------------------------------------------------------

void __fastcall TTestBedForm::PaintBoxMouseUp(TObject *Sender, TMouseButton Button,
          TShiftState Shift, float X, float Y)
{
  if (test)
  {
    b2Vec2 pt = g_debugDraw.ScreenToWorld(X, Y);
    test->MouseUp(pt);
  }
}
//---------------------------------------------------------------------------


void __fastcall TTestBedForm::DrawLayoutChanged(TObject *Sender)
{
  settings.drawShapes = this->ShapesChk->IsChecked;
  settings.drawJoints = this->JointsChk->IsChecked;
  settings.drawAABBs = this->AABBsChk->IsChecked;
  settings.drawContactPoints = this->ContactPointsChk->IsChecked;
  settings.drawContactNormals = this->ContactNormalsChk->IsChecked;
  settings.drawContactImpulse = this->ContactImpulsesChk->IsChecked;
  settings.drawFrictionImpulse = this->FrictionImpulsesChk->IsChecked;
  settings.drawCOMs = this->CenterOfMassesChk->IsChecked;
  settings.drawStats = this->StatisticsChk->IsChecked;
  settings.drawProfile = this->ProfileChk->IsChecked;
}
//---------------------------------------------------------------------------

void __fastcall TTestBedForm::PauseBtnClick(TObject *Sender)
{
  settings.pause = !settings.pause;
}
//---------------------------------------------------------------------------

void __fastcall TTestBedForm::SingleStepBtnClick(TObject *Sender)
{
  settings.singleStep = !settings.singleStep;
}
//---------------------------------------------------------------------------

void __fastcall TTestBedForm::RestartBtnClick(TObject *Sender)
{
  delete test;
  test = entry->createFcn();
}
//---------------------------------------------------------------------------

void __fastcall TTestBedForm::QuiBtnClick(TObject *Sender)
{
  Close();
}
//---------------------------------------------------------------------------


