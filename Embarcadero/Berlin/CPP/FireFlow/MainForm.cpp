//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#include <fmx.h>
#include <FMX.Effects.hpp>
#pragma hdrstop

#include "MainForm.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
#pragma resource ("*.Windows.fmx", _PLAT_MSWINDOWS)

TFrmMain *FrmMain;
//---------------------------------------------------------------------------

const float Factor = 0.8;
const int DivFac  = 3;
const int RotationYAngle = 70;
const float DurationVal = 0.5;


__fastcall TFrmMain::TFrmMain(TComponent* Owner) : TForm(Owner)
{
}

static __int64 Round(double n)
{
  // NOTE: Not like Delphi's Round
  return (n > 0.0) ? floor(n + 0.5) : ceil(n - 0.5);
}

static int Trunc(double n)
{
  return (n > 0.0) ? floor(n + 0.5) : ceil(n - 0.5);
}

//---------------------------------------------------------------------------
void __fastcall TFrmMain::AddFolder()
{
  OpenDialog1->Filter = "All Images|" + TBitmapCodecManager::GetFileTypes();

  if (OpenDialog1->Execute())
  {
    Coverflow->DeleteChildren();
    AniIndicator1->Visible = true;
    System::String Dir = ExtractFilePath(OpenDialog1->FileName);
    int i = 0;

     // create a FilterPredicate and get the image file list

    TDirectory::_di_TFilterPredicate predicate(new TFlowFilterPredicate());
    System::TStringDynArray files = TDirectory::GetFiles(Dir, predicate);

    for (int j = 0; j < files.Length; j++)
    {
      System::String FileName = files[j];

      // Create Cover
      TLayer3D *Cover = new TLayer3D(this);
      Cover->Parent = Coverflow;

	  Cover->Projection = TProjection::Screen;
      Cover->Width = Round(Coverflow->Height * Factor);
      Cover->Height = Round(Round(Coverflow->Height * Factor) * 1.5);
      Cover->ZWrite = True;
      Cover->Fill->Color = Viewport3D1->Color;
	  Cover->Fill->Kind  = TBrushKind::Solid;
      Cover->Transparency = True;
      Cover->OnLayerMouseDown = DoCoverMouseDown;
      Cover->Tag = i;
      Cover->Padding->Rect = TRectF(0, 0, 0, 0);
      Cover->Position->Y = Trunc((Coverflow->Height + Round(Coverflow->Height * Factor)) / 2);
      Cover->Cursor = crHandPoint;

      if (i == 0)
      {
        Cover->Position->X = Coverflow->Width/2;
      }
      else
      {
        Cover->Position->X = (i + 1) * (Round(Coverflow->Height * Factor) / DivFac) + Coverflow->Width/2;
        Cover->Position->Z = Round(Coverflow->Height * Factor) * 2;
        Cover->RotationAngle->Y = RotationYAngle;
      }

      // Child
      TLayout *Layout = new TLayout(this);
      Layout->Parent = Cover;
	  Layout->Align = TAlignLayout::Top;
      Layout->Height = Trunc(Cover->Height / 2); // original = 2
      Layout->Padding->Rect = TRectF(0, 0, 0, 0);
	  Layout->Cursor = crHandPoint;

      // This rectangle is necessary to avoid blank lines on the image
      TRectangle *L = new TRectangle(this);
      L->Parent = Layout;
	  L->Align = TAlignLayout::Top;
      L->Height = Trunc(Cover->Height / 2);
	  L->Fill->Kind = TBrushKind::None;
      L->Stroke->Color = Viewport3D1->Color;
	  L->Stroke->Kind = TBrushKind::None;

      TImage *Image = new TImage(this);
      Image->Parent = Layout;
      Image->Padding->Rect = TRectF(0, 0, 0, 0);
      Image->TagString = FileName;

      TPointF imageSize;
      imageSize = TBitmapCodecManager::GetImageSize(FileName);
      Image->Width = imageSize.X;
      Image->Height = imageSize.Y;


	  Image->WrapMode = TImageWrapMode::Stretch;
	  Image->Align = TAlignLayout::Fit;
      Image->HitTest = true;
      Image->TagString = FileName;
      Image->Cursor = crHandPoint;

      TImageThread *imageThread = new TImageThread(*Image, Image->TagString);
      imageThread->Start();
      Image->OnMouseDown = DoCoverMouseDown;
  //      Image->OnMouseWheel = CoverflowMouseWheel;

      Image->Tag = i;

      TReflectionEffect *Effect = new TReflectionEffect(this);
      Effect->Parent = Image;
      Effect->Opacity = 0.6;

      // Opacity animation
      Cover->Opacity = 0.01;
      Cover->AnimateFloat("Opacity", 1, DurationVal);

      // Load thumb
      Cover->TagObject = Image;

      i++;

      Application->ProcessMessages();
    }

    CoverIndex = 0;
    AniIndicator1->Visible = False;
    TrackBar1->Max = Coverflow->ChildrenCount-1;
    TrackBar1->Value = 0;
    TrackBar1->Visible = True;
    TrackBar1->SetFocus();
  }
}

void __fastcall TFrmMain::CoverScrollChange(TObject *Sender)
{
  SetCoverIndex(Round(TrackBar1->Value));
}

//---------------------------------------------------------------------------
void __fastcall TFrmMain::SetCoverIndex(int AIndex)
{
  int i;
  TLayer3D *Cover;
  float PercCoeff, Coeff;

  if (AniIndicator1->Visible || Coverflow->ChildrenCount == 0)
  {
     TrackBar1->Value = CoverIndex;
     return;
  }

  PercCoeff = 0.6;

  if (AIndex < 0)
    AIndex = 0;
  if (AIndex >= Coverflow->ChildrenCount)
    AIndex = Coverflow->ChildrenCount - 1;
  if (AIndex != CoverIndex)
  {
    // translate all; move the pictures in the back.
    // note here that the properties passed to AnimateFloat etc. use Delphi punctuation

    for (i = 0; i < Coverflow->ChildrenCount; i++)
    {
      Cover = (TLayer3D*)Coverflow->Children->Items[i];
      Cover->StopPropertyAnimation("Position.X");
      Cover->AnimateFloat("Position.X", Cover->Position->X + ((CoverIndex - AIndex) * (Round(Coverflow->Height * Factor) / DivFac)), DurationVal);
    }

    // transform between old an new value
    i = CoverIndex;
    while (i != AIndex)
    {
      Coeff = (0.1 + (abs(AIndex - i) / abs(AIndex - CoverIndex))) * (PercCoeff + 0.1);

      Cover = (TLayer3D *)Coverflow->Children->Items[i];
      Cover->StopPropertyAnimation("Position.X");
      Cover->StopPropertyAnimation("RotationAngle.Y");

      if (CoverIndex > AIndex)
      {
        Cover->AnimateFloat("RotationAngle.Y", RotationYAngle, DurationVal );
        if (i == CoverIndex)
          Cover->AnimateFloat("Position.X", Cover->Position->X + (1 * (Round(Coverflow->Height * Factor) / DivFac)), DurationVal * Coeff);
        else
          Cover->AnimateFloat("Position.X", Cover->Position->X + (2 * (Round(Coverflow->Height * Factor) / DivFac)), DurationVal * Coeff);
      }
      else
      {
        Cover->AnimateFloat("RotationAngle.Y", RotationYAngle*-1, DurationVal );
        if (i == CoverIndex)
          Cover->AnimateFloat("Position.X", Cover->Position->X - (1 * (Round(Coverflow->Height * Factor) / DivFac)), DurationVal * Coeff);
        else
          Cover->AnimateFloat("Position.X", Cover->Position->X - (2 * (Round(Coverflow->Height * Factor) / DivFac)), DurationVal * Coeff);
      }
      Cover->AnimateFloat("Position.Z", Round(Coverflow->Height * Factor) * 2, DurationVal);
      if (AIndex > CoverIndex)
        i++;
      else
        i--;
    }

    Cover = (TLayer3D *)Coverflow->Children->Items[AIndex];

    Cover->StopPropertyAnimation("Position.X");
    Cover->StopPropertyAnimation("Position.Z");

    Cover->AnimateFloat("RotationAngle.Y", 0, DurationVal);
    Cover->AnimateFloat("Position.Z", 0, DurationVal);
    if (CoverIndex > AIndex)
       Cover->AnimateFloat("Position.X", Cover->Position->X + (1 * (Round(Coverflow->Height * Factor) / DivFac)), DurationVal);
    else
      Cover->AnimateFloat("Position.X", Cover->Position->X - (1 * (Round(Coverflow->Height * Factor) / DivFac)), DurationVal);

    CoverIndex = AIndex;
  }
}

//---------------------------------------------------------------------------
void __fastcall TFrmMain::DoCoverMouseDown(TObject *Sender, TMouseButton Button,
  TShiftState Shift, float X, float Y)
{
  TrackBar1->Value = Round(StrToFloat(IntToStr(((TImage *)(Sender))->Tag)));
}
//---------------------------------------------------------------------------

void __fastcall TFrmMain::CoverflowMouseWheel(TObject *Sender, TShiftState Shift,
        int WheelDelta, bool &Handled)
{
  TrackBar1->Value = CoverIndex - (WheelDelta / 120);
  Handled = true;

}
//---------------------------------------------------------------------------
void __fastcall TFrmMain::TrackBar1Change(TObject *Sender)
{
  SetCoverIndex(Round(TrackBar1->Value));
}
//---------------------------------------------------------------------------
void __fastcall TFrmMain::Button1Click(TObject *Sender)
{
  AddFolder();
}
//---------------------------------------------------------------------------

void __fastcall TImageThread::Execute()
{
//  TThread::NameThreadForDebugging(FFileName);
  FTempBitmap = new TBitmap(0, 0);
  FTempBitmap->LoadThumbnailFromFile(FFileName,
                                     const_cast<TImage*>(FImage)->Width,
                                     const_cast<TImage*>(FImage)->Height,
                                     false);
  Synchronize(Finished);
}

void __fastcall TImageThread::Finished()
{
  FImage->Bitmap->Assign(FTempBitmap);
  delete FTempBitmap;
}

__fastcall TImageThread::TImageThread(const TImage &AImage, const String AFileName):
     TThread(true), FImage( const_cast<TImage* const> (&AImage)), FFileName(AFileName)
{
  FreeOnTerminate = True;
}


