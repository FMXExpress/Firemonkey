//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef MainFormH
#define MainFormH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Forms3D.hpp>
#include <FMX.Dialogs.hpp>
#include <FMX.Layers3D.hpp>
#include <FMX.Objects.hpp>
#include <FMX.Types.hpp>
#include <FMX.Types3D.hpp>
#include <FMX.Effects.hpp>
#include <System.IOUtils.hpp>
#include <FMX.Controls3D.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Viewport3D.hpp>
#include <System.Math.Vectors.hpp>
//---------------------------------------------------------------------------
class TFrmMain : public TForm
{
__published:	// IDE-managed Components
  TRectangle *Rectangle1;
  TAniIndicator *AniIndicator1;
  TButton *Button1;
  TTrackBar *TrackBar1;
  TStyleBook *ResourcesDark;
  TViewport3D *Viewport3D1;
  TLayout3D *Coverflow;
  TOpenDialog *OpenDialog1;
  void __fastcall CoverScrollChange(TObject *Sender);
  void __fastcall CoverflowMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
      bool &Handled);
  void __fastcall TrackBar1Change(TObject *Sender);
  void __fastcall Button1Click(TObject *Sender);
private:	// User declarations
  int CoverIndex;

  void __fastcall AddFolder();
  void __fastcall SetCoverIndex(int AIndex);
  void __fastcall DoCoverMouseDown(TObject *Sender, TMouseButton Button,
    TShiftState Shift, float X, float Y);
public:		// User declarations
  __fastcall TFrmMain(TComponent* Owner);
};


/*
 **************************************************************
  TFlowFilterPredicate - defines a class containing the
  TFilterPredicate callback to retrieve the OS image file
  extensions, used here to validate the user-selected file.
 **************************************************************
*/

class TFlowFilterPredicate : public TCppInterfacedObject<TDirectory::TFilterPredicate> {
public:
  bool __fastcall Invoke(const System::UnicodeString Path,
                         const System::Sysutils::TSearchRec &SearchRec)
  {
   bool filterResult = System::Ioutils::TPath::MatchesPattern(SearchRec.Name,
                                                              "*.*", False);
   if (filterResult) { // Find out if it is an image file
     System::String imagesExt = TBitmapCodecManager::GetFileTypes();
     filterResult =imagesExt.Pos(LowerCase(ExtractFileExt(SearchRec.Name))) > 0;
   }
   return filterResult;
  }
};


class TImageThread : public TThread
{
private:
  TImage* const FImage;
  TBitmap *FTempBitmap;
  String FFileName;
protected:
  void __fastcall Execute();
  void __fastcall Finished();
public:
  __fastcall TImageThread(const TImage &AImage, const String AFileName);
};


//---------------------------------------------------------------------------
extern PACKAGE TFrmMain *FrmMain;
//---------------------------------------------------------------------------
#endif
