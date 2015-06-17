//---------------------------------------------------------------------------

#ifndef MainFrmH
#define MainFrmH
#include <FMX.ActnList.hpp>
#include <FMX.Ani.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.MediaLibrary.Actions.hpp>
#include <FMX.Objects.hpp>
#include <FMX.StdActns.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <System.Actions.hpp>
#include <System.Classes.hpp>
#include <FMX.MobilePreview.hpp>
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
class TBaseMainForm : public TForm
{
__published:	// IDE-managed Components
	TImage *ImageContainer;
	TToolBar *ToolBarBottom;
	TButton *ButtonTakePhotoFromCamera;
	TButton *ButtonRemovePhoto;
	TFloatAnimation *RemoveBtnAnimation;
	TButton *ButtonSendImage;
	TToolBar *ToolBarTop;
	TButton *ButtonTakePhotoFromLibrary;
	TSpeedButton *SpeedButton1;
	TSpeedButton *SpeedButton2;
	TSpeedButton *SpeedButton3;
	TSpeedButton *SpeedButton4;
	TLayout *LayoutFilterSettings;
	TActionList *ActionList;
	TTakePhotoFromLibraryAction *ActionTakePhotoFromLibrary;
	TTakePhotoFromCameraAction *ActionTakePhotoFromCamera;
	TShowShareSheetAction *ActionShowShareSheet;
	TAction *ActionBlurEffect;
	TAction *ActionPixelateEffect;
	TAction *ActionSharpenEffect;
	TAction *ActionResetEffect;
	TAction *ActionClearImage;
	TLayout *TopHelp;
	TImage *Image1;
	TText *Text1;
	TImage *Image2;
	TText *Text2;
	TSpeedButton *SpeedButton11;
	TSpeedButton *SpeedButton7;
	TSpeedButton *SpeedButton8;
	TAction *ActionWaveEffect;
	TAction *ActionContrastEffect;
	TAction *ActionPaperSketchEffect;
	void __fastcall ActionListUpdate(TBasicAction *Action, bool &Handled);
	void __fastcall ActionBlurEffectExecute(TObject *Sender);
	void __fastcall ActionPixelateEffectExecute(TObject *Sender);
	void __fastcall ActionSharpenEffectExecute(TObject *Sender);
	void __fastcall ActionResetEffectExecute(TObject *Sender);
	void __fastcall ActionShowShareSheetBeforeExecute(TObject *Sender);
	void __fastcall ActionClearImageExecute(TObject *Sender);
	void __fastcall ActionTakePhotoFromLibraryDidFinishTaking(TBitmap *Image);
	void __fastcall ActionWaveEffectExecute(TObject *Sender);
	void __fastcall ActionContrastEffectExecute(TObject *Sender);
	void __fastcall ActionPaperSketchEffectExecute(TObject *Sender);
protected:	// User declarations
	TBitmap *FRawBitmap;
	TFilter *FEffect;
	void __fastcall DoOnChangedEffectParam(TObject *Sender);
	void __fastcall LoadFilterSettings(TFilterRec Rec);
public:		// User declarations
	__fastcall TBaseMainForm(TComponent* Owner);
	__fastcall ~TBaseMainForm();
	void __fastcall SetEffect(const String AFilterName);
	void __fastcall UpdateEffect();
};
//---------------------------------------------------------------------------
extern PACKAGE TBaseMainForm *BaseMainForm;
//---------------------------------------------------------------------------
#endif
