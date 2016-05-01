//---------------------------------------------------------------------------

#ifndef uMainH
#define uMainH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.ActnList.hpp>
#include <FMX.MediaLibrary.Actions.hpp>
#include <FMX.Objects.hpp>
#include <FMX.StdActns.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <System.Actions.hpp>
#include <FMX.MobilePreview.hpp>
//---------------------------------------------------------------------------
class TCameraRollForm : public TForm
{
__published:	// IDE-managed Components
	TImage *imgPhotoLibraryImage;
	TToolBar *ToolBar1;
	TLabel *Label1;
	TButton *btnPhotoLibrary;
	TActionList *alGetCameraRoll;
	TTakePhotoFromLibraryAction *TakePhotoFromLibraryAction1;
	void __fastcall TakePhotoFromLibraryAction1DidFinishTaking(TBitmap *Image);
private:	// User declarations
public:		// User declarations
	__fastcall TCameraRollForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TCameraRollForm *CameraRollForm;
//---------------------------------------------------------------------------
#endif
