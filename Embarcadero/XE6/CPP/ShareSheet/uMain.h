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
class TfrmShareSheet : public TForm
{
__published:	// IDE-managed Components
	TToolBar *TopToolbar;
	TLabel *Label1;
	TToolBar *BottomToolbar;
	TButton *btnShare;
	TButton *btnTakePhoto;
	TImage *imgCameraPicture;
	TActionList *ActionList1;
	TTakePhotoFromCameraAction *TakePhotoFromCameraAction1;
	TShowShareSheetAction *ShowShareSheetAction1;
	void __fastcall ShowShareSheetAction1BeforeExecute(TObject *Sender);
	void __fastcall TakePhotoFromCameraAction1DidFinishTaking(TBitmap *Image);
private:	// User declarations
public:		// User declarations
	__fastcall TfrmShareSheet(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmShareSheet *frmShareSheet;
//---------------------------------------------------------------------------
#endif
