//---------------------------------------------------------------------------

#ifndef uMainH
#define uMainH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Media.hpp>
#include <FMX.Objects.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <FMX.MobilePreview.hpp>
//---------------------------------------------------------------------------
class TCameraComponentForm : public TForm
{
__published:	// IDE-managed Components
	TButton *btnStartCamera;
	TButton *btnStopCamera;
	TLabel *lblCameraType;
	TLabel *cbCameraFlashType;
	TCameraComponent *CameraComponent1;
	TImage *imgCameraView;
	TSpeedButton *btnFrontCamera;
	TSpeedButton *btnBackCamera;
	TSpeedButton *btnOn;
	TSpeedButton *btnOff;
	TSpeedButton *btnAuto;
	void __fastcall btnAutoClick(TObject *Sender);
	void __fastcall btnBackCameraClick(TObject *Sender);
	void __fastcall btnFrontCameraClick(TObject *Sender);
	void __fastcall btnOnClick(TObject *Sender);
	void __fastcall btnOffClick(TObject *Sender);
	void __fastcall btnStartCameraClick(TObject *Sender);
	void __fastcall btnStopCameraClick(TObject *Sender);
	void __fastcall CameraComponent1SampleBufferReady(TObject *Sender, const __int64 ATime);
	void __fastcall FormCreate(TObject *Sender);

private:	// User declarations
	void __fastcall GetImage();
public:		// User declarations
	__fastcall TCameraComponentForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TCameraComponentForm *CameraComponentForm;
//---------------------------------------------------------------------------
#endif
