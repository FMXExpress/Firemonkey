//---------------------------------------------------------------------------

#ifndef UBeaconDevDemoH
#define UBeaconDevDemoH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Ani.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.Edit.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <System.Beacon.Components.hpp>
#include <System.Beacon.hpp>
//---------------------------------------------------------------------------
class TForm8 : public TForm
{
__published:	// IDE-managed Components
	TBeaconDevice *BeaconDevice1;
	TImageControl *ImageControl1;
	TBitmapListAnimation *Animation;
	TPanel *Panel2;
	TButton *BtnEnableBeacon;
	TPanel *PnlBeaconInfo;
	TEdit *EdtBeaconUUID;
	TEdit *EdtBeaconMajor;
	TEdit *EdtBeaconMinor;
	TLabel *Label3;
	TLabel *Label4;
	TLabel *Label5;
	TLabel *Label1;
	TButton *BtnRandom;
	TLabel *Label6;
	TEdit *EdTxPower;
	TStyleBook *StyleBook1;
	void __fastcall BtnRandomClick(TObject *Sender);
	void __fastcall BtnEnableBeaconClick(TObject *Sender);
private:	// User declarations
	TGUID FGuid;
	Integer FMajor;
	Integer FMinor;
	Integer FTxPower;

	bool __fastcall CheckValues();
public:		// User declarations
	__fastcall TForm8(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm8 *Form8;
//---------------------------------------------------------------------------
#endif
