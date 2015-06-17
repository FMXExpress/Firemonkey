//---------------------------------------------------------------------------

#ifndef Unit5H
#define Unit5H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <System.Beacon.hpp>
#include <System.UiTypes.hpp>
//---------------------------------------------------------------------------
class TForm5 : public TForm
{
__published:	// IDE-managed Components
	TLabel *BeaconType;
	TButton *Button1;
	TLabel *Company;
	TLabel *MajorMinor;
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall FormShow(TObject *Sender);
private:	// User declarations
	Boolean FisScanning;
	TBeaconManager *FBeaconManager;
	void __fastcall BeaconProximity(System::TObject* const Sender, const _di_IBeacon ABeacon, TBeaconProximity Proximity);
public:		// User declarations
	__fastcall TForm5(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm5 *Form5;
//---------------------------------------------------------------------------
#endif
