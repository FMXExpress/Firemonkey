//---------------------------------------------------------------------------

#ifndef Unit3H
#define Unit3H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.Memo.hpp>
#include <FMX.ScrollBox.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <System.Beacon.Components.hpp>
#include <System.Beacon.hpp>
//---------------------------------------------------------------------------
class TForm3 : public TForm
{
__published:	// IDE-managed Components
	TBeacon *Beacon1;
	TButton *Button1;
	TMemo *Memo1;
	void __fastcall Beacon1BeaconEnter(TObject * const Sender, IBeacon * const ABeacon, const TBeaconList CurrentBeaconList);
	void __fastcall Beacon1BeaconExit(TObject * const Sender, IBeacon * const ABeacon, const TBeaconList CurrentBeaconList);
	void __fastcall Button1Click(TObject *Sender);


private:	// User declarations
public:		// User declarations
	__fastcall TForm3(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm3 *Form3;
//---------------------------------------------------------------------------
#endif
