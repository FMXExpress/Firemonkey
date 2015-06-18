//---------------------------------------------------------------------------

#ifndef UFrmHomeH
#define UFrmHomeH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <Data.Bind.Components.hpp>
#include <Data.Bind.ObjectScope.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.Objects.hpp>
#include <FMX.Types.hpp>
#include <IPPeerClient.hpp>
#include <REST.Backend.BindSource.hpp>
#include <REST.Backend.KinveyProvider.hpp>
#include <REST.Backend.KinveyPushDevice.hpp>
#include <REST.Backend.PushDevice.hpp>
#include <REST.Backend.PushTypes.hpp>
#include <REST.OpenSSL.hpp>
#include <System.JSON.hpp>
#include <System.PushNotification.hpp>
#include <FMX.StdCtrls.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TKinveyProvider *KinveyProvider1;
	TPushEvents *PushEvents1;
	TRectangle *Rectangle1;
	TRectangle *Rectangle2;
	TListBox *ListBox1;
	TSpeedButton *SpeedButton1;
	TLabel *Label1;
	void __fastcall PushEvents1DeviceTokenReceived(TObject *Sender);
	void __fastcall PushEvents1DeviceTokenRequestFailed(TObject *Sender, const UnicodeString AErrorMessage);
	void __fastcall PushEvents1PushReceived(TObject *Sender, TPushData * const AData);


private:	// User declarations
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
