//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

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
#include <FMX.ListBox.hpp>
#include <System.Bluetooth.hpp>
//---------------------------------------------------------------------------
class TForm8 : public TForm
{
__published:	// IDE-managed Components
	TBeaconDevice *BeaconDevice1;
	TStyleBook *StyleBook1;
	TPanel *Panel2;
	TButton *BtnEnableBeacon;
	TPanel *PnlBeaconInfo;
	TEdit *EdtBeaconUUID;
	TEdit *EdtBeaconMajor;
	TEdit *EdtBeaconMinor;
	TLabel *Label3;
	TLabel *Label4;
	TLabel *Label5;
	TLabel *Label6;
	TEdit *EdTxPower;
	TComboBox *CbbBeaconType;
	TLabel *Label2;
	TButton *BtnRandom;
	TPanel *PnlEddyInfo;
	TComboBox *CbbNamespaceGeneration;
	TEdit *EdtEddyInstance;
	TEdit *EdtEddyNamespace;
	TEdit *EdtEddyURL;
	TLabel *Label10;
	TLabel *Label7;
	TLabel *Label8;
	TLabel *Label9;
	TImageControl *ImageControl1;
	TBitmapListAnimation *Animation;
	TButton *BtnRandomNamespace;
	void __fastcall BtnRandomClick(TObject *Sender);
	void __fastcall BtnEnableBeaconClick(TObject *Sender);
	void __fastcall CbbBeaconTypeChange(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall BtnRandomNamespaceClick(TObject *Sender);
private:	// User declarations
	TGUID FGuid;
	Integer FMajor;
	Integer FMinor;
	Integer FTxPower;
	String FEddyNamespace;
	String FEddyInstance;
	String FEddyURL;

	bool __fastcall CheckUUID();
	bool __fastcall CheckValues();
	void __fastcall SetNamespaceGeneration();
	String __fastcall RandomHexString(int ABytesCount);

public:		// User declarations
	__fastcall TForm8(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm8 *Form8;
//---------------------------------------------------------------------------
#endif
