//---------------------------------------------------------------------------

// This software is Copyright (c) 2016 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef ConnectionFrameUH
#define ConnectionFrameUH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.Edit.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <IPPeerClient.hpp>
#include <REST.Backend.EMSProvider.hpp>
//---------------------------------------------------------------------------
class TEMSServerConnectionFrame : public TFrame
{
__published:	// IDE-managed Components
	TLayout *Layout1;
	TGroupBox *GroupBox1;
	TButton *ButtonTestConnection;
	TEdit *EditURLHost;
	TLabel *LabelURLHost;
	TEdit *EditURLPort;
	TLabel *LabelURLPort;
	void __fastcall EditURLHostChange(TObject *Sender);
	void __fastcall EditURLPortChange(TObject *Sender);
	void __fastcall ButtonTestConnectionClick(TObject *Sender);
private:	// User declarations
	TEMSProvider * FEMSProvider;
	void __fastcall EMSProviderChanged(void);
	TEMSProvider * __fastcall GetEMSProvider(void);
	void __fastcall SetEMSProvider(const TEMSProvider * Value);
public:		// User declarations
	__fastcall TEMSServerConnectionFrame(TComponent* Owner);
protected:
	void __fastcall Notification(TComponent* AComponent, TOperation Operation);
public:
	__property TEMSProvider * EMSProvider={read=GetEMSProvider, write=SetEMSProvider};
};
//---------------------------------------------------------------------------
extern PACKAGE TEMSServerConnectionFrame *EMSServerConnectionFrame;
//---------------------------------------------------------------------------
#endif
