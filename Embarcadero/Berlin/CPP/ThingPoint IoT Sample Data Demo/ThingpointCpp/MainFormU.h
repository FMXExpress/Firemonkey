//---------------------------------------------------------------------------

// This software is Copyright (c) 2016 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef MainFormUH
#define MainFormUH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include "ConnectionFrameU.h"
#include <FMX.Types.hpp>
#include "EdgeServiceModuleU.h"
#include "ListenerFrameU.h"
#include "LoggingFrameU.h"
#include "StreamingDeviceFrameU.h"
#include <FMX.TabControl.hpp>
#include "CacheDataModuleU.h"
#include "NotifyDeviceFrameU.h"
#include <FMX.Layouts.hpp>
//---------------------------------------------------------------------------
class TEdgeMainForm : public TForm
{
__published:	// IDE-managed Components
	TEMSServerConnectionFrame *EMSServerConnectionFrame1;
	TEMSEdgeModuleListenerFrame *EMSEdgeModuleListenerFrame1;
	TEMSEdgeLoggingFrame *EMSEdgeLoggingFrame1;
	TTabControl *TabControl1;
	TTabItem *TabItem1;
	TStreamingDeviceFrame *HeartRateFrame;
	TTabItem *TabItem2;
	TNotifyDeviceFrame *BloodPressureFrame;
	TLayout *Layout1;
	void __fastcall FormCreate(TObject *Sender);
private:	// User declarations
	void __fastcall OnModuleOverwrite(String AModuleName,  bool &AResult);
	void __fastcall OnProtocolPropsConflictDelete(String AModuleName, String AProtocolProps,  bool &AResult);
	void __fastcall OnIdle(TObject *Sender, bool &ADone);
void __fastcall HeartRateChanged(TObject * Sender);
void __fastcall HeartRateClear(TObject * Sender);
void __fastcall HeartRateNextValue(TJSONObject * Value);
void __fastcall BloodPressureChanged(TObject * Sender);
void __fastcall BloodPressureClear(TObject * Sender);
void __fastcall BloodPressureNextValue(TJSONObject * Value);
public:		// User declarations
	__fastcall TEdgeMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TEdgeMainForm *EdgeMainForm;
//---------------------------------------------------------------------------
#endif
