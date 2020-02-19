//---------------------------------------------------------------------------

// This software is Copyright (c) 2016 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef NotifyDeviceFrameUH
#define NotifyDeviceFrameUH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.Memo.hpp>
#include <FMX.ScrollBox.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <System.JSON.hpp>
//---------------------------------------------------------------------------
typedef void __fastcall(__closure * TNextValueEvent)(TJSONObject * Value);
class TNotifyDeviceFrame : public TFrame
{
__published:	// IDE-managed Components
	TLayout *Layout3;
	TLayout *Layout2;
	TMemo *Memo1;
	TLayout *Layout4;
	TButton *ButtonClear;
	TLayout *Layout1;
	TLabel *LabelDevice;
	TButton *ButtonNotify;
    void __fastcall ActionStartStopExecute(TObject * Sender);
    void __fastcall ButtonClearClick(TObject * Sender);
private:	// User declarations
    TJSONObject * FValue;
    TDateTime FTime;
    TNotifyEvent FOnChanged;
    TNotifyEvent FOnClear;
    TNextValueEvent FOnNextValue;

    void __fastcall DoClear(void);
    void __fastcall DoChanged(void);
    void __fastcall DoNextValue(void);
public:		// User declarations
	__fastcall TNotifyDeviceFrame(TComponent* Owner);
	__fastcall ~TNotifyDeviceFrame(void);
    void __fastcall ClearLog(void);
    void __fastcall LogValue(void);
    void __fastcall NextValue(void);
    __property TJSONObject * Value = {read = FValue};
    __property TDateTime Time = {read = FTime};
    __property TNotifyEvent OnChanged = {read = FOnChanged, write = FOnChanged};
    __property TNotifyEvent OnClear = {read = FOnClear, write = FOnClear};
    __property TNextValueEvent OnNextValue = {
	read = FOnNextValue, write = FOnNextValue};
};
//---------------------------------------------------------------------------
extern PACKAGE TNotifyDeviceFrame *NotifyDeviceFrame;
//---------------------------------------------------------------------------
#endif
