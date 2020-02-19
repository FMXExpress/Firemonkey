//---------------------------------------------------------------------------

// This software is Copyright (c) 2016 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef StreamingDeviceFrameUH
#define StreamingDeviceFrameUH
// ---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.ActnList.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.Memo.hpp>
#include <FMX.ScrollBox.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <System.Actions.hpp>
#include <System.JSON.hpp>
#include "Iot.Device.GenericHeartRateMonitor.hpp"
#include <Iot.Family.BluetoothLE.hpp>
#include <FMX.Edit.hpp>
// ---------------------------------------------------------------------------
typedef void __fastcall(__closure * TNextValueEvent)(TJSONObject * Value);

class TStreamingDeviceFrame : public TFrame {
__published: // IDE-managed Components

    TLayout *Layout3;
    TLayout *Layout2;
    TMemo *Memo1;
    TLayout *Layout4;
    TButton *ButtonClear;
    TLayout *Layout1;
    TButton *ButtonStartStop;
    TActionList *ActionList1;
    TAction *ActionStartStop;
	TBluetoothDeviceDiscoveryManager *BLEManager;
	TGenericHeartRateMonitor *GenericHRM;
	TButton *ButtonConnect;
	TEdit *edDeviceName;

    void __fastcall ActionStartStopExecute(TObject * Sender);
    void __fastcall ActionStartStopUpdate(TObject * Sender);
    void __fastcall ButtonClearClick(TObject * Sender);
	void __fastcall GenericHRMDeviceDisconnect(TCustomGeneratedBluetoothLEComponent * const Sender);
	void __fastcall GenericHRMHeartRateMeasurementUpdate(TObject *Sender, const TGattHeartRateMeasurement &Value);
	void __fastcall ButtonConnectClick(TObject *Sender);
	void __fastcall GenericHRMDeviceConnected();
	void __fastcall BLEManagerGeneralDiscoveryError(TObject * const Sender, Exception * const AException,
		  bool &Handled);




private: // User declarations

    TJSONObject * FValue;
    TDateTime FTime;
    TNotifyEvent FOnChanged;
	TNotifyEvent FOnClear;
	TNextValueEvent FOnNextValue;
	System::Boolean FStarted;

    bool __fastcall GetStarted(void);
    void __fastcall SetStarted(bool Value);
    void __fastcall DoClear(void);
    void __fastcall DoChanged(void);
    void __fastcall DoNextValue(void);

public: // User declarations
    __fastcall TStreamingDeviceFrame(TComponent* Owner);
	__fastcall ~TStreamingDeviceFrame(void);
    void __fastcall ClearLog(void);
    void __fastcall Start(void);
    void __fastcall Stop(void);
    void __fastcall LogValue(void);
    __property TJSONObject * Value = {read = FValue};
	__property TDateTime Time = {read = FTime};
    __property bool Started = {read = GetStarted};
    __property TNotifyEvent OnChanged = {read = FOnChanged, write = FOnChanged};
    __property TNotifyEvent OnClear = {read = FOnClear, write = FOnClear};
    __property TNextValueEvent OnNextValue = {
	read = FOnNextValue, write = FOnNextValue};
};

// ---------------------------------------------------------------------------
extern PACKAGE TStreamingDeviceFrame *StreamingDeviceFrame;
// ---------------------------------------------------------------------------
#endif
