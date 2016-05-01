//---------------------------------------------------------------------------

// This software is Copyright (c) 2016 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef LoggingFrameUH
#define LoggingFrameUH
// ---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.Memo.hpp>
#include <FMX.ScrollBox.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>

// ---------------------------------------------------------------------------
class TEMSEdgeLoggingFrame : public TFrame {
__published: // IDE-managed Components

    TMemo *Memo1;
    TLayout *Layout1;
    TButton *ButtonClear;
    TCheckBox *CheckBoxLogging;

    void __fastcall ButtonClearClick(TObject *Sender);

private: // User declarations
public: // User declarations
	__fastcall TEMSEdgeLoggingFrame(TComponent* Owner);
    void __fastcall LogItem(String ACategory, TJSONObject * AJSON);
};

// ---------------------------------------------------------------------------
extern PACKAGE TEMSEdgeLoggingFrame *EMSEdgeLoggingFrame;
// ---------------------------------------------------------------------------
#endif
