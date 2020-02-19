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
#include "EndpointResultFrameU.h"
#include <FMX.Controls.Presentation.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.TabControl.hpp>
#include <FMX.Types.hpp>
#include "ClientSettingsU.h"
#include "ClientDataModuleU.h"
#include <FMX.Grid.hpp>
#include <FMX.Grid.Style.hpp>
#include <FMX.ScrollBox.hpp>
#include <System.Rtti.hpp>
//---------------------------------------------------------------------------
class TEMSThingPointForm : public TForm
{
__published:	// IDE-managed Components
	TLayout *Layout1;
	TEMSServerConnectionFrame *EMSServerConnectionFrame1;
	TLayout *Layout2;
	TTabControl *TabControl1;
	TTabItem *TabItemServerRequest;
	TEMSEndpointResultFrame *MeasurementsResultFrame;
	TTabItem *TabItemEdgeRequest;
	TLayout *Layout3;
	TComboBox *ComboBoxEdgeName;
	TButton *ButtonRefreshEdgeNames;
	TEMSEndpointResultFrame *EdgeResultsFrame;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
	void __fastcall FormDestroy(TObject *Sender);
	void __fastcall MeasurementsResultFrameButtonExecuteClick(TObject *Sender);
	void __fastcall EMSServerConnectionFrame1ButtonTestConnectionClick(TObject *Sender);
	void __fastcall MeasurementsResultFrameButtonClearClick(TObject *Sender);
	void __fastcall ComboBoxEdgeNameChange(TObject *Sender);
	void __fastcall ButtonRefreshEdgeNamesClick(TObject *Sender);
	void __fastcall EdgeResultsFrameButtonExecuteClick(TObject *Sender);
	void __fastcall TabItemEdgeRequestClick(TObject *Sender);

private:	// User declarations
	TSettingsList *FSettingsList;
	void __fastcall OnIdle(TObject * Sender, bool &ADone);
	void __fastcall RefreshEdgeNames(void);
public:		// User declarations
	__fastcall TEMSThingPointForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TEMSThingPointForm *EMSThingPointForm;
//---------------------------------------------------------------------------
#endif
