//---------------------------------------------------------------------------

// This software is Copyright (c) 2016 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef EndpointResultFrameUH
#define EndpointResultFrameUH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <System.Sysutils.hpp>
#include <System.StrUtils.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <Data.Bind.Components.hpp>
#include <Data.Bind.DBScope.hpp>
#include <Data.Bind.EngExt.hpp>
#include <Data.Bind.Grid.hpp>
#include <Data.DB.hpp>
#include <FireDAC.Comp.Client.hpp>
#include <FireDAC.Comp.DataSet.hpp>
#include <FireDAC.DApt.Intf.hpp>
#include <FireDAC.DatS.hpp>
#include <FireDAC.Phys.Intf.hpp>
#include <FireDAC.Stan.Error.hpp>
#include <FireDAC.Stan.Intf.hpp>
#include <FireDAC.Stan.Option.hpp>
#include <FireDAC.Stan.Param.hpp>
#include <FMX.ActnList.hpp>
#include <Fmx.Bind.DBEngExt.hpp>
#include <Fmx.Bind.Editors.hpp>
#include <Fmx.Bind.Grid.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.Grid.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <REST.Response.Adapter.hpp>
#include <System.Actions.hpp>
#include <System.Bindings.Outputs.hpp>
#include <System.Rtti.hpp>
#include <REST.Backend.EndPoint.hpp>
#include "ClientSettingsU.h"
//---------------------------------------------------------------------------
class TEMSEndpointResultFrame : public TFrame
{
__published:	// IDE-managed Components
	TLayout *Layout1;
	TLayout *Layout2;
	TButton *ButtonExecute;
	TButton *ButtonClear;
	TStringGrid *StringGrid1;
	TLayout *Layout3;
	TCheckBox *CheckBoxAutoRefresh;
	TTimer *Timer1;
	TFDMemTable *FDMemTable1;
	TBindSourceDB *BindSourceDB1;
	TBindingsList *BindingsList1;
	TLinkGridToDataSource *LinkGridToDataSourceBindSourceDB1;
	TRESTResponseDataSetAdapter *RESTResponseDataSetAdapter1;
	TActionList *ActionList1;
	TAction *ActionAutoRefresh;
	void __fastcall ButtonExecuteClick(TObject *Sender);
	void __fastcall ButtonClearClick(TObject *Sender);
	void __fastcall CheckBoxAutoRefreshClick(TObject *Sender);
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall ActionAutoRefreshExecute(TObject *Sender);
	void __fastcall ActionAutoRefreshUpdate(TObject *Sender);
protected:
	void __fastcall Notification(TComponent * AComponent, TOperation Operation);
private:	// User declarations
	TSettingsList *FSettingsList;
	TBackendEndpoint * FBackendEndpoint;
	void __fastcall SetBackendEndpoint(const TBackendEndpoint * value);
	void __fastcall SetSettingsList(const TSettingsList * value);
	bool __fastcall GetEmpty(void);
	bool __fastcall GetAutoRefresh(void);
	void __fastcall SetAutoRefresh(const bool value);
	void __fastcall CheckBackendEndpoint(void);
	void __fastcall BackendEndpointChanged(void);
    String __fastcall MakeWidthKey(const String AHeader);
	void __fastcall RestoreGridColumnWidths(void);
	void __fastcall SaveGridColumnWidths(void);
public:		// User declarations
	__fastcall TEMSEndpointResultFrame(TComponent* Owner);
public:
	void __fastcall Execute(void);
	void __fastcall Clear(void);
	void __fastcall SaveSettings(void);
	__property TBackendEndpoint * BackendEndpoint ={read=FBackendEndpoint, write=SetBackendEndpoint};
	__property TSettingsList * SettingsList = {read=FSettingsList, write=SetSettingsList};
	__property bool AutoRefresh = {read=GetAutoRefresh, write=SetAutoRefresh};
	__property bool Empty = {read=GetEmpty};
};
//---------------------------------------------------------------------------
extern PACKAGE TEMSEndpointResultFrame *EMSEndpointResultFrame;
//---------------------------------------------------------------------------
#endif
