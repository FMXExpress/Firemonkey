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
#include <FMX.ActnList.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.ListView.Adapters.Base.hpp>
#include <FMX.ListView.Appearances.hpp>
#include <FMX.ListView.hpp>
#include <FMX.ListView.Types.hpp>
#include <FMX.MultiView.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.TabControl.hpp>
#include <FMX.Types.hpp>
#include <System.Actions.hpp>
#include <System.JSON.hpp>
#include "ClientDataModuleU.h"
#include <memory>
//---------------------------------------------------------------------------
class TMainForm : public TForm
{
__published:	// IDE-managed Components
	TPanel *PanelDetail;
	TToolBar *ToolBar1;
	TLabel *Label2;
	TSpeedButton *MasterButton;
	TButton *ButtonBack;
	TButton *ButtonForward;
	TLabel *Label1;
	TTabControl *TabControl1;
	TTabItem *TabItemSettings;
	TEMSServerConnectionFrame *EMSServerConnectionFrame1;
	TTabItem *TabItemThings;
	TListView *ListView1;
	TToolBar *ToolBar2;
	TButton *ButtonRefreshThing;
	TLayout *Layout1;
	TSwitch *SwitchDetailed;
	TLabel *Label3;
	TLayout *Layout2;
	TSwitch *SwitchAutoRefresh;
	TLabel *Label4;
	TTimer *Timer1;
	TMultiView *MultiView1;
	TListBox *ListBox1;
	TButton *ButtonRefreshNames;
	TActionList *ActionList1;
	TAction *ActionBack;
	TAction *ActionForward;
	TAction *ActionCaption;
	TAction *ActionAutoRefresh;
	TAction *ActionDetailed;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall ListBox1Change(TObject *Sender);
	void __fastcall ListBox1ItemClick(TCustomListBox * const Sender, TListBoxItem * const Item);
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall SwitchDetailedSwitch(TObject *Sender);
    void __fastcall SetAutoRefresh(bool AValue);
	void __fastcall ActionBackExecute(TObject *Sender);
	void __fastcall ActionBackUpdate(TObject *Sender);
	void __fastcall ActionForwardUpdate(TObject *Sender);
	void __fastcall ActionForwardExecute(TObject *Sender);
	void __fastcall ActionCaptionUpdate(TObject *Sender);
	void __fastcall ActionAutoRefreshExecute(TObject *Sender);
	void __fastcall ActionAutoRefreshUpdate(TObject *Sender);
	void __fastcall ActionDetailedExecute(TObject *Sender);
	void __fastcall ActionDetailedUpdate(TObject *Sender);
private:	// User declarations
	void __fastcall ShowSettings(void);
	void __fastcall ShowThings(void);
	void __fastcall UpdateMultiview(void);
	void __fastcall ListThings(void);
	void __fastcall ListObject(const String APrefix, const TJSONObject * AJSONObject);
	void __fastcall RefreshThing(void);
    TJSONArray * __fastcall ExecuteEndpoint(void);
	bool __fastcall ShowingAll(void);
public:		// User declarations
	__fastcall TMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TMainForm *MainForm;
//---------------------------------------------------------------------------
#endif
