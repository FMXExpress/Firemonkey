//---------------------------------------------------------------------------

// This software is Copyright (c) 2016 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef ListenerFrameUH
#define ListenerFrameUH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <Data.Bind.Components.hpp>
#include <Data.Bind.ObjectScope.hpp>
#include <FMX.ActnList.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.Edit.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <REST.Backend.EndPoint.hpp>
#include <REST.Backend.ServiceTypes.hpp>
#include <REST.Client.hpp>
#include <System.Actions.hpp>
#include <System.JSON.hpp>
#include <EMSHosting.EdgeService.hpp>
#include <EMSHosting.ExtensionsServices.hpp>
#include <REST.Backend.Providers.hpp>
//---------------------------------------------------------------------------
class TEMSEdgeModuleListenerFrame : public TFrame
{
__published:	// IDE-managed Components
	TLayout *Layout1;
	TGroupBox *GroupBox2;
	TCheckBox *CheckBoxEdgepointActive;
	TEdit *EditListenerHost;
	TLabel *Label1;
	TEdit *EditListenerPort;
	TLabel *Label2;
	TButton *ButtonTestVersion;
	TEdit *EditModuleName;
	TLabel *Label3;
	TButton *ButtonGetLocal;
	TBackendEndpoint *BackendEndpointVersion;
	TActionList *ActionList1;
	TAction *ActionActivateEdgeModule;
	void __fastcall ActionActivateEdgeModuleExecute(TObject *Sender);
	void __fastcall ActionActivateEdgeModuleUpdate(TObject *Sender);
	void __fastcall ButtonGetLocalClick(TObject *Sender);
	void __fastcall ButtonTestVersionClick(TObject *Sender);
private:	// User declarations
	TEMSEdgeService * FEMSEdgeService;
	TEMSProvider * FEMSProvider;
        void __fastcall SetEMSEdgeService(TEMSEdgeService * Value);
        TEMSProvider * __fastcall GetEMSProvider(void);
        void __fastcall SetEMSProvider(TEMSProvider * Value);
        void __fastcall EdgeServiceChanged(void);
        void __fastcall UpdateEdgeService(void);
protected:
	virtual void __fastcall Notification(TComponent * AComponent, TOperation opeOperationration);
public:		// User declarations
	__fastcall TEMSEdgeModuleListenerFrame(TComponent* Owner);
        __property TEMSEdgeService * EMSEdgeService = {read=FEMSEdgeService, write=SetEMSEdgeService};
        __property TEMSProvider * EMSProvider = {read=FEMSProvider, write=SetEMSProvider};
};

//  private
//    FEMSEdgeService: TEMSEdgeService;
//    procedure SetEMSEdgeService(const Value: TEMSEdgeService);
//    function GetEMSProvider: TEMSProvider;
//    procedure EdgeServiceChanged;
//    procedure UpdateEdgeService;
//    //procedure CheckEdgeService;
//    { Private declarations }
//    property EMSProvider: TEMSProvider read GetEMSProvider;
//  protected
//    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
//
//  public
//    { Public declarations }
//    property EMSEdgeService: TEMSEdgeService read FEMSEdgeService write SetEMSEdgeService;
//  end;

//---------------------------------------------------------------------------
extern PACKAGE TEMSEdgeModuleListenerFrame *EMSEdgeModuleListenerFrame;
//---------------------------------------------------------------------------
#endif
