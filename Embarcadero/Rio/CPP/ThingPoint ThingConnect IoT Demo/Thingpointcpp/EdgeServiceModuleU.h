//---------------------------------------------------------------------------

// This software is Copyright (c) 2016 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef EdgeServiceModuleUH
#define EdgeServiceModuleUH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <EMSHosting.EdgeHTTPListener.hpp>
#include <EMSHosting.EdgeService.hpp>
#include <EMSHosting.ExtensionsServices.hpp>
#include <IPPeerClient.hpp>
#include <REST.Backend.EMSProvider.hpp>
#include <REST.Backend.EMSServices.hpp>
#include <REST.Backend.Providers.hpp>
#include <System.JSON.hpp>
//---------------------------------------------------------------------------
typedef void __fastcall (__closure *TModuleOverwriteEvent)(String AModuleName, bool &AResult);
typedef void __fastcall (__closure *TProtocolPropsConflictDeleteEvent)(String AModuleName, String AProtocolProps, bool &AResult);
class TEdgeServiceModule : public TDataModule
{
__published:	// IDE-managed Components
	TEMSEdgeService *EMSEdgeService1;
	TEMSProvider *EMSProvider1;
	void __fastcall EMSEdgeService1Registered(TObject *Sender);
	void __fastcall EMSEdgeService1Registering(TObject *Sender, TJSONObject * const AModuleDetail,
          TJSONArray * const AResources, bool &AHandled);

private:	// User declarations
        TModuleOverwriteEvent FOnModuleOverwrite;
        TProtocolPropsConflictDeleteEvent FOnProtocolPropsConflictDelete;
public:		// User declarations
	__fastcall TEdgeServiceModule(TComponent* Owner);
        __property TModuleOverwriteEvent OnModuleOverwrite = {read=FOnModuleOverwrite, write=FOnModuleOverwrite};
        __property TProtocolPropsConflictDeleteEvent OnProtocolPropsConflictDelete = {read=FOnProtocolPropsConflictDelete, write=FOnProtocolPropsConflictDelete};
//  public type
//    TOnModuleOvewriteCallback = reference to function(const AModuleName: string): Boolean;
//    TOnProtocolPropsConflictCallback = reference to function(const AModuleName, AProtocolProps: string): Boolean;
//  private
//    FOnModuleOverwrite: TOnModuleOvewriteCallback;
//    FOnProtocolPropsConflictDelete: TOnProtocolPropsConflictCallback;
//    { Private declarations }
//  public
//    { Public declarations }
//    property OnModuleOverwrite: TOnModuleOvewriteCallback read FOnModuleOverwrite write FOnModuleOverwrite;
//    property OnProtocolPropsConflictDelete: TOnProtocolPropsConflictCallback read FOnProtocolPropsConflictDelete write FOnProtocolPropsConflictDelete;
//  end;
};
//---------------------------------------------------------------------------
extern PACKAGE TEdgeServiceModule *EdgeServiceModule;
//---------------------------------------------------------------------------
#endif
