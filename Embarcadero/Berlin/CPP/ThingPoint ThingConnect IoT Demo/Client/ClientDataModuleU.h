//---------------------------------------------------------------------------

// This software is Copyright (c) 2016 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef ClientDataModuleUH
#define ClientDataModuleUH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Data.Bind.Components.hpp>
#include <Data.Bind.ObjectScope.hpp>
#include <IPPeerClient.hpp>
#include <REST.Backend.BindSource.hpp>
#include <REST.Backend.EMSProvider.hpp>
#include <REST.Backend.EMSServices.hpp>
#include <REST.Backend.EndPoint.hpp>
#include <REST.Backend.MetaTypes.hpp>
#include <REST.Backend.ServiceComponents.hpp>
#include <REST.Backend.ServiceTypes.hpp>
#include <REST.Client.hpp>
#include <System.JSON.hpp>
#include <vector>
//---------------------------------------------------------------------------
class TEMSClientDataModule : public TDataModule
{
__published:	// IDE-managed Components
	TEMSProvider *EMSProvider1;
	TBackendEndpoint *BackendEndpointMeasurements;
	TBackendEndpoint *BackendEndpointEdgeMeasurements;
	TBackendQuery *BackendQueryEdgeModules;
	TBackendEndpoint *BackenEndpointEdgeDetailedMeasurements;
private:	// User declarations
	String __fastcall GetCurrentEdge(void);
	void __fastcall SetCurrentEdge(const String Value);
public:		// User declarations
	__fastcall TEMSClientDataModule(TComponent* Owner);
public:
	void __fastcall GetEdgeNames(std::vector<String> & value);
	__property String CurrentEdge = {read=GetCurrentEdge, write=SetCurrentEdge};
};
//---------------------------------------------------------------------------
extern PACKAGE TEMSClientDataModule *EMSClientDataModule;
//---------------------------------------------------------------------------
#endif
