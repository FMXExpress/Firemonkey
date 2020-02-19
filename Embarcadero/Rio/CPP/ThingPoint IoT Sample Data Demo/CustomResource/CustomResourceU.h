//---------------------------------------------------------------------------

// This software is Copyright (c) 2016 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
// EMS Resource Modules
//---------------------------------------------------------------------------

#ifndef CustomResourceUH
#define CustomResourceUH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <EMS.Services.hpp>
#include <EMS.ResourceAPI.hpp>
#include <EMS.ResourceTypes.hpp>
#include <System.JSON.hpp>
#include <vector>
//---------------------------------------------------------------------------
#pragma explicit_rtti methods (public)
class TMeasurementsResource : public TDataModule
{
__published:
private:
	void __fastcall GetModuleNames(TEndpointContext * AContext, std::vector<String> & names);
public:
	__fastcall TMeasurementsResource(TComponent* Owner);
	void Get(TEndpointContext* AContext, TEndpointRequest* ARequest, TEndpointResponse* AResponse);
};
#endif


