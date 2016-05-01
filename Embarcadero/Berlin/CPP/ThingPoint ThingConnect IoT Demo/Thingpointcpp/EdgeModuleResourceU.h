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

#ifndef EdgeModuleResourceUH
#define EdgeModuleResourceUH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <EMS.Services.hpp>
#include <EMS.ResourceAPI.hpp>
#include <EMS.ResourceTypes.hpp>
#include "CacheDataModuleU.h"
//---------------------------------------------------------------------------
#pragma explicit_rtti methods (public)
typedef void __fastcall(__closure * TMeasurementsEvent)(String ADevice, TDateTime &ATime, TJSONObject * AData, bool &AResult);
class TMeasurementsResource : public TObject
{
private:
void __fastcall OnGetSimple(String ADevice, TDateTime &ATime, TJSONObject * AData, bool &AResult);
void __fastcall OnGetDetailed(String ADevice, TDateTime &ATime, TJSONObject * AData, bool &AResult);
public:
	__fastcall ~TMeasurementsResource(void) { }
	void GetSimple(TEndpointContext* AContext, TEndpointRequest* ARequest, TEndpointResponse* AResponse);
	void GetDetailed(TEndpointContext* AContext, TEndpointRequest* ARequest, TEndpointResponse* AResponse);
void __fastcall DoGetResponse(TEndpointResponse * AResponse, TMeasurementsEvent ACallback);
};

//static void Register()
//{
//TObject * o = new TObject();
//        //std::auto_ptr<TEMSResourceAttributes> attributes(new TEMSResourceAttributes());
//        TEMSResourceAttributes * attributes = new TEMSResourceAttributes();
//        attributes->ResourceName = "Measurements";
//        attributes->ResourceSuffix["GetDetailed"] = "detailed";
//        //RegisterResource(__typeinfo(TMeasurementsResource), attributes.release());
//        RegisterResource(__typeinfo(TMeasurementsResource), attributes);
//}
#endif


