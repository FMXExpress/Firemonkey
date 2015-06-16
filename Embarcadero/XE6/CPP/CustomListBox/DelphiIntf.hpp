// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DelphiIntf.pas' rev: 27.00 (MacOS)

#ifndef DelphiintfHPP
#define DelphiintfHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <FMX.Types.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <FMX.Graphics.hpp>	// Pascal unit
#include <System.Rtti.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Delphiintf
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE Fmx::Graphics::TBitmapCodecManager* __fastcall GetFmxCodecInstance(void);
extern DELPHI_PACKAGE System::Rtti::TValue __fastcall NotifyEventAsTValue(System::Classes::TNotifyEvent Ev);
}	/* namespace Delphiintf */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DELPHIINTF)
using namespace Delphiintf;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DelphiintfHPP
