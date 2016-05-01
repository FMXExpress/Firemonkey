// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DelphiIntf.pas' rev: 31.00 (Android)

#ifndef DelphiintfHPP
#define DelphiintfHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w       // Display all warnings
#pragma option -w-inl   // Functions %s are not expanded inline
#pragma option -w-8111  // Accessing deprecated entity
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <FMX.Types.hpp>
#include <System.Classes.hpp>
#include <FMX.Graphics.hpp>
#include <System.Rtti.hpp>

//-- user supplied -----------------------------------------------------------

namespace Delphiintf
{
//-- forward type declarations -----------------------------------------------
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
