// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.FontGlyphs.Android.pas' rev: 28.00 (Android)

#ifndef Fmx_Fontglyphs_AndroidHPP
#define Fmx_Fontglyphs_AndroidHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <FMX.FontGlyphs.hpp>	// Pascal unit
#include <Androidapi.JNI.GraphicsContentViewText.hpp>	// Pascal unit
#include <System.IOUtils.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Fontglyphs
{
namespace Android
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TAndroidFontGlyphManager;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TAndroidFontGlyphManager : public Fmx::Fontglyphs::TFontGlyphManager
{
	typedef Fmx::Fontglyphs::TFontGlyphManager inherited;
	
private:
	Androidapi::Jni::Graphicscontentviewtext::_di_JPaint FPaint;
	int FTop;
	int FAscent;
	int FDescent;
	int FBottom;
	int FLeading;
	
protected:
	virtual void __fastcall LoadResource(void);
	virtual void __fastcall FreeResource(void);
	virtual Fmx::Fontglyphs::TFontGlyph* __fastcall DoGetGlyph(const System::UCS4Char Char, const Fmx::Fontglyphs::TFontGlyphSettings Settings);
	virtual float __fastcall DoGetBaseline(void);
	
public:
	__fastcall TAndroidFontGlyphManager(void);
	__fastcall virtual ~TAndroidFontGlyphManager(void);
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Android */
}	/* namespace Fontglyphs */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_FONTGLYPHS_ANDROID)
using namespace Fmx::Fontglyphs::Android;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_FONTGLYPHS)
using namespace Fmx::Fontglyphs;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_Fontglyphs_AndroidHPP
