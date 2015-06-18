// CodeGear C++Builder
// Copyright (c) 1995, 2013 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'HTMLHelpFMXViewer.pas' rev: 25.00 (Windows)

#ifndef HtmlhelpfmxviewerHPP
#define HtmlhelpfmxviewerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Types.hpp>	// Pascal unit
#include <FMX.Controls.hpp>	// Pascal unit
#include <FMX.Forms.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Htmlhelpfmxviewer
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall RegisterFormForHelp(Fmx::Forms::TCommonCustomForm* AForm);
extern DELPHI_PACKAGE bool __fastcall SetHTMLHelpFile(System::UnicodeString PathAndFilename);
extern DELPHI_PACKAGE void __fastcall ShowHTMLHelp(void);
extern DELPHI_PACKAGE void __fastcall ShowFocusedFormHTMLHelp(Fmx::Forms::TCommonCustomForm* AForm);
extern DELPHI_PACKAGE void __fastcall ShowControlHTMLHelp(Fmx::Controls::TStyledControl* aControl);
extern DELPHI_PACKAGE void __fastcall ShowHTMLHelpContents(void);
extern DELPHI_PACKAGE void __fastcall ShowPopupHTMLHelp(void);
extern DELPHI_PACKAGE void __fastcall ShowFocusedFormPopupHTMLHelp(Fmx::Forms::TCommonCustomForm* AForm);
extern DELPHI_PACKAGE void __fastcall ShowControlPopupHTMLHelp(Fmx::Controls::TStyledControl* aControl);
}	/* namespace Htmlhelpfmxviewer */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_HTMLHELPFMXVIEWER)
using namespace Htmlhelpfmxviewer;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// HtmlhelpfmxviewerHPP
