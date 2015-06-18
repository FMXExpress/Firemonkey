//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop
#include <htmlhelp.h>

#include "Unit2.h"
#include "HTMLHelpFMXViewer.hpp"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TForm2 *Form2;
//---------------------------------------------------------------------------
__fastcall TForm2::TForm2(TComponent* Owner)
	: TForm(Owner)
{
 String HelpFile = IncludeTrailingBackslash(ExtractFilePath(GetModuleName(0)))+L"Help.chm";
 if (!Htmlhelpfmxviewer::SetHTMLHelpFile(HelpFile))
 {
   ShowMessage(L"Help file could not be found! at " + HelpFile);
 }
 Htmlhelpfmxviewer::RegisterFormForHelp(this);

}
//---------------------------------------------------------------------------

void __fastcall TForm2::Button1Click(TObject *Sender)
{
Htmlhelpfmxviewer::ShowHTMLHelpContents();
}
//---------------------------------------------------------------------------


