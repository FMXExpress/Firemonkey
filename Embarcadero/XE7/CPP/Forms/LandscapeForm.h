//---------------------------------------------------------------------------

#ifndef LandscapeFormH
#define LandscapeFormH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Objects.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <FMX.MobilePreview.hpp>
//---------------------------------------------------------------------------
class TLSForm : public TForm
{
__published:	// IDE-managed Components
	TToolBar *ToolBar1;
	TLabel *Label2;
	TImage *Image1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormResize(TObject *Sender);
private:	// User declarations
	bool FCreated;
public:		// User declarations
	__fastcall TLSForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TLSForm *LSForm;
//---------------------------------------------------------------------------
#endif
