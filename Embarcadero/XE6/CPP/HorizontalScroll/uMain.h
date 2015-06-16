//---------------------------------------------------------------------------

#ifndef uMainH
#define uMainH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.Objects.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <FMX.MobilePreview.hpp>
//---------------------------------------------------------------------------
class THorizontalScrollForm : public TForm
{
__published:	// IDE-managed Components
	THorzScrollBox *HorzScrollBox1;
	TImage *Image2;
	TImage *Image1;
	TImage *Image3;
	TImage *Image4;
	TToolBar *ToolBar1;
	TLabel *Label1;
private:	// User declarations
public:		// User declarations
	__fastcall THorizontalScrollForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE THorizontalScrollForm *HorizontalScrollForm;
//---------------------------------------------------------------------------
#endif
