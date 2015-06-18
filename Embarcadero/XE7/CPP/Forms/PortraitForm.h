//---------------------------------------------------------------------------

#ifndef PortraitFormH
#define PortraitFormH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Objects.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
//---------------------------------------------------------------------------
class TPForm : public TForm
{
__published:	// IDE-managed Components
	TToolBar *ToolBar1;
	TLabel *Label2;
	TImage *Image1;
	void __fastcall FormResize(TObject *Sender);
	void __fastcall FormKeyUp(TObject *Sender, WORD &Key, System::WideChar &KeyChar,
          TShiftState Shift);
private:	// User declarations
	bool FCreated;
public:		// User declarations
	__fastcall TPForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TPForm *PForm;
//---------------------------------------------------------------------------
#endif
