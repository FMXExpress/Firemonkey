//---------------------------------------------------------------------------

#ifndef TabbedMapH
#define TabbedMapH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.Maps.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.TabControl.hpp>
#include <FMX.Types.hpp>
//---------------------------------------------------------------------------
class TForm2 : public TForm
{
__published:	// IDE-managed Components
	TTabControl *TabControl1;
	TTabItem *TabItem1;
	TMapView *MapPiter;
	TTabItem *TabItem2;
	TMapView *MapFrisco;
	TLabel *CameraInfo;
	TButton *ZoomOut;
	TButton *ZoomIn;
	void __fastcall FormShow(TObject *Sender);
	void __fastcall ZoomOutClick(TObject *Sender);
	void __fastcall ZoomInClick(TObject *Sender);
	void __fastcall CameraChanged(TObject * Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TForm2(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm2 *Form2;
//---------------------------------------------------------------------------
#endif
