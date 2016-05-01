//---------------------------------------------------------------------------

#ifndef uMainH
#define uMainH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Forms3D.hpp>
#include <FMX.Controls3D.hpp>
#include <FMX.Layers3D.hpp>
#include <FMX.MaterialSources.hpp>
#include <FMX.Objects3D.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <System.Sensors.hpp>
#include <FMX.MobilePreview.hpp>
#include <FMX.Types3D.hpp>
#include <System.Math.Vectors.hpp>
//---------------------------------------------------------------------------
class TForm3D2 : public TForm3D
{
__published:	// IDE-managed Components
	TRectangle3D *Rectangle3D1;
	TTimer *Timer1;
	TLight *Light1;
	TLayer3D *Layer3D1;
	TLabel *Label1;
	TLightMaterialSource *LightMaterialSource1;
	void __fastcall Form3DCreate(TObject *Sender);
	void __fastcall Timer1Timer(TObject *Sender);
private:	// User declarations
    TCustomOrientationSensor * FSensor;
	TSensorArray FSensors;
	TSensorManager * FSensorManager;
public:		// User declarations
	__fastcall TForm3D2(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm3D2 *Form3D2;
//---------------------------------------------------------------------------
#endif
