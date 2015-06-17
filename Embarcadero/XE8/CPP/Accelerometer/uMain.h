//---------------------------------------------------------------------------

#ifndef uMainH
#define uMainH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListBox.hpp>
#include <System.Sensors.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <System.Sensors.hpp>
#include <FMX.MobilePreview.hpp>
#include <System.Sensors.Components.hpp>
//---------------------------------------------------------------------------
class TAccelerometerForm : public TForm
{
__published:	// IDE-managed Components
	TTimer *Timer1;
	TToolBar *ToolBar1;
	TLabel *Label1;
	TListBox *ListBox1;
	TListBoxItem *lbAccelerometerSensor;
	TSwitch *swAccelerometerSensorActive;
	TListBoxItem *lbAccelerationX;
	TListBoxItem *lbAccelerationY;
	TListBoxItem *lbAccelerationZ;
	TListBoxItem *lbAngleAccelX;
	TListBoxItem *lbAngleAccelY;
	TListBoxItem *lbAngleAccelZ;
	TListBoxItem *lbMotion;
	TListBoxItem *lbSpeed;
	TMotionSensor *MotionSensor1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall swAccelerometerSensorActiveSwitch(TObject *Sender);
	void __fastcall Timer1Timer(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TAccelerometerForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TAccelerometerForm *AccelerometerForm;
//---------------------------------------------------------------------------
#endif
