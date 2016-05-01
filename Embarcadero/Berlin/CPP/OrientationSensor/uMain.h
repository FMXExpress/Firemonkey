//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

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
#include <FMX.MobilePreview.hpp>
#include <System.Sensors.Components.hpp>
//---------------------------------------------------------------------------
class TOrientationSensorForm : public TForm
{
__published:	// IDE-managed Components
	TOrientationSensor *OrientationSensor1;
	TToolBar *ToolBar1;
	TLabel *Label1;
	TListBox *ListBox1;
	TListBoxItem *lbOrientationSensor;
	TSwitch *swOrientationSensorActive;
	TListBoxItem *lbTiltX;
	TListBoxItem *lbTiltY;
	TListBoxItem *lbTiltZ;
	TListBoxItem *lbHeadingX;
	TListBoxItem *lbHeadingY;
	TListBoxItem *lbHeadingZ;
	TLayout *Layout1;
	TSpeedButton *TiltButton;
	TSpeedButton *HeadingButton;
	TTimer *Timer1;
	void __fastcall FormActivate(TObject *Sender);
	void __fastcall HeadingButtonClick(TObject *Sender);
	void __fastcall TiltButtonClick(TObject *Sender);
	void __fastcall OrientationSensor1SensorChoosing(TObject *Sender, const TSensorArray Sensors,
          int &ChoseSensorIndex);
	void __fastcall swOrientationSensorActiveSwitch(TObject *Sender);
	void __fastcall Timer1Timer(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TOrientationSensorForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TOrientationSensorForm *OrientationSensorForm;
//---------------------------------------------------------------------------
#endif
