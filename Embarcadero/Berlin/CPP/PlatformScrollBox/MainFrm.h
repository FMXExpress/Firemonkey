//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.
//---------------------------------------------------------------------------

#ifndef MainFrmH
#define MainFrmH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.Edit.hpp>
#include <FMX.EditBox.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.MultiView.hpp>
#include <FMX.ScrollBox.hpp>
#include <FMX.SpinBox.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <FMX.Calendar.hpp>
#include <FMX.NumberBox.hpp>
#include <FMX.ComboEdit.hpp>
//---------------------------------------------------------------------------

typedef System::TMetaClass* TControlClass;

const int DemoControlsCount = 16;

TControlClass DemoControlsClasses[] = {__classid(TButton), __classid(TTrackBar), __classid(TPanel),
		__classid(TAniIndicator), __classid(TArcDial), __classid(TProgressBar), __classid(TLabel),
		__classid(TRadioButton), __classid(TCheckBox), __classid(TSwitch), __classid(TEdit),
		__classid(TMemo), __classid(TCalendar), __classid(TNumberBox), __classid(TSpinBox),
		__classid(TComboEdit)};

class TFormMain : public TForm
{
__published:	// IDE-managed Components
	TLayout *Layout1;
	TPresentedScrollBox *PresentedScrollBox1;
	TMultiView *MultiView1;
	TListBox *ListBox1;
	TListBoxGroupHeader *ListBoxGroupHeader4;
	TListBoxItem *ListBoxItem26;
	TSwitch *swControlType;
	TListBoxGroupHeader *ListBoxGroupHeader1;
	TListBoxItem *ListBoxItem30;
	TSwitch *swScrollEnabled;
	TListBoxItem *ListBoxItem31;
	TSwitch *swDisableMouseWheel;
	TListBoxItem *ListBoxItem1;
	TComboBox *cbScrollAnimation;
	TListBoxItem *ListBoxItem9;
	TListBoxItem *ListBoxItem10;
	TListBoxItem *ListBoxItem11;
	TListBoxItem *ListBoxItem2;
	TComboBox *cbBounces;
	TListBoxItem *ListBoxItem6;
	TListBoxItem *ListBoxItem7;
	TListBoxItem *ListBoxItem8;
	TListBoxItem *ListBoxItem3;
	TComboBox *cbScrollDirection;
	TListBoxItem *ListBoxItem4;
	TListBoxItem *ListBoxItem5;
	TListBoxItem *ListBoxItem12;
	TListBoxItem *ListBoxItem13;
	TComboBox *cbTouchTracking;
	TListBoxItem *ListBoxItem14;
	TListBoxItem *ListBoxItem15;
	TListBoxItem *ListBoxItem16;
	TListBoxGroupHeader *ListBoxGroupHeader2;
	TListBoxItem *ListBoxItem17;
	TComboBox *cbAutoHide;
	TListBoxItem *ListBoxItem18;
	TListBoxItem *ListBoxItem19;
	TListBoxItem *ListBoxItem20;
	TListBoxItem *ListBoxItem21;
	TSwitch *swShowScrollBars;
	TListBoxItem *ListBoxItem22;
	TSwitch *swShowSizeGrip;
	TListBoxGroupHeader *ListBoxGroupHeader3;
	TListBoxItem *ListBoxItem23;
	TSwitch *swAutoSize;
	TListBoxItem *ListBoxItem24;
	TSpinBox *spContentWidth;
	TListBoxItem *ListBoxItem25;
	TSpinBox *spContentHeight;
	TListBoxGroupHeader *ListBoxGroupHeader5;
	TListBoxItem *ListBoxItem27;
	TButton *Button4;
	TListBoxItem *ListBoxItem28;
	TButton *Button5;
	TListBoxItem *ListBoxItem29;
	TButton *Button6;
	TListBoxItem *ListBoxItem36;
	TButton *Button2;
	TListBoxGroupHeader *ListBoxGroupHeader6;
	TListBoxItem *ListBoxItem32;
	TSpinBox *spViewPortPositionX;
	TListBoxItem *ListBoxItem33;
	TSpinBox *spViewPortPositionY;
	TListBoxItem *ListBoxItem34;
	TSpinBox *spViewPortWidth;
	TListBoxItem *ListBoxItem35;
	TSpinBox *spViewPortHeight;
	void __fastcall FormShow(TObject *Sender);
	void __fastcall swControlTypeSwitch(TObject *Sender);
	void __fastcall swScrollEnabledClick(TObject *Sender);
	void __fastcall swDisableMouseWheelClick(TObject *Sender);
	void __fastcall cbScrollAnimationChange(TObject *Sender);
	void __fastcall cbBouncesChange(TObject *Sender);
	void __fastcall cbScrollDirectionChange(TObject *Sender);
	void __fastcall cbTouchTrackingChange(TObject *Sender);
	void __fastcall cbAutoHideChange(TObject *Sender);
	void __fastcall swShowScrollBarsSwitch(TObject *Sender);
	void __fastcall swShowSizeGripSwitch(TObject *Sender);
	void __fastcall swAutoSizeSwitch(TObject *Sender);
	void __fastcall spContentWidthChange(TObject *Sender);
	void __fastcall spContentHeightChange(TObject *Sender);
	void __fastcall Button4Click(TObject *Sender);
	void __fastcall Button5Click(TObject *Sender);
	void __fastcall Button6Click(TObject *Sender);
	void __fastcall Button2Click(TObject *Sender);
	void __fastcall spViewPortPositionXChange(TObject *Sender);
	void __fastcall spViewPortPositionYChange(TObject *Sender);
private:	// User declarations
	void __fastcall FillDemoControls();
	void __fastcall UpdateViewportSize(void);
	void __fastcall UpdateViewportPositionWithoutOnChange(void);
	void __fastcall SetChildrenControlType(TFmxObject * const ARoot, TControlType const AControlType);
public:		// User declarations
	__fastcall TFormMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormMain *FormMain;
//---------------------------------------------------------------------------
#endif
