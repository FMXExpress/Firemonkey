//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef ctrlsdemofrmH
#define ctrlsdemofrmH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Dialogs.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.Menus.hpp>
#include <FMX.TabControl.hpp>
#include <FMX.Types.hpp>
#include <FMX.Edit.hpp>
#include <FMX.ListBox.hpp>
  #include <FMX.Objects.hpp>
  #include <FMX.ExtCtrls.hpp>
  #include <FMX.TreeView.hpp>
  #include <FMX.DateTimeCtrls.hpp>
  #include <FMX.Effects.hpp>
  #include <FMX.Memo.hpp>
  #include <FMX.Colors.hpp>
  #include <FMX.Layers3D.hpp>
  #include <FMX.Types3D.hpp>
#include "aboutboxfrm.h"
#include <FMX.StdCtrls.hpp>
#include <FMX.Calendar.hpp>
#include <FMX.ComboEdit.hpp>
#include <FMX.ComboTrackBar.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.EditBox.hpp>
#include <FMX.NumberBox.hpp>
#include <FMX.SpinBox.hpp>
#include <Data.Bind.Components.hpp>
#include <Data.Bind.EngExt.hpp>
#include <Fmx.Bind.DBEngExt.hpp>
#include <Fmx.Bind.Editors.hpp>
#include <System.Bindings.Outputs.hpp>
#include <System.Rtti.hpp>
#include <FMX.ScrollBox.hpp>
//---------------------------------------------------------------------------
class TfrmCtrlsDemo : public TForm
{
__published:	// IDE-managed Components
	TMenuBar *MenuBar1;
	TMenuItem *MenuItem1;
	TMenuItem *MenuItem2;
	TMenuItem *MenuItem3;
	TMenuItem *MenuItem4;
	TMenuItem *MenuItem5;
	TMenuItem *MenuItem6;
	TOpenDialog *OpenDialog1;
	TStatusBar *StatusBar1;
	TButton *Button1;
	TTrackBar *ScaleTrack;
	TLabel *Text1;
	TLabel *Label20;
	TLabel *TextScale;
	TLayout *ControlRoot;
	TTabControl *TabControl1;
	TTabItem *TabItem1;
	TLabel *Text2;
	TLabel *Text3;
	TEdit *TextBox1;
	TScrollBar *ScrollBar1;
	TScrollBar *ScrollBar2;
	TCheckBox *CheckBox1;
	TRadioButton *RadioButton1;
	TRadioButton *RadioButton2;
	TProgressBar *ProgressBar1;
	TAniIndicator *AniIndicator1;
	TLabel *Label1;
	TListBox *StringListBox1;
	TComboBox *StringComboBox1;
	TButton *Button4;
	TPanel *Panel1;
	TSpeedButton *SpeedButton2;
	TSpeedButton *SpeedButton3;
	TLabel *Label19;
	TSmallScrollBar *SmallScrollBar1;
	TCheckBox *CheckBox2;
	TCalloutPanel *CalloutPanel1;
	TRadioButton *calloutBottom;
	TRadioButton *calloutRight;
	TLabel *Label29;
	TRadioButton *calloutTop;
	TRadioButton *calloutLeft;
	TTrackBar *TrackBar6;
	TTabItem *TabItem4;
	TExpander *Expander1;
	TLabel *Label8;
	TButton *Button3;
	TGroupBox *GroupBox1;
	TButton *Button6;
	TGlowEffect *GlowEffect1;
	TLabel *Label13;
	TButton *Button5;
	TLabel *Label9;
	TImage *Image3;
	TLabel *Label10;
	TNumberBox *NumberBox1;
	TGlowEffect *GlowEffect2;
	TTrackBar *TrackBar1;
	TLabel *Label12;
	TLabel *Label14;
	TMemo *Memo1;
	TArcDial *AngleButton1;
	TLabel *Label16;
	TArcDial *AngleButton2;
	TArcDial *AngleButton3;
	TLabel *Label17;
	TPopupBox *PopupBox1;
	TEdit *TextBox3;
	TCalendar *Calendar1;
	TTabItem *TabItem5;
	TDropTarget *DropTarget1;
	TTabItem *TabItem3;
	TListBox *ListBox1;
	TListBoxItem *ListBoxItem1;
	TListBoxItem *ListBoxItem2;
	TImage *Image1;
	TListBoxItem *ListBoxItem3;
	TPath *Path1;
	TListBoxItem *ListBoxItem4;
	TEdit *TextBox2;
	TListBoxItem *ListBoxItem6;
	TRectangle *Rectangle1;
	TLabel *Text6;
	TEllipse *Ellipse1;
	TTreeView *TreeView1;
	TTreeViewItem *TreeViewItem1;
	TTreeViewItem *TreeViewItem2;
	TTreeViewItem *TreeViewItem15;
	TTreeViewItem *TreeViewItem16;
	TTreeViewItem *TreeViewItem20;
	TTreeViewItem *TreeViewItem21;
	TTreeViewItem *TreeViewItem22;
	TTreeViewItem *TreeViewItem23;
	TTreeViewItem *TreeViewItem24;
	TTreeViewItem *TreeViewItem17;
	TTreeViewItem *TreeViewItem18;
	TTreeViewItem *TreeViewItem25;
	TTreeViewItem *TreeViewItem26;
	TTreeViewItem *TreeViewItem19;
	TTreeViewItem *TreeViewItem3;
	TTreeViewItem *TreeViewItem4;
	TTreeViewItem *TreeViewItem27;
	TTreeViewItem *TreeViewItem28;
	TTreeViewItem *TreeViewItem29;
	TTreeViewItem *TreeViewItem34;
	TTreeViewItem *TreeViewItem35;
	TTreeViewItem *TreeViewItem36;
	TTreeViewItem *TreeViewItem37;
	TTreeViewItem *TreeViewItem30;
	TTreeViewItem *TreeViewItem31;
	TTreeViewItem *TreeViewItem32;
	TTreeViewItem *TreeViewItem33;
	TTreeViewItem *TreeViewItem5;
	TTreeViewItem *TreeViewItem6;
	TTreeViewItem *TreeViewItem7;
	TTreeViewItem *TreeViewItem8;
	TTreeViewItem *TreeViewItem9;
	TTreeViewItem *TreeViewItem10;
	TTreeViewItem *TreeViewItem11;
	TTreeViewItem *TreeViewItem12;
	TTreeViewItem *TreeViewItem13;
	TTreeViewItem *TreeViewItem14;
	TCheckBox *CheckBox8;
	TTabItem *TabItem6;
	TEllipse *Ellipse2;
	TListBox *ListTransform;
	TTrackBar *TrackBar2;
	TLabel *Label21;
	TLabel *Label22;
	TTrackBar *TrackBar3;
	TEdit *TextBox4;
	TTabItem *TabItem2;
	TPanel *Rectangle2;
	TVertScrollBox *VertScrollBox1;
	TExpander *Expander2;
	TButton *Button7;
	TTrackBar *TrackBar5;
	TEdit *TextBox5;
	TExpander *Expander3;
	TExpander *Expander4;
	TLabel *Label23;
	TSplitter *Splitter1;
	TPanel *Panel2;
	TScrollBox *ScrollBox1;
	TTabItem *TabItem8;
	TMemo *Memo2;
	TTabItem *TabItem7;
	TCornerButton *CornerButton1;
	TTrackBar *TrackBar4;
	TCheckBox *CheckBox3;
	TCheckBox *CheckBox4;
	TCheckBox *CheckBox5;
	TCheckBox *CheckBox6;
	TCornerButton *CornerButton2;
	TCornerButton *CornerButton3;
	TCornerButton *CornerButton4;
	TCornerButton *CornerButton5;
	TPath *Path2;
	TCornerButton *CornerButton6;
	TPath *Path3;
	TSpinBox *SpinBox1;
	TLabel *Label26;
	TComboEdit *comboedit1;
	TLabel *Label27;
	TComboTrackBar *ComboTrackBar1;
	TAlphaTrackBar *AlphaTrackBar1;
	TBWTrackBar *BWTrackBar1;
	THueTrackBar *HueTrackBar1;
	TLabel *Label28;
	TComboColorBox *ComboColorBox1;
	TListBox *ListBox2;
	TListBoxItem *ListBoxItem5;
	TListBoxItem *ListBoxItem7;
	TListBoxItem *ListBoxItem8;
	TListBoxItem *ListBoxItem9;
	TListBoxItem *ListBoxItem10;
	TListBoxItem *ListBoxItem11;
	TListBoxItem *ListBoxItem12;
	TListBoxItem *ListBoxItem13;
	TListBoxItem *ListBoxItem14;
	TListBoxItem *ListBoxItem15;
	TListBoxItem *ListBoxItem16;
	TListBoxItem *ListBoxItem17;
	TListBoxItem *ListBoxItem18;
	TListBoxItem *ListBoxItem19;
	TListBoxItem *ListBoxItem20;
	TListBoxItem *ListBoxItem21;
	TListBoxItem *ListBoxItem22;
	TListBoxItem *ListBoxItem23;
	TListBoxItem *ListBoxItem24;
	TListBoxItem *ListBoxItem25;
	TListBoxItem *ListBoxItem26;
	TListBoxItem *ListBoxItem27;
	TTreeView *TreeView2;
	TTreeViewItem *TreeViewItem38;
	TTreeViewItem *TreeViewItem39;
	TTreeViewItem *TreeViewItem40;
	TTreeViewItem *TreeViewItem41;
	TTreeViewItem *TreeViewItem42;
	TTreeViewItem *TreeViewItem43;
	TTreeViewItem *TreeViewItem44;
	TTreeViewItem *TreeViewItem45;
	TTreeViewItem *TreeViewItem46;
	TTreeViewItem *TreeViewItem47;
	TTreeViewItem *TreeViewItem48;
	TTreeViewItem *TreeViewItem49;
	TTreeViewItem *TreeViewItem50;
	TDateEdit *DateEdit1;
	TRectangle *Rectangle3;
	TCircle *Circle1;
	TButton *Button2;
	TBindingsList *BindingsList1;
	TLinkControlToProperty *LinkControlToPropertyValue;
	TListBoxItem *ListBoxItem30;
	TListBoxItem *ListBoxItem31;
	TListBoxItem *ListBoxItem32;
	TListBoxItem *ListBoxItem33;
	TListBoxItem *ListBoxItem34;
	TListBoxItem *ListBoxItem35;
	TListBoxItem *ListBoxItem36;
	TListBoxItem *ListBoxItem37;
	TListBoxItem *ListBoxItem38;
	TListBoxItem *ListBoxItem39;
	TListBoxItem *ListBoxItem40;
	TListBoxItem *ListBoxItem41;
	TListBoxItem *ListBoxItem42;
	TListBoxItem *ListBoxItem43;
	TListBoxItem *ListBoxItem44;
	TListBoxItem *ListBoxItem45;
	TListBoxItem *ListBoxItem46;
	TListBoxItem *ListBoxItem47;
	TListBoxItem *ListBoxItem48;
	TListBoxItem *ListBoxItem49;
	TListBoxItem *ListBoxItem50;
	TListBoxItem *ListBoxItem51;
	TListBoxItem *ListBoxItem53;
	TListBoxItem *ListBoxItem54;
	TListBoxItem *ListBoxItem55;
	TListBoxItem *ListBoxItem56;
	TListBoxItem *ListBoxItem57;
	TListBoxItem *ListBoxItem58;
	TListBoxItem *ListBoxItem59;
	TListBoxItem *ListBoxItem60;
	TListBoxItem *ListBoxItem61;
	TListBoxItem *ListBoxItem62;
	TListBoxItem *ListBoxItem63;
	TListBoxItem *ListBoxItem64;
	TListBoxItem *ListBoxItem65;
	TListBoxItem *ListBoxItem66;
	TListBoxItem *ListBoxItem67;
	TListBoxItem *ListBoxItem68;
	TListBoxItem *ListBoxItem69;
	TListBoxItem *ListBoxItem70;
	TListBoxItem *ListBoxItem71;
	TListBoxItem *ListBoxItem72;
	TListBoxItem *ListBoxItem73;
	TListBoxItem *ListBoxItem74;
	TButton *ButtonE3;
	TTrackBar *TrackBarE3;
	TEdit *EditE3;
	TButton *ButtonE4;
	TEdit *EditE4;
	TTrackBar *TrackBarE4;
	TEdit *Edit1;
	void __fastcall MenuItem6Click(TObject *Sender);
	void __fastcall MenuItem4Click(TObject *Sender);
	void __fastcall MenuItem2Click(TObject *Sender);
	void __fastcall MenuItem1Click(TObject *Sender);
	void __fastcall CheckBox2Change(TObject *Sender);
	void __fastcall calloutBottomChange(TObject *Sender);
	void __fastcall MenuItem3Click(TObject *Sender);
	void __fastcall ScaleTrackChange(TObject *Sender);
	void __fastcall AngleButton1Change(TObject *Sender);
	void __fastcall AngleButtonl1Change(TObject *Sender);
	void __fastcall AngleButton3Change(TObject *Sender);
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall Button4Click(TObject *Sender);
	void __fastcall AngleButton2Change(TObject *Sender);
	void __fastcall TrackBar2Change(TObject *Sender);
	void __fastcall TrackBar3Change(TObject *Sender);
	void __fastcall TrackBar4Change(TObject *Sender);
	void __fastcall CheckBox6Change(TObject *Sender);
	void __fastcall CheckBox5Change(TObject *Sender);
	void __fastcall CheckBox4Change(TObject *Sender);
	void __fastcall CheckBox3Change(TObject *Sender);
	void __fastcall CheckBox8Change(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall MenuItem7Click(TObject *Sender);
	void __fastcall DropTarget1DragOver(TObject *Sender, const TDragObject &Data, const TPointF &Point,
          TDragOperation &Operation);
	void __fastcall DropTarget1Dropped(TObject *Sender, const TDragObject &Data, const TPointF &Point);
private:	// User declarations
	TViewport3D *FViewport;
	TLayer3D *FContainer;
	void __fastcall SwitchTo3D();
	void __fastcall SwitchTo2D();
public:		// User declarations
	__fastcall TfrmCtrlsDemo(TComponent* Owner);
};

long rndm(long max);

//---------------------------------------------------------------------------
extern PACKAGE TfrmCtrlsDemo *frmCtrlsDemo;
//---------------------------------------------------------------------------
#endif
