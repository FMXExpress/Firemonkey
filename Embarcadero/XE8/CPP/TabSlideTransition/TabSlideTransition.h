// ---------------------------------------------------------------------------

#ifndef TabSlideTransitionH
#define TabSlideTransitionH
// ---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.ActnList.hpp>
#include <FMX.DateTimeCtrls.hpp>
#include <FMX.Edit.hpp>
#include <FMX.Gestures.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.Memo.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.TabControl.hpp>
#include <FMX.Types.hpp>
#include <System.Actions.hpp>
#include <FMX.MobilePreview.hpp>
#include <FMX.Controls.Presentation.hpp>

// ---------------------------------------------------------------------------
class TTabSlideTransitionFrmBase : public TForm {
__published: // IDE-managed Components

	TActionList *ActionList1;
	TPreviousTabAction *PreviousTabAction;
	TNextTabAction *NextTabAction;
	TControlAction *TitleAction;
	TToolBar *ToolBar1;
	TLabel *Label2;
	TButton *Button1;
	TButton *Button2;
	TVertScrollBox *VertScrollBox1;
	TLayout *MainLayout1;
	TTabControl *TabControl1;
	TTabItem *TabItem1;
	TListBox *NameList;
	TListBoxItem *firstName;
	TEdit *efirstName;
	TListBoxItem *LastName;
	TEdit *eLastName;
	TTabItem *TabItem2;
	TListBox *PersonalInfoList;
	TListBoxItem *Address;
	TEdit *infoAddress;
	TListBoxItem *Phone;
	TEdit *infoTelephone;
	TListBoxItem *Email;
	TEdit *infoEmail;
	TListBoxItem *BirthDate;
	TDateEdit *infoDate;
	TTabItem *TabItem3;
	TListBox *EducationList;
	TListBoxItem *Institution;
	TEdit *edInstitution;
	TListBoxItem *City;
	TEdit *edCity;
	TListBoxItem *AdmissionDate;
	TComboBox *edAdmissionDate;
	TListBoxItem *ListBoxItem5;
	TListBoxItem *ListBoxItem6;
	TListBoxItem *ListBoxItem7;
	TListBoxItem *ListBoxItem8;
	TListBoxItem *ListBoxItem9;
	TListBoxItem *GraduationDate;
	TComboBox *edGraduationDate;
	TListBoxItem *ListBoxItem10;
	TListBoxItem *ListBoxItem11;
	TListBoxItem *ListBoxItem12;
	TListBoxItem *ListBoxItem13;
	TListBoxItem *ListBoxItem14;
	TListBoxItem *ListBoxItem1;
	TTabItem *TabItem4;
	TListBox *WorkList;
	TListBoxItem *Employer;
	TEdit *weEmpName;
	TListBoxItem *EmployerCity;
	TEdit *weCity;
	TListBoxItem *CurrentJob;
	TEdit *weOccupiedJob;
	TListBoxItem *FromDate;
	TComboBox *weFrom;
	TListBoxItem *ListBoxItem18;
	TListBoxItem *ListBoxItem19;
	TListBoxItem *ListBoxItem20;
	TListBoxItem *ListBoxItem21;
	TListBoxItem *ListBoxItem22;
	TListBoxItem *ToDate;
	TComboBox *weTo;
	TListBoxItem *ListBoxItem23;
	TListBoxItem *ListBoxItem24;
	TListBoxItem *ListBoxItem25;
	TListBoxItem *ListBoxItem26;
	TListBoxItem *ListBoxItem27;
	TListBoxItem *ListBoxItem2;
	TTabItem *TabItem5;
	TMemo *Memo1;
	TGestureManager *GestureManager1;

	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormVirtualKeyboardHidden(TObject *Sender, bool KeyboardVisible, const TRect &Bounds);
	void __fastcall FormVirtualKeyboardShown(TObject *Sender, bool KeyboardVisible, const TRect &Bounds);
	void __fastcall CalcContentBoundsProc(TObject* Sender, TRectF &ContentBounds);
	void __fastcall FormFocusChanged(TObject *Sender);
	void __fastcall ActionList1Update(TBasicAction *Action, bool &Handled);
	void __fastcall FormKeyUp(TObject *Sender, WORD &Key, System::WideChar &KeyChar, TShiftState Shift);
	void __fastcall TabControl1Gesture(TObject *Sender, const TGestureEventInfo &EventInfo, bool &Handled);

private: // User declarations

	_di_IFMXVirtualKeyboardToolbarService FService; ;
	TRectF FKBBounds;
	bool FNeedOffset;

	void __fastcall UpdateKBBounds();
	void __fastcall RestorePosition();
	void __fastcall UpdateMemo();
	TListBox* FindListBox(TFmxObject* AObj);
	bool GetIsFilled(TListBox* AList);

public: // User declarations
	__fastcall TTabSlideTransitionFrmBase(TComponent* Owner);
};

// ---------------------------------------------------------------------------
extern PACKAGE TTabSlideTransitionFrmBase *TabSlideTransitionFrmBase;
// ---------------------------------------------------------------------------
#endif
