//---------------------------------------------------------------------------

#ifndef TabSlideTransitionH
#define TabSlideTransitionH
//---------------------------------------------------------------------------
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
//---------------------------------------------------------------------------
class TTabSlideTransitionFrmBase : public TForm
{
__published:	// IDE-managed Components
	TVertScrollBox *VertScrollBox1;
	TLayout *MainLayout1;
	TTabControl *TabControl1;
	TTabItem *TabItem1;
	TToolBar *ToolBar1;
	TLabel *Label2;
	TButton *Button1;
	TListBox *NameList;
	TListBoxGroupHeader *ListBoxGroupHeader1;
	TListBoxItem *firstName;
	TEdit *eFirstName;
	TListBoxItem *LastName;
	TEdit *eLastName;
	TTabItem *TabItem2;
	TToolBar *ToolBar2;
	TLabel *Label4;
	TButton *Button3;
	TButton *Button2;
	TListBox *PersonalInfoList;
	TListBoxGroupHeader *PersonalInformation;
	TListBoxItem *Address;
	TEdit *infoAddress;
	TListBoxItem *Phone;
	TEdit *infoTelephone;
	TListBoxItem *Email;
	TEdit *infoEmail;
	TListBoxItem *BirthDate;
	TCalendarEdit *infoDate;
	TTabItem *TabItem3;
	TToolBar *ToolBar3;
	TLabel *Label5;
	TButton *Button4;
	TButton *Button5;
	TListBox *EducationList;
	TListBoxGroupHeader *Education;
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
	TToolBar *ToolBar4;
	TLabel *Label6;
	TButton *Button6;
	TButton *Button7;
	TListBox *WorkList;
	TListBoxGroupHeader *WorkExperience;
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
	TToolBar *ToolBar5;
	TLabel *Label8;
	TButton *Button8;
	TLabel *PersonalInfoSummary;
	TGestureManager *GestureManager1;
	TActionList *ActionList1;
	TChangeTabAction *ChangeTabAction1;
	TChangeTabAction *ChangeTabAction2;
	TChangeTabAction *ChangeTabAction3;
	TChangeTabAction *ChangeTabAction4;
	TChangeTabAction *ChangeTabAction5;
	TChangeTabAction *ChangeTabAction6;
	TChangeTabAction *ChangeTabAction7;
	TChangeTabAction *ChangeTabAction8;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormVirtualKeyboardHidden(TObject *Sender, bool KeyboardVisible,
          const TRect &Bounds);
	void __fastcall FormVirtualKeyboardShown(TObject *Sender, bool KeyboardVisible,
		  const TRect &Bounds);
	void __fastcall CalcContentBoundsProc(TObject* Sender, TRectF &ContentBounds);
	void __fastcall FormFocusChanged(TObject *Sender);
	void __fastcall edChangeTracking(TObject *Sender);
	void __fastcall eUserChangeTracking(TObject *Sender);
	void __fastcall infoChangeTracking(TObject *Sender);
	void __fastcall weChangeTracking(TObject *Sender);
	void __fastcall TabControl1Gesture(TObject *Sender, const TGestureEventInfo &EventInfo,
          bool &Handled);
private:	// User declarations
	_di_IFMXVirtualKeyboardToolbarService FService;;
	TRectF FKBBounds;
	bool FNeedOffset;
	void __fastcall UpdateKBBounds();
	void __fastcall RestorePosition();
	void __fastcall UpdateMemo();
public:		// User declarations
	__fastcall TTabSlideTransitionFrmBase(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TTabSlideTransitionFrmBase *TabSlideTransitionFrmBase;
//---------------------------------------------------------------------------
#endif
