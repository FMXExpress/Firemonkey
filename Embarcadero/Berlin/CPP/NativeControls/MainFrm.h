// ---------------------------------------------------------------------------

#ifndef MainFrmH
#define MainFrmH
// ---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <Data.Bind.Components.hpp>
#include <Data.Bind.EngExt.hpp>
#include <Data.Bind.GenData.hpp>
#include <Data.Bind.ObjectScope.hpp>
#include <Fmx.Bind.DBEngExt.hpp>
#include <Fmx.Bind.Editors.hpp>
#include <Fmx.Bind.GenData.hpp>
#include <FMX.Calendar.hpp>
#include <FMX.Colors.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.Edit.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListView.Adapters.Base.hpp>
#include <FMX.ListView.Appearances.hpp>
#include <FMX.ListView.hpp>
#include <FMX.ListView.Types.hpp>
#include <FMX.Memo.hpp>
#include <FMX.Objects.hpp>
#include <FMX.ScrollBox.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.TabControl.hpp>
#include <FMX.Types.hpp>
#include <FMX.WebBrowser.hpp>
#include <System.Bindings.Outputs.hpp>
#include <System.Rtti.hpp>
#include <FMX.Maps.hpp>
#include <map>

// ---------------------------------------------------------------------------
class TFormMain : public TForm {
__published: // IDE-managed Components

	TLayout *Layout1;
	TTabControl *TabControl1;
	TTabItem *TabItemMenu;
	TListView *ListViewMenu;
	TTabItem *TabItemEditPrompt;
	TEdit *Edit2;
	TLabel *Label2;
	TTabItem *TabItemEditCursorColor;
	TEdit *Edit1;
	TLabel *Label4;
	TLabel *Label14;
	TEdit *Edit8;
	TTabItem *TabItemEditClearButton;
	TEdit *Edit3;
	TClearEditButton *ClearEditButton2;
	TLabel *Label3;
	TEdit *Edit12;
	TClearEditButton *ClearEditButton3;
	TTabItem *TabItemEditCustomBackground;
	TLabel *Label15;
	TImage *Image1;
	TRectangle *Rectangle1;
	TEdit *Edit9;
	TClearEditButton *ClearEditButton1;
	TEdit *Edit5;
	TLabel *Label7;
	TTabItem *TabItemEditTextAlign;
	TEdit *Edit4;
	TLabel *Label6;
	TEdit *Edit6;
	TLabel *Label12;
	TEdit *Edit7;
	TLabel *Label13;
	TTabItem *TabItemEditFonstSettings;
	TEdit *Edit10;
	TEdit *Edit11;
	TEdit *Edit13;
	TTabItem *TabItemMemoCursorColor;
	TLabel *Label8;
	TLabel *Label16;
	TMemo *Memo1;
	TMemo *Memo6;
	TTabItem *TabItemMemoFontSettings;
	TLabel *Label9;
	TMemo *Memo2;
	TTabItem *TabItemMemoDetectingLinks;
	TMemo *Memo3;
	TLabel *Label10;
	TTabItem *TabItemMemoCheckSpelling;
	TMemo *Memo4;
	TTabItem *TabItemMemoCustomBackground;
	TImage *Image2;
	TRectangle *Rectangle2;
	TMemo *Memo5;
	TTabItem *TabItemCalendar;
	TCalendar *Calendar1;
	TLabel *Label1;
	TTabItem *TabItemListView;
	TListView *ListView1;
	TTabItem *TabItemSwitch;
	TLabel *Label5;
	TSwitch *Switch1;
	TTabItem *TabItemScrollBox;
	TPresentedScrollBox *PresentedScrollBox1;
	TTrackBar *TrackBar1;
	TColorQuad *ColorQuad1;
	TColorPicker *ColorPicker1;
	TTabItem *TabItemWebBrowser;
	TWebBrowser *WebBrowser1;
	TToolBar *ToolBar1;
	TLabel *Label11;
	TSpeedButton *SpeedButtonBackToMenu;
	TPrototypeBindSource *PrototypeBindSource1;
	TBindingsList *BindingsList1;
	TLinkFillControlToField *LinkFillControlToField1;
	TTabItem *TabItemMap;
	TMapView *MapView1;
	TText *Text1;

	void __fastcall FormDestroy(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall SpeedButtonBackToMenuClick(TObject *Sender);
	void __fastcall ListViewMenuItemClickEx(TObject * const Sender, int ItemIndex, const TPointF &LocalClickPos, TListItemDrawable * const ItemObject);
	void __fastcall TabControl1Change(TObject *Sender);


private:
	std::map<TListViewItem*, TTabItem*> *fMenu;
	void addItem(const UnicodeString text, TTabItem* tabItem,
		TListItemPurpose purpose = TListItemPurpose::None);

public: // User declarations
	__fastcall TFormMain(TComponent* Owner);
};

// ---------------------------------------------------------------------------
extern PACKAGE TFormMain *FormMain;
// ---------------------------------------------------------------------------
#endif
