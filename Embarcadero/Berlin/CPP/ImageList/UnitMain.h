//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef UnitMainH
#define UnitMainH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <Data.Bind.Components.hpp>
#include <Data.Bind.EngExt.hpp>
#include <Data.Bind.GenData.hpp>
#include <Data.Bind.ObjectScope.hpp>
#include <FMX.ActnList.hpp>
#include <Fmx.Bind.DBEngExt.hpp>
#include <Fmx.Bind.Editors.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.ImgList.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.ListView.hpp>
#include <FMX.ListView.Appearances.hpp>
#include <FMX.ListView.Types.hpp>
#include <FMX.Menus.hpp>
#include <FMX.Objects.hpp>
#include <FMX.StdActns.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.TabControl.hpp>
#include <FMX.TreeView.hpp>
#include <FMX.Types.hpp>
#include <System.Actions.hpp>
#include <System.Bindings.Outputs.hpp>
#include <System.Rtti.hpp>
#include <System.Uitypes.hpp>
#include "UnitDataModule.h"
//---------------------------------------------------------------------------
class TMainForm : public TForm
{
__published:	// IDE-managed Components
	TMainMenu *MainMenu1;
	TMenuItem *MenuItem1;
	TMenuItem *MenuItem5;
	TMenuItem *MenuItem11;
	TMenuItem *MenuItem2;
	TMenuItem *MenuItem6;
	TMenuItem *MenuItem7;
	TMenuItem *MenuItem8;
	TMenuItem *MenuItem9;
	TMenuItem *MenuItem10;
	TMenuItem *MenuItem3;
	TMenuItem *MenuItem12;
	TMenuItem *MenuItem13;
	TMenuItem *MenuItem14;
	TMenuItem *MenuItem4;
	TMenuBar *MenuBar1;
	TMenuItem *CopyMenuItem1;
	TMenuItem *CopyMenuItem5;
	TMenuItem *CopyMenuItem11;
	TMenuItem *CopyMenuItem2;
	TMenuItem *CopyMenuItem6;
	TMenuItem *CopyMenuItem7;
	TMenuItem *CopyMenuItem8;
	TMenuItem *CopyMenuItem9;
	TMenuItem *CopyMenuItem10;
	TMenuItem *CopyMenuItem3;
	TMenuItem *CopyMenuItem12;
	TMenuItem *CopyMenuItem13;
	TMenuItem *CopyMenuItem14;
	TMenuItem *CopyMenuItem4;
	TTabControl *TabControl1;
	TTabItem *TabItem1;
	TPanel *Panel1;
	TButton *Button1;
	TCornerButton *CornerButton1;
	TSpeedButton *SpeedButton1;
	TButton *Button2;
	TSplitter *Splitter1;
	TTabItem *TabItem2;
	TSelection *Selection1;
	TRectangle *Rectangle1;
	TGlyph *Glyph1;
	TToolBar *ToolBar1;
	TSpeedButton *SpeedButton2;
	TSpeedButton *SpeedButton3;
	TLabel *Label1;
	TPanel *Panel2;
	TLabel *Label2;
	TCheckBox *CheckBox1;
	TButton *Button3;
	TButton *Button4;
	TButton *Button5;
	TButton *Button6;
	TButton *Button7;
	TSplitter *Splitter2;
	TTabItem *TabItem3;
	TPanel *Panel3;
	TTreeView *TreeView1;
	TTreeViewItem *TreeViewItem1;
	TTreeViewItem *TreeViewItem2;
	TTreeViewItem *TreeViewItem6;
	TTreeViewItem *TreeViewItem7;
	TTreeViewItem *TreeViewItem10;
	TTreeViewItem *TreeViewItem11;
	TTreeViewItem *TreeViewItem8;
	TTreeViewItem *TreeViewItem9;
	TTreeViewItem *TreeViewItem3;
	TTreeViewItem *TreeViewItem4;
	TTreeViewItem *TreeViewItem5;
	TTreeViewItem *TreeViewItem12;
	TTreeViewItem *TreeViewItem13;
	TTreeViewItem *TreeViewItem15;
	TTreeViewItem *TreeViewItem16;
	TTreeViewItem *TreeViewItem17;
	TTreeViewItem *TreeViewItem18;
	TTreeViewItem *TreeViewItem19;
	TTreeViewItem *TreeViewItem21;
	TTreeViewItem *TreeViewItem22;
	TTreeViewItem *TreeViewItem24;
	TTreeViewItem *TreeViewItem25;
	TTreeViewItem *TreeViewItem23;
	TTreeViewItem *TreeViewItem20;
	TTreeViewItem *TreeViewItem14;
	TListBox *ListBox1;
	TListBoxItem *ListBoxItem5;
	TListBoxItem *ListBoxItem6;
	TListBoxItem *ListBoxItem7;
	TListBoxItem *ListBoxItem8;
	TListBoxItem *ListBoxItem9;
	TComboBox *ComboBox1;
	TListBoxItem *ListBoxItem1;
	TListBoxItem *ListBoxItem2;
	TListBoxItem *ListBoxItem3;
	TListBoxItem *ListBoxItem4;
	TCheckBox *CheckBox3;
	TSplitter *Splitter3;
	TTabItem *TabItem4;
	TSplitter *Splitter4;
	TLayout *Layout1;
	TToolBar *ToolBar2;
	TButton *Button8;
	TButton *Button9;
	TButton *Button10;
	TButton *Button11;
	TCheckBox *CheckBox2;
	TListView *ListView2;
	TImage *Image1;
	TListView *ListView1;
	TPopupMenu *PopupMenu1;
	TMenuItem *MenuItem15;
	TMenuItem *MenuItem16;
	TMenuItem *MenuItem17;
	TMenuItem *MenuItem18;
	TMenuItem *MenuItem19;
	TActionList *ActionList1;
	TFileExit *FileExit1;
	TFileExit *FileExit2;
	TAction *ActnNextImage;
	TAction *ActnDormant;
	TAction *ActnUpCache;
	TAction *ActnDownCache;
	TAction *ActnClearCache;
	TAction *ActnUpdateText;
	TAction *ActnAddSource;
	TAction *ActnListViewAdd;
	TAction *ActnClear;
	TAction *ActnListAddBitmap;
	TAction *ActnAddBitmapAndImage;
	TAction *ActnImages;
	TControlAction *ActnInfo;
	TAction *ActnIncIndex;
	TAction *ActnDecIndex;
	TAction *ActnShowCheckBox;
	TPrototypeBindSource *PrototypeBindSource1;
	TBindingsList *BindingsList1;
	TLinkFillControlToField *LinkFillControlToField1;
	void __fastcall TabItem4Paint(TObject *Sender, TCanvas *Canvas, const TRectF &ARect);
	void __fastcall Glyph1Changed(TObject *Sender);
	void __fastcall ActnUpdateTextExecute(TObject *Sender);
	void __fastcall ActnAddSourceExecute(TObject *Sender);
	void __fastcall ActnNextImageExecute(TObject *Sender);
	void __fastcall ActnDormantExecute(TObject *Sender);
	void __fastcall ActnDormantUpdate(TObject *Sender);
	void __fastcall ActnUpCacheExecute(TObject *Sender);
	void __fastcall ActnUpCacheUpdate(TObject *Sender);
	void __fastcall ActnDownCacheExecute(TObject *Sender);
	void __fastcall ActnDownCacheUpdate(TObject *Sender);
	void __fastcall ActnClearExecute(TObject *Sender);
	void __fastcall ActnClearCacheExecute(TObject *Sender);
	void __fastcall ActnListViewAddExecute(TObject *Sender);
	void __fastcall ActnListAddBitmapExecute(TObject *Sender);
	void __fastcall ActnAddBitmapAndImageExecute(TObject *Sender);
	void __fastcall ActnImagesExecute(TObject *Sender);
	void __fastcall ActnImagesUpdate(TObject *Sender);
	void __fastcall ActnInfoUpdate(TObject *Sender);
	void __fastcall ActnIncIndexExecute(TObject *Sender);
	void __fastcall ActnDecIndexExecute(TObject *Sender);
	void __fastcall ActnShowCheckBoxExecute(TObject *Sender);
	void __fastcall ActnShowCheckBoxUpdate(TObject *Sender);

private:	// User declarations
	int FChangeCount;
	int FNumber;
	TImageLink * FImageLink;
	void __fastcall DrawPicture(TCanvas * ACanvas, TRectF R, float Scale);
	void __fastcall DrawTextOnLayer(const int Index, const String Text);
	void __fastcall AddSourceToItem(const int Index);
	void __fastcall OnImagesChange(TObject * Sender);
public:		// User declarations
	__fastcall TMainForm(TComponent* Owner);
	__fastcall ~TMainForm(void);
};
//---------------------------------------------------------------------------
extern PACKAGE TMainForm *MainForm;
//---------------------------------------------------------------------------
#endif
