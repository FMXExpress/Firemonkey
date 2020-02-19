//---------------------------------------------------------------------------
// Copyright (c) 2016 Embarcadero Technologies, Inc. All rights reserved.  
//
// This software is the copyrighted property of Embarcadero Technologies, Inc. 
// ("Embarcadero") and its licensors. You may only use this software if you 
// are an authorized licensee of Delphi, C++Builder or RAD Studio 
// (the "Embarcadero Products").  This software is subject to Embarcadero's 
// standard software license and support agreement that accompanied your 
// purchase of the Embarcadero Products and is considered a Redistributable, 
// as such term is defined thereunder. Your use of this software constitutes 
// your acknowledgement of your agreement to the foregoing software license 
// and support agreement. 
//---------------------------------------------------------------------------
// ---------------------------------------------------------------------------

#ifndef UnitMainH
#define UnitMainH



#include <Data.Bind.Components.hpp>
#include <Data.Bind.Controls.hpp>
#include <Data.Bind.DBScope.hpp>
#include <Data.Bind.EngExt.hpp>
#include <Data.Bind.Grid.hpp>
#include <Data.DB.hpp>
#include <FireDAC.Comp.Client.hpp>
#include <FireDAC.Comp.DataSet.hpp>
#include <FireDAC.DApt.Intf.hpp>
#include <FireDAC.DatS.hpp>
#include <FireDAC.Phys.Intf.hpp>
#include <FireDAC.Stan.Error.hpp>
#include <FireDAC.Stan.Intf.hpp>
#include <FireDAC.Stan.Option.hpp>
#include <FireDAC.Stan.Param.hpp>
#include <FMX.ActnList.hpp>
#include <Fmx.Bind.DBEngExt.hpp>
#include <Fmx.Bind.Editors.hpp>
#include <Fmx.Bind.Grid.hpp>
#include <Fmx.Bind.Navigator.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.Grid.hpp>
#include <FMX.Grid.Style.hpp>
#include <FMX.ImgList.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.Memo.hpp>
#include <FMX.ScrollBox.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.TabControl.hpp>
#include <FMX.TreeView.hpp>
#include <FMX.Types.hpp>
#include <System.Actions.hpp>
#include <System.Bindings.Outputs.hpp>
#include <System.Classes.hpp>
#include <System.ImageList.hpp>
#include <System.Rtti.hpp>
// ---------------------------------------------------------------------------
#include <System.SysUtils.hpp>
#include <System.Actions.hpp>
#include <System.Rtti.hpp>
#include <System.Types.hpp>
#include <System.RTLConsts.hpp>
#include <System.ImageList.hpp>
#include <System.TypInfo.hpp>
#include <System.UITypes.hpp>
#include <System.Classes.hpp>
#include <System.Variants.hpp>
#include <System.Math.hpp>
#include <FMX.Types.hpp>
#include <FMX.Consts.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Graphics.hpp>
#include <FMX.Dialogs.hpp>
#include <FMX.Grid.Style.hpp>
#include <FMX.ActnList.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.ScrollBox.hpp>
#include <FMX.Grid.hpp>
#include <FMX.TabControl.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ImgList.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.TreeView.hpp>
#include <FMX.Effects.hpp>
//#include <FMX.Grid.iOS.hpp>
#include <FMX.StdActns.hpp>
#include <Data.Bind.Controls.hpp>
#include <FMX.Bind.Navigator.hpp>
#include <FireDAC.Stan.Intf.hpp>
#include <FireDAC.Stan.Option.hpp>
#include <FireDAC.Stan.Param.hpp>
#include <FireDAC.Stan.Error.hpp>
#include <FireDAC.DatS.hpp>
#include <FireDAC.Phys.Intf.hpp>
#include <FireDAC.DApt.Intf.hpp>
#include <Data.Bind.EngExt.hpp>
#include <FMX.Bind.DBEngExt.hpp>
#include <FMX.Bind.Grid.hpp>
#include <System.Bindings.Outputs.hpp>
#include <FMX.Bind.Editors.hpp>
#include <Data.Bind.Components.hpp>
#include <Data.Bind.Grid.hpp>
#include <Data.Bind.DBScope.hpp>
#include <Data.DB.hpp>
#include <FireDAC.Comp.DataSet.hpp>
#include <FireDAC.Comp.Client.hpp>
#include <FMX.Memo.hpp>
#include <FMX.Objects.hpp>
#include <FMX.DateTimeCtrls.hpp>

// ---------------------------------------------------------------------------
class TMainForm : public TForm {
__published: // IDE-managed Components
    TTabControl *TabControl1;
    TLayout *Layout1;
    TTabItem *TabItem1;
	TStringGrid *StringGrid1;
    TActionList *ActionList1;
    TControlAction *actnControlType;
    TStringColumn *StringColumn1;
    TColumn *Column1;
    TProgressColumn *ProgressColumn1;
    TCurrencyColumn *CurrencyColumn1;
    TCheckColumn *CheckColumn1;
    TDateColumn *DateColumn1;
    TTimeColumn *TimeColumn1;
    TPopupColumn *PopupColumn1;
    TGlyphColumn *GlyphColumn1;
    TImageList *ImageList1;
    TCheckBox *CheckBox1;
    TControlAction *actnVisibleColumn;
    TCheckBox *CheckBox2;
    TControlAction *actnWidthColumn;
    TCheckBox *CheckBox3;
    TTreeView *TreeView1;
    TSplitter *Splitter1;
    TVertScrollBox *VertScrollBox1;
    TControlAction *actnEnableColumn;
    TCheckBox *CheckBox5;
    TCheckBox *CheckBox6;
    TControlAction *actnReadOnlyColumn;
    TTabItem *TabItem2;
    TControlAction *actnOptions;
    TSpeedButton *SpeedButton1;
    TGrid *Grid1;
    TTabItem *TabItem3;
    TGrid *Grid2;
    TBindNavigator *BindNavigator1;
    TFDMemTable *FDMemTable1;
    TIntegerField *FDMemTable1Index;
	TIntegerField *FDMemTable1ImageIndex;
    TStringField *FDMemTable1String;
    TStringField *FDMemTable1Column;
    TBooleanField *FDMemTable1CheckBox;
    TDateField *FDMemTable1Date;
    TTimeField *FDMemTable1Time;
	TCurrencyField *FDMemTable1Currency;
    TBindSourceDB *BindSourceDB1;
    TBindingsList *BindingsList1;
    TLinkGridToDataSource *LinkGridToDataSourceBindSourceDB1;
    TSplitter *Splitter2;
	TMemo *Memo1;
	void __fastcall actnControlTypeUpdate(TObject *Sender);
	void __fastcall actnControlTypeExecute(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormDestroy(TObject *Sender);
	void __fastcall actnVisibleColumnExecute(TObject *Sender);
	void __fastcall actnVisibleColumnUpdate(TObject *Sender);
	void __fastcall actnWidthColumnExecute(TObject *Sender);
	void __fastcall actnWidthColumnUpdate(TObject *Sender);
	void __fastcall actnEnableColumnExecute(TObject *Sender);
	void __fastcall actnEnableColumnUpdate(TObject *Sender);
	void __fastcall actnReadOnlyColumnExecute(TObject *Sender);
	void __fastcall actnReadOnlyColumnUpdate(TObject *Sender);
	void __fastcall actnOptionsExecute(TObject *Sender);
	void __fastcall actnOptionsUpdate(TObject *Sender);
	void __fastcall Grid1GetValue(TObject *Sender, const int ACol, const int ARow, TValue &Value);
	void __fastcall Grid1SetValue(TObject *Sender, const int ACol, const int ARow, const TValue &Value);
	void __fastcall TreeView1ChangeCheck(TObject *Sender);
	void __fastcall VertScrollBox1Resize(TObject *Sender);
	void __fastcall DrawColumnBackgroundProc(TObject *Sender, TCanvas * const Canvas, TColumn * const Column, const TRectF &Bounds,
          const int Row, const TValue &Value, const TGridDrawStates State);
	void __fastcall DrawColumnCellProc(TObject *Sender, TCanvas * const Canvas, TColumn * const Column, const TRectF &Bounds,
          const int Row, const TValue &Value, const TGridDrawStates State);
	void __fastcall DrawColumnHeaderProc(TObject *Sender, TCanvas * const Canvas, TColumn * const Column, const TRectF &Bounds);
	void __fastcall EditingDoneProc(TObject *Sender, const int ACol, const int ARow);
	void __fastcall HeaderClickProc(TColumn *Column);
	void __fastcall ResizeProc(TObject *Sender);
	void __fastcall SelChangedProc(TObject *Sender);
	void __fastcall ColumnMovedProc(TColumn *Column, int FromIndex, int ToIndex);
	void __fastcall SelectCellProc(TObject *Sender, const int ACol, const int ARow, bool &CanSelect);
	void __fastcall TapProc(TObject *Sender, const TPointF &Point);

private: // User declarations
	TTreeViewItem *FItemGridOptions;
	TTreeViewItem *FItemCellReturn;
	TTreeViewItem *FDefaultDrawing;
	System::Boolean FTreeUpdating;
	DynamicArray<TBitmap*> FBitmaps;
	DynamicArray<String> FStrings;
	DynamicArray<String> FPopups;
	DynamicArray<TDateTime>	FDates;
	DynamicArray<TDateTime> FTimes;
	void __fastcall InitStringValue(const int ColumnIndex, int &RowIndex, const String Caption,
		 const String Value);
	void __fastcall UpdateGridByTreeView();
	void __fastcall UpdateTreeView();
	void __fastcall InternalPropagate(TGrid * const Grid);
	void __fastcall PropagateOptions();
	void __fastcall MemoAddLine(const String EventName, const String Text);
public: // User declarations
	__fastcall TMainForm(TComponent* Owner);
};

// ---------------------------------------------------------------------------
extern PACKAGE TMainForm *MainForm;
// ---------------------------------------------------------------------------
#endif
