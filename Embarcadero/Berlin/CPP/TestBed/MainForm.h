//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef MainFormH
#define MainFormH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.Memo.hpp>
#include <FMX.Objects.hpp>
#include <FMX.ScrollBox.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListBox.hpp>
//---------------------------------------------------------------------------
class TTestBedForm : public TForm
{
__published:	// IDE-managed Components
  TPanel *MainPanel;
  TPanel *RightPanel;
  TPaintBox *PaintBox;
  TSplitter *RightSplitter;
  TLayout *RightLayout;
  TComboBox *TestList;
  TLabel *TestLabel;
  TTimer *Timer1;
  TLine *Line1;
  TLayout *EnableLayout;
  TCheckBox *Sleep;
  TCheckBox *WarmStarting;
  TCheckBox *TimeOfImpact;
  TCheckBox *SubStepping;
  TLayout *DrawLayout;
  TCheckBox *ShapesChk;
  TCheckBox *AABBsChk;
  TCheckBox *ContactPointsChk;
  TCheckBox *ContactNormalsChk;
  TCheckBox *ContactImpulsesChk;
  TCheckBox *FrictionImpulsesChk;
  TCheckBox *CenterOfMassesChk;
  TCheckBox *ProfileChk;
  TCheckBox *StatisticsChk;
  TCheckBox *JointsChk;
  TLayout *ButtonsLayout;
  TButton *QuiBtn;
  TButton *RestartBtn;
  TButton *SingleStepBtn;
  TButton *PauseBtn;
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall FormDestroy(TObject *Sender);
  void __fastcall PaintBoxPaint(TObject *Sender, TCanvas *Canvas);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall FormResize(TObject *Sender);
  void __fastcall Timer1Timer(TObject *Sender);
  void __fastcall TestListChange(TObject *Sender);
  void __fastcall FormKeyDown(TObject *Sender, WORD &Key, System::WideChar &KeyChar,
          TShiftState Shift);
  void __fastcall PaintBoxMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          float X, float Y);
  void __fastcall PaintBoxMouseMove(TObject *Sender, TShiftState Shift, float X,
          float Y);
  void __fastcall PaintBoxMouseUp(TObject *Sender, TMouseButton Button, TShiftState Shift,
          float X, float Y);
  void __fastcall DrawLayoutChanged(TObject *Sender);
  void __fastcall PauseBtnClick(TObject *Sender);
  void __fastcall SingleStepBtnClick(TObject *Sender);
  void __fastcall RestartBtnClick(TObject *Sender);
  void __fastcall QuiBtnClick(TObject *Sender);
  void __fastcall FormKeyUp(TObject *Sender, WORD &Key, System::WideChar &KeyChar,
          TShiftState Shift);
private:	// User declarations
  void LoadTests();
  void ResetView();
  void TestChanged();
public:		// User declarations
  __fastcall TTestBedForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TTestBedForm *TestBedForm;
//---------------------------------------------------------------------------
#endif
