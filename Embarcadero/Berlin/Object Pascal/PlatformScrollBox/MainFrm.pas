//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit MainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.MultiView, FMX.StdCtrls, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.ListBox, FMX.Layouts, FMX.Edit, FMX.EditBox, FMX.SpinBox, FMX.Memo, FMX.Objects, FMX.Calendar,
  FMX.ComboEdit, FMX.NumberBox;

type
  TControlClass = class of TControl;

const
  DemoControlsCount = 16;
  DemoControlsClasses: array [0..DemoControlsCount - 1] of TControlClass = (TButton, TTrackBar, TPanel, TAniIndicator, TArcDial,
    TProgressBar, TLabel, TRadioButton, TCheckBox, TSwitch, TEdit, TMemo, TCalendar, TNumberBox, TSpinBox, TComboEdit);

type
  TFormMain = class(TForm)
    PresentedScrollBox1: TPresentedScrollBox;
    MultiView1: TMultiView;
    ListBox1: TListBox;
    ListBoxGroupHeader1: TListBoxGroupHeader;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    cbBounces: TComboBox;
    ListBoxItem6: TListBoxItem;
    ListBoxItem7: TListBoxItem;
    ListBoxItem8: TListBoxItem;
    cbScrollAnimation: TComboBox;
    ListBoxItem9: TListBoxItem;
    ListBoxItem10: TListBoxItem;
    ListBoxItem11: TListBoxItem;
    cbScrollDirection: TComboBox;
    ListBoxItem4: TListBoxItem;
    ListBoxItem5: TListBoxItem;
    ListBoxItem12: TListBoxItem;
    ListBoxItem13: TListBoxItem;
    cbTouchTracking: TComboBox;
    ListBoxItem14: TListBoxItem;
    ListBoxItem15: TListBoxItem;
    ListBoxItem16: TListBoxItem;
    ListBoxGroupHeader2: TListBoxGroupHeader;
    ListBoxItem17: TListBoxItem;
    cbAutoHide: TComboBox;
    ListBoxItem18: TListBoxItem;
    ListBoxItem19: TListBoxItem;
    ListBoxItem20: TListBoxItem;
    ListBoxItem21: TListBoxItem;
    swShowScrollBars: TSwitch;
    ListBoxItem22: TListBoxItem;
    swShowSizeGrip: TSwitch;
    ListBoxGroupHeader3: TListBoxGroupHeader;
    ListBoxItem23: TListBoxItem;
    ListBoxItem24: TListBoxItem;
    ListBoxItem25: TListBoxItem;
    spContentWidth: TSpinBox;
    spContentHeight: TSpinBox;
    swAutoSize: TSwitch;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    ListBoxItem30: TListBoxItem;
    swScrollEnabled: TSwitch;
    ListBoxItem31: TListBoxItem;
    swDisableMouseWheel: TSwitch;
    ListBoxGroupHeader4: TListBoxGroupHeader;
    ListBoxItem26: TListBoxItem;
    swControlType: TSwitch;
    ListBoxGroupHeader5: TListBoxGroupHeader;
    ListBoxItem27: TListBoxItem;
    ListBoxItem28: TListBoxItem;
    ListBoxItem29: TListBoxItem;
    ListBoxGroupHeader6: TListBoxGroupHeader;
    ListBoxItem32: TListBoxItem;
    ListBoxItem33: TListBoxItem;
    ListBoxItem34: TListBoxItem;
    ListBoxItem35: TListBoxItem;
    spViewPortPositionX: TSpinBox;
    spViewPortPositionY: TSpinBox;
    spViewPortWidth: TSpinBox;
    spViewPortHeight: TSpinBox;
    ListBoxItem36: TListBoxItem;
    Button2: TButton;
    Layout1: TLayout;
    procedure cbScrollAnimationChange(Sender: TObject);
    procedure cbBouncesChange(Sender: TObject);
    procedure cbScrollDirectionChange(Sender: TObject);
    procedure cbTouchTrackingChange(Sender: TObject);
    procedure cbAutoHideChange(Sender: TObject);
    procedure swShowScrollBarsSwitch(Sender: TObject);
    procedure swShowSizeGripSwitch(Sender: TObject);
    procedure swAutoSizeSwitch(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure spContentWidthChange(Sender: TObject);
    procedure spContentHeightChange(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure PresentedScrollBox1ViewportPositionChange(Sender: TObject; const OldViewportPosition,
      NewViewportPosition: TPointF; const ContentSizeChanged: Boolean);
    procedure swScrollEnabledClick(Sender: TObject);
    procedure swDisableMouseWheelClick(Sender: TObject);
    procedure swControlTypeSwitch(Sender: TObject);
    procedure spViewPortPositionXChange(Sender: TObject);
    procedure PresentedScrollBox1Resize(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    procedure SetChildrenControlType(const ARoot: TFmxObject; const AControlType: TControlType);
    procedure UpdateViewportSize;
    procedure UpdateViewportPositionWithoutOnChange;
  public
    procedure FillGridControls;
    procedure FillDemoControls;
  end;

var
  FormMain: TFormMain;

implementation

uses
  FMX.BehaviorManager;

{$R *.fmx}

procedure TFormMain.Button2Click(Sender: TObject);
begin
  PresentedScrollBox1.ScrollToCenter(True);
end;

procedure TFormMain.Button4Click(Sender: TObject);
begin
  PresentedScrollBox1.ScrollToTop(True);
end;

procedure TFormMain.Button5Click(Sender: TObject);
begin
  PresentedScrollBox1.ScrollBy(100, 100);
end;

procedure TFormMain.Button6Click(Sender: TObject);
begin
  // Right bottom corner, Size less ViewPortSize
  PresentedScrollBox1.InViewRect(TRectF.Create(900, 900, 1500, 1500));
end;

procedure TFormMain.cbAutoHideChange(Sender: TObject);
begin
  if cbAutoHide.ItemIndex <> -1 then
    PresentedScrollBox1.AutoHide := TBehaviorBoolean(cbAutoHide.ItemIndex);
  UpdateViewportSize;
end;

procedure TFormMain.cbBouncesChange(Sender: TObject);
begin
  if cbBounces.ItemIndex <> -1 then
    PresentedScrollBox1.Bounces := TBehaviorBoolean(cbBounces.ItemIndex);
end;

procedure TFormMain.cbScrollAnimationChange(Sender: TObject);
begin
  if cbScrollAnimation.ItemIndex <> -1 then
    PresentedScrollBox1.ScrollAnimation := TBehaviorBoolean(cbScrollAnimation.ItemIndex);
end;

procedure TFormMain.cbScrollDirectionChange(Sender: TObject);
begin
  if cbScrollDirection.ItemIndex <> -1 then
    PresentedScrollBox1.ScrollDirections := TScrollDirections(cbScrollDirection.ItemIndex);
  UpdateViewportSize;
end;

procedure TFormMain.cbTouchTrackingChange(Sender: TObject);
begin
  if cbTouchTracking.ItemIndex <> -1 then
    PresentedScrollBox1.TouchTracking := TBehaviorBoolean(cbTouchTracking.ItemIndex);
end;

procedure TFormMain.FillDemoControls;
const
  ColCount = 20;
  RowCount = 20;
var
  Col: Integer;
  Cell: TControl;
  Row: Integer;
  CellSize: TSizeF;
  ControlSize: TSizeF;
begin
  Randomize;
  CellSize.Width := PresentedScrollBox1.ContentSize.Width / ColCount;
  CellSize.Height := PresentedScrollBox1.ContentSize.Height / RowCount;
  PresentedScrollBox1.BeginUpdate;
  try
    for Col := 1 to ColCount do
      for Row := 1 to RowCount do
      begin
        Cell := DemoControlsClasses[Random(DemoControlsCount)].Create(nil);
        ControlSize := TSizeF.Create(Random(Round(CellSize.Width - 50)) + 50, Random(Round(CellSize.Height - 50)) + 50);
        Cell.Size.Size := ControlSize;
//        Cell.HitTest := False;
        Cell.Position.Point := TPointF.Create(CellSize.Width * (Col - 1), CellSize.Height * (Row - 1));
        Cell.Parent := PresentedScrollBox1;
      end;
  finally
    PresentedScrollBox1.EndUpdate;
  end;
end;

procedure TFormMain.FillGridControls;
const
  ColCount = 50;
  RowCount = 50;
var
  Col: Integer;
  Cell: TRectangle;
  Row: Integer;
  CellSize: TSizeF;
  Red: Single;
  Green: Single;
  Blue: Single;
begin
  CellSize.Width := PresentedScrollBox1.ContentSize.Width / ColCount;
  CellSize.Height := PresentedScrollBox1.ContentSize.Height / RowCount;
  PresentedScrollBox1.BeginUpdate;
  try
    for Col := 1 to ColCount do
      for Row := 1 to RowCount do
      begin
        Cell := TRectangle.Create(nil);
        Cell.Size.Size := CellSize;
        Cell.HitTest := False;
        Cell.Fill.Color := TAlphaColorF.Create(Col / ColCount, Row / RowCount, 0.5).ToAlphaColor;
        Cell.Stroke.Thickness := 0.5;
        Cell.Position.Point := TPointF.Create(CellSize.Width * (Col - 1), CellSize.Height * (Row - 1));
        Cell.Parent := PresentedScrollBox1;
      end;
  finally
    PresentedScrollBox1.EndUpdate;
  end;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
// Uncomment, if you would like to look at the performance of scrolling
//  FillGridControls;
  FillDemoControls;
  spContentWidth.Value := PresentedScrollBox1.ContentSize.Width;
  spContentHeight.Value := PresentedScrollBox1.ContentSize.Height;
  swAutoSize.IsChecked := PresentedScrollBox1.AutoCalculateContentSize;
  swShowScrollBars.IsChecked := PresentedScrollBox1.ShowScrollBars;
  swShowSizeGrip.IsChecked := PresentedScrollBox1.ShowSizeGrip;
  cbScrollDirection.ItemIndex := Integer(PresentedScrollBox1.ScrollDirections);
  cbTouchTracking.ItemIndex := Integer(PresentedScrollBox1.TouchTracking);
  cbScrollAnimation.ItemIndex := Integer(PresentedScrollBox1.ScrollAnimation);
  cbBounces.ItemIndex := Integer(PresentedScrollBox1.Bounces);
  cbAutoHide.ItemIndex := Integer(PresentedScrollBox1.AutoHide);
  swDisableMouseWheel.IsChecked := not PresentedScrollBox1.DisableMouseWheel;
  swScrollEnabled.IsChecked := PresentedScrollBox1.EnabledScroll;
end;

procedure TFormMain.PresentedScrollBox1Resize(Sender: TObject);
begin
  UpdateViewportSize;
end;

procedure TFormMain.PresentedScrollBox1ViewportPositionChange(Sender: TObject; const OldViewportPosition,
  NewViewportPosition: TPointF; const ContentSizeChanged: Boolean);
begin
  UpdateViewportSize;
  UpdateViewportPositionWithoutOnChange;
end;

procedure TFormMain.SetChildrenControlType(const ARoot: TFmxObject; const AControlType: TControlType);
var
  I: Integer;
begin
  for I := 0 to ARoot.ChildrenCount - 1 do
    if ARoot.Children[I] is TPresentedControl then
      TPresentedControl(ARoot.Children[I]).ControlType := AControlType
    else
      SetChildrenControlType(ARoot.Children[I], AControlType);
end;

procedure TFormMain.spContentHeightChange(Sender: TObject);
begin
  PresentedScrollBox1.ContentSize.Height := spContentHeight.Value
end;

procedure TFormMain.spContentWidthChange(Sender: TObject);
begin
  PresentedScrollBox1.ContentSize.Width := spContentWidth.Value;
end;

procedure TFormMain.spViewPortPositionXChange(Sender: TObject);
begin
  PresentedScrollBox1.ViewportPosition := TPointF.Create(spViewPortPositionX.Value, spViewPortPositionY.Value);
end;

procedure TFormMain.swAutoSizeSwitch(Sender: TObject);
begin
  PresentedScrollBox1.AutoCalculateContentSize := swAutoSize.IsChecked;
  if not swAutoSize.IsChecked then
    PresentedScrollBox1.ContentSize.Size := TSizeF.Create(spContentWidth.Value, spContentHeight.Value)
  else
  begin
    spContentWidth.Value := PresentedScrollBox1.ContentSize.Width;
    spContentHeight.Value := PresentedScrollBox1.ContentSize.Height;
  end;
  UpdateViewportSize;
end;

procedure TFormMain.swControlTypeSwitch(Sender: TObject);
begin
  if swControlType.IsChecked then
    PresentedScrollBox1.ControlType := TControlType.Platform
  else
    PresentedScrollBox1.ControlType := TControlType.Styled;

  SetChildrenControlType(PresentedScrollBox1, PresentedScrollBox1.ControlType);
end;

procedure TFormMain.swDisableMouseWheelClick(Sender: TObject);
begin
  PresentedScrollBox1.DisableMouseWheel := not swDisableMouseWheel.IsChecked;
end;

procedure TFormMain.swScrollEnabledClick(Sender: TObject);
begin
  PresentedScrollBox1.EnabledScroll := swScrollEnabled.IsChecked;
end;

procedure TFormMain.swShowScrollBarsSwitch(Sender: TObject);
begin
  PresentedScrollBox1.ShowScrollBars := swShowScrollBars.IsChecked;
  UpdateViewportSize;
end;

procedure TFormMain.swShowSizeGripSwitch(Sender: TObject);
begin
  PresentedScrollBox1.ShowSizeGrip := swShowSizeGrip.IsChecked;
end;

procedure TFormMain.UpdateViewportPositionWithoutOnChange;
var
  OldChange: TNotifyEvent;
begin
  OldChange := spViewPortPositionX.OnChange;
  try
    spViewPortPositionX.OnChange := nil;
    spViewPortPositionY.OnChange := nil;
    spViewPortPositionX.Value := PresentedScrollBox1.ViewportPosition.X;
    spViewPortPositionY.Value := PresentedScrollBox1.ViewportPosition.Y;
  finally
    spViewPortPositionX.OnChange := OldChange;
    spViewPortPositionY.OnChange := OldChange;
  end;
end;

procedure TFormMain.UpdateViewportSize;
begin
  spViewPortWidth.Value := PresentedScrollBox1.ViewportSize.Width;
  spViewPortHeight.Value := PresentedScrollBox1.ViewportSize.Height;
end;

end.
