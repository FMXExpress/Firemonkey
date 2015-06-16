
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Text, FMX.TextLayout, FMX.StdCtrls,
  FMX.Edit, FMX.Layouts, FMX.ListBox, FMX.Memo, FMX.Ani, FMX.Colors, FMX.Objects,
  System.Actions, FMX.ActnList, FMX.Menus, FMX.Graphics;

type
  TMainForm = class(TForm)
    pnlText: TPanel;
    Panel3: TPanel;
    Label9: TLabel;
    lbLayouts: TListBox;
    LayoutsPopupMenu: TPopupMenu;
    FMXActionList1: TActionList;
    actAddLayout: TAction;
    actEditLayout: TAction;
    actDeleteLayout: TAction;
    actClearLayouts: TAction;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    lbLayoutAttributes: TListBox;
    Label6: TLabel;
    actAddAttribute: TAction;
    actDeleteAttribute: TAction;
    actClearAttributes: TAction;
    LayoutAttributesPopupMenu: TPopupMenu;
    MenuItem5: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    MenuItem10: TMenuItem;
    actConvertToPath: TAction;
    MenuItem6: TMenuItem;
    MenuItem11: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure pnlTextPaint(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
    procedure pnlTextResize(Sender: TObject);
    procedure lbLayoutsChange(Sender: TObject);
    procedure pnlTextMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure pnlTextMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure pnlTextMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure actAddLayoutExecute(Sender: TObject);
    procedure actEditLayoutExecute(Sender: TObject);
    procedure actDeleteLayoutExecute(Sender: TObject);
    procedure actClearLayoutsExecute(Sender: TObject);
    procedure actDeleteAttributeExecute(Sender: TObject);
    procedure actClearAttributesExecute(Sender: TObject);
    procedure actAddAttributeExecute(Sender: TObject);
    procedure lbLayoutAttributesChange(Sender: TObject);
    procedure actConvertToPathExecute(Sender: TObject);
  private
    FMouseDown: Boolean;
    FCaretStartPos: Integer;
    FCaretEndPos: Integer;
    procedure SetCaretStartPos(const Value: Integer);
    procedure CheckForCaretUpdate;
    procedure SetCaretEndPos(const Value: Integer);
    procedure FillLayoutAttributes(ALayout: TTextLayout);
    procedure RealignLayouts;
    function GetCarectPositionByCoords(const X, Y: Single): Integer;
    function GetRegionForRange(const ARange: TTextRange): TRegion;
  public
    property CaretStartPos: Integer read FCaretStartPos write SetCaretStartPos;
    property CaretEndPos: Integer read FCaretEndPos write SetCaretEndPos;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  System.Math, fmAddEditLayout, fmAddEditLayoutAttribute, fmLayoutPath;


procedure TMainForm.CheckForCaretUpdate;
begin
  if (FCaretStartPos >= 0) and (FCaretEndPos >= 0) and
     (Abs(FCaretEndPos - FCaretStartPos) >= 0) then
    pnlText.Repaint;
end;

procedure TMainForm.actAddAttributeExecute(Sender: TObject);
var
  LLayout: TTextLayout;
  LAttr: TTextAttributedRange;
begin
  with TAddEditLayoutAttributeForm.Create(Self) do
  try
    Caption := 'Add new attribute';
    LLayout := TTextLayout(lbLayouts.Selected.Data);
    SetDefaults(LLayout.Text);
    if ShowModal = mrOk then
    begin
      LAttr := TTextAttributedRange.Create(TTextRange.Create(0, 0),
        TTextAttribute.Create(TFont.Create, TAlphaColorRec.Black));
      Controls2Attribute(LAttr);
      LLayout.AddAttribute(LAttr);
      FillLayoutAttributes(LLayout);
      if lbLayouts.Count > 1 then
        RealignLayouts;
      actClearAttributes.Enabled := lbLayoutAttributes.Count > 0;
      //
      pnlText.Repaint;
    end;
  finally
    Free;
  end;
end;

procedure TMainForm.actAddLayoutExecute(Sender: TObject);
var
  LLayout: TTextLayout;
begin
  with TAddEditLayoutForm.Create(Self) do
  try
    Caption := 'Add new layout';
    SetDefaults;
    if ShowModal = mrOk then
    begin
      LLayout := TTextLayoutManager.DefaultTextLayout.Create;
      Controls2Layout(LLayout);
      LLayout.MaxSize := PointF(pnlText.ClipRect.Width, pnlText.ClipRect.Height);
      lbLayouts.Items.AddObject(LLayout.Text.Substring(0, 20), LLayout);
      if lbLayouts.Count > 1 then
        RealignLayouts;
      actClearLayouts.Enabled := lbLayouts.Count > 0;
      actConvertToPath.Enabled := lbLayouts.Count > 0;
      //
      pnlText.Repaint;
    end;
  finally
    Free;
  end;
end;

procedure TMainForm.actClearAttributesExecute(Sender: TObject);
var
  LLayout: TTextLayout;
begin
  LLayout := TTextLayout(lbLayouts.Selected.Data);
  if MessageDlg('Clear attributes in layout "' + lbLayouts.Selected.Text + '"?',
    TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes then
    begin
      LLayout.ClearAttributes;
      lbLayoutAttributes.Clear;
      if lbLayouts.Count > 1 then
        RealignLayouts;
      actClearAttributes.Enabled := lbLayoutAttributes.Count > 0;
      //
      pnlText.Repaint;
    end;
end;

procedure TMainForm.actClearLayoutsExecute(Sender: TObject);
begin
  if MessageDlg('Clear layouts?',
    TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes then
    begin
      lbLayouts.Clear;
      actClearLayouts.Enabled := lbLayouts.Count > 0;
      actConvertToPath.Enabled := lbLayouts.Count > 0;
      pnlText.Repaint;
    end;
end;

procedure TMainForm.actConvertToPathExecute(Sender: TObject);
var
  Path: TPathData;
  i: Integer;
  ShiftX, ShiftY: Single;
  TL: TTextLayout;
  R: TRectF;
begin
  with TLayoutPathFrom.Create(Self) do
  try
    Path := TPathData.Create;
    ShiftX := 0;
    ShiftY := 0;
    for i := 0 to lbLayouts.Count - 1 do
    begin
      TL := TTextLayout(lbLayouts.ItemByIndex(i).Data);
      ShiftX := ShiftX + TL.Padding.Left + TL.TopLeft.X;
      ShiftY := ShiftY + TL.Padding.Top + TL.TopLeft.Y;
      TL.ConvertToPath(Path);
      R := Path.GetBounds;
      Path.Translate(ShiftX, -ShiftY - R.Top - R.Bottom);
      ShiftY := ShiftY + TL.Padding.Bottom;
    end;
    SetPath(Path);
    FreeAndNil(Path);
    ShowModal;
  finally
    Free;
  end;
end;

procedure TMainForm.actDeleteAttributeExecute(Sender: TObject);
var
  LLayout: TTextLayout;
  LText: string;
  Index: Integer;
begin
  LLayout := TTextLayout(lbLayouts.Selected.Data);
  Index := lbLayoutAttributes.ItemIndex;
  LText := Format('From %d to %d', [
    LLayout.Attributes[Index].Range.Pos,
    LLayout.Attributes[Index].Range.Pos +
    LLayout.Attributes[Index].Range.Length]);
  if MessageDlg('Delete attribute "' + LText + '"?',
    TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes then
    begin
      LLayout.DeleteAttribute(Index);
      if lbLayouts.Count > 1 then
        RealignLayouts;
      lbLayoutAttributes.Items.Delete(Index);
      actClearAttributes.Enabled := lbLayoutAttributes.Count > 0;
      //
      pnlText.Repaint;
    end;
end;

procedure TMainForm.actDeleteLayoutExecute(Sender: TObject);
begin
  if MessageDlg('Delete layout"' + lbLayouts.Selected.Text + '"?',
    TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes then
    begin
      lbLayouts.Items.Delete(lbLayouts.ItemIndex);
      actClearLayouts.Enabled := lbLayouts.Count > 0;
      actConvertToPath.Enabled := lbLayouts.Count > 0;
      RealignLayouts;
      pnlText.Repaint;
    end;
end;

procedure TMainForm.actEditLayoutExecute(Sender: TObject);
var
  LLayout: TTextLayout;
begin
  with TAddEditLayoutForm.Create(Self) do
  try
    Caption := 'Edit existing layout';
    LLayout := TTextLayout(lbLayouts.Selected.Data);
    Layout2Controls(LLayout);
    if ShowModal = mrOk then
    begin
      Controls2Layout(LLayout);
      lbLayouts.Selected.Text := LLayout.Text.Substring(0, 20);
      if lbLayouts.Count > 1 then
        RealignLayouts;
      //
      pnlText.Repaint;
    end;
  finally
    Free;
  end;
end;

procedure TMainForm.FillLayoutAttributes(ALayout: TTextLayout);
var
  i: Integer;
  LText: string;
begin
  lbLayoutAttributes.Clear;
  lbLayoutAttributes.ItemIndex := -1;
  if Assigned(ALayout) then
    for i := 0 to ALayout.AttributesCount - 1 do
    begin
      LText := Format('From %d to %d', [
        ALayout.Attributes[i].Range.Pos,
        ALayout.Attributes[i].Range.Pos + ALayout.Attributes[i].Range.Length]);
      lbLayoutAttributes.Items.Add(LText);
    end;
  actAddAttribute.Enabled := Assigned(ALayout);
  actClearAttributes.Enabled := lbLayoutAttributes.Count > 0;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FCaretStartPos := -1;
  FCaretEndPos := -1;
end;

function TMainForm.GetCarectPositionByCoords(const X, Y: Single): Integer;
var
  i, LPos, LShift: Integer;
  LPoint: TPointF;
  LastPoint: TPointF;
  Layout: TTextLayout;
begin
  Result := -1;
  LPoint := PointF(X, Y);
  LShift := 0;
  for i := 0 to lbLayouts.Count - 1 do
  begin
    LPos := TTextLayout(lbLayouts.ItemByIndex(i).Data).PositionAtPoint(LPoint);
    if LPos >= 0 then
    begin
      Result := LShift + LPos;
      Break;
    end;
    Inc(LShift, TTextLayout(lbLayouts.ItemByIndex(i).Data).Text.Length);
  end;
  if (Result = -1) and (lbLayouts.Count > 0) then
  begin
    Layout := TTextLayout(lbLayouts.ItemByIndex(lbLayouts.Count - 1).Data);
    LastPoint :=  Layout.TopLeft;
    LastPoint.Offset(Layout.Width, Layout.Height);
    if LPoint.Y >= LastPoint.Y then
    begin
      if LPoint.X >= LastPoint.X then
        Result := LShift
      else
      begin
        LPoint.Y := Layout.TopLeft.Y +Layout.Height / 2;
        LPos := Layout.PositionAtPoint(LPoint);
        if LPos >= 0 then
          Result := LPos + (LShift - Layout.Text.Length);
      end;
    end;
  end;
end;

function TMainForm.GetRegionForRange(const ARange: TTextRange): TRegion;
var
  i, j: Integer;
  LPos, LStart, RemainLength, LLength: Integer;
  LLayout: TTextLayout;
  LRegion: TRegion;
begin
  SetLength(Result, 0);
  LStart := 0;
  LPos := ARange.Pos;
  RemainLength := ARange.Length;
  for i := 0 to lbLayouts.Count - 1 do
  begin
    LLayout := TTextLayout(lbLayouts.ItemByIndex(i).Data);
    //Checking layout for contains a part of requested range
    if (LPos >= LStart) and (LPos < (LStart + LLayout.Text.Length)) then
    begin
      LLength := Min(RemainLength, LStart + LLayout.Text.Length - LPos);
      LRegion := LLayout.RegionForRange(TTextRange.Create(LPos - LStart, LLength));
      for j := 0 to High(LRegion) do
      begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := LRegion[j];
      end;
      Inc(LPos, LLength);
      Dec(RemainLength, LLength);
      if RemainLength = 0 then
        Break;
    end;
    Inc(LStart, LLayout.Text.Length);
  end;
end;

procedure TMainForm.lbLayoutAttributesChange(Sender: TObject);
begin
  actDeleteAttribute.Enabled := Assigned(lbLayoutAttributes.Selected);
  actClearAttributes.Enabled := lbLayoutAttributes.Count > 0;
end;

procedure TMainForm.lbLayoutsChange(Sender: TObject);
begin
  actEditLayout.Enabled := Assigned(lbLayouts.Selected);
  actDeleteLayout.Enabled := Assigned(lbLayouts.Selected);
  actClearLayouts.Enabled := lbLayouts.Count > 0;
  actConvertToPath.Enabled := lbLayouts.Count > 0;
  if Assigned(lbLayouts.Selected) then
    FillLayoutAttributes(TTextLayout(lbLayouts.Selected.Data));
end;

procedure TMainForm.pnlTextMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  //Determining caret position by point
  FMouseDown := True;
  CaretStartPos := GetCarectPositionByCoords(X, Y);
  CaretEndPos := FCaretStartPos;
end;

procedure TMainForm.pnlTextMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  if FMouseDown then
    CaretEndPos := GetCarectPositionByCoords(X, Y);
end;

procedure TMainForm.pnlTextMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  CaretEndPos := GetCarectPositionByCoords(X, Y);
  FMouseDown := False;
end;

procedure TMainForm.pnlTextPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
var
  LState: TCanvasSaveState;
  LRange: TTextRange;
  LRegion: TRegion;
  i: Integer;
begin
  for i := 0 to lbLayouts.Count - 1 do
    TTextLayout(lbLayouts.ItemByIndex(i).Data).RenderLayout(Self.Canvas);
  if (FCaretStartPos >= 0) and (FCaretEndPos >= 0) and
     (Abs(FCaretEndPos - FCaretStartPos) > 0) then
  begin
    LState := pnlText.Canvas.SaveState;
    pnlText.Canvas.Fill.Color := TAlphaColorRec.Blue;
    LRange.Pos := Min(FCaretStartPos, FCaretEndPos);
    LRange.Length := Abs(FCaretEndPos - FCaretStartPos);
    LRegion := GetRegionForRange(LRange);
    for i := 0 to High(LRegion) do
      pnlText.Canvas.FillRect(LRegion[i], 0, 0, [], 0.5);
    pnlText.Canvas.RestoreState(LState);
  end;
end;

procedure TMainForm.pnlTextResize(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to lbLayouts.Count - 1 do
    TTextLayout(lbLayouts.ItemByIndex(i).Data).MaxSize :=
      PointF(pnlText.ClipRect.Width, pnlText.ClipRect.Height);
end;

procedure TMainForm.RealignLayouts;
var
  i: Integer;
  LPoint: TPointF;
  LLayout: TTextLayout;
begin
  LPoint := PointF(0, 0);
  for i := 0 to lbLayouts.Count - 1 do
  begin
    LLayout := TTextLayout(lbLayouts.ItemByIndex(i).Data);
    LLayout.TopLeft := LPoint;
    LPoint.Offset(0, LLayout.Height);
  end;
end;

procedure TMainForm.SetCaretEndPos(const Value: Integer);
begin
  if FCaretEndPos <> Value then
  begin
    FCaretEndPos := Value;
    CheckForCaretUpdate;
  end;
end;

procedure TMainForm.SetCaretStartPos(const Value: Integer);
begin
  if FCaretStartPos <> Value then
  begin
    FCaretStartPos := Value;
    CheckForCaretUpdate;
  end;
end;

end.
