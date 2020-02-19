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
unit UnitMain;

interface

uses
  System.SysUtils, System.Actions, System.Rtti, System.Types, System.RTLConsts, System.ImageList, System.TypInfo,
  System.UITypes, System.Classes, System.Variants, System.Math, FMX.Types, FMX.Consts, FMX.Controls, FMX.Forms,
  FMX.Graphics, FMX.Dialogs, FMX.Grid.Style, FMX.ActnList, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Grid,
  FMX.TabControl, FMX.Layouts, FMX.ImgList, FMX.StdCtrls, FMX.TreeView, FMX.Effects, FMX.StdActns,
  Data.Bind.Controls, Fmx.Bind.Navigator, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf, Data.Bind.EngExt, Fmx.Bind.DBEngExt,
  Fmx.Bind.Grid, System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.Components, Data.Bind.Grid, Data.Bind.DBScope,
  Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client, FMX.Memo, FMX.Objects, FMX.DateTimeCtrls;

type
  /// <summary> The demonstration project to show the features of TGrid component in <b>Fire Monkey</b> </summary>
  TMainForm = class(TForm)
    TabControl1: TTabControl;
    Layout1: TLayout;
    TabItem1: TTabItem;
    StringGrid1: TStringGrid;
    ActionList1: TActionList;
    actnControlType: TControlAction;
    StringColumn1: TStringColumn;
    Column1: TColumn;
    ProgressColumn1: TProgressColumn;
    CurrencyColumn1: TCurrencyColumn;
    CheckColumn1: TCheckColumn;
    DateColumn1: TDateColumn;
    TimeColumn1: TTimeColumn;
    PopupColumn1: TPopupColumn;
    GlyphColumn1: TGlyphColumn;
    ImageList1: TImageList;
    CheckBox1: TCheckBox;
    actnVisibleColumn: TControlAction;
    CheckBox2: TCheckBox;
    actnWidthColumn: TControlAction;
    CheckBox3: TCheckBox;
    TreeView1: TTreeView;
    Splitter1: TSplitter;
    VertScrollBox1: TVertScrollBox;
    actnEnableColumn: TControlAction;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    actnReadOnlyColumn: TControlAction;
    TabItem2: TTabItem;
    actnOptions: TControlAction;
    SpeedButton1: TSpeedButton;
    Grid1: TGrid;
    TabItem3: TTabItem;
    Grid2: TGrid;
    BindNavigator1: TBindNavigator;
    FDMemTable1: TFDMemTable;
    FDMemTable1Index: TIntegerField;
    FDMemTable1ImageIndex: TIntegerField;
    FDMemTable1String: TStringField;
    FDMemTable1Column: TStringField;
    FDMemTable1CheckBox: TBooleanField;
    FDMemTable1Date: TDateField;
    FDMemTable1Time: TTimeField;
    FDMemTable1Currency: TCurrencyField;
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkGridToDataSourceBindSourceDB1: TLinkGridToDataSource;
    Splitter2: TSplitter;
    Memo1: TMemo;
    procedure actnControlTypeUpdate(Sender: TObject);
    procedure actnControlTypeExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actnVisibleColumnUpdate(Sender: TObject);
    procedure actnVisibleColumnExecute(Sender: TObject);
    procedure actnWidthColumnUpdate(Sender: TObject);
    procedure actnWidthColumnExecute(Sender: TObject);
    procedure TreeView1ChangeCheck(Sender: TObject);
    procedure VertScrollBox1Resize(Sender: TObject);
    procedure actnEnableColumnUpdate(Sender: TObject);
    procedure actnEnableColumnExecute(Sender: TObject);
    procedure actnReadOnlyColumnUpdate(Sender: TObject);
    procedure actnReadOnlyColumnExecute(Sender: TObject);
    procedure actnOptionsUpdate(Sender: TObject);
    procedure actnOptionsExecute(Sender: TObject);
    procedure Grid1GetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
    procedure FormDestroy(Sender: TObject);
    procedure Grid1SetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
    procedure HeaderClickProc(Column: TColumn);
    procedure SelectCellProc(Sender: TObject; const ACol, ARow: Integer; var CanSelect: Boolean);
    procedure SelChangedProc(Sender: TObject);
    procedure DrawColumnCellProc(Sender: TObject; const Canvas: TCanvas; const Column: TColumn;
      const Bounds: TRectF; const Row: Integer; const Value: TValue; const State: TGridDrawStates);
    procedure DrawColumnBackgroundProc(Sender: TObject; const Canvas: TCanvas; const Column: TColumn;
      const Bounds: TRectF; const Row: Integer; const Value: TValue; const State: TGridDrawStates);
    procedure DrawColumnHeaderProc(Sender: TObject; const Canvas: TCanvas; const Column: TColumn;
      const Bounds: TRectF);
    procedure ColumnMovedProc(Column: TColumn; FromIndex, ToIndex: Integer);
    procedure EditingDoneProc(Sender: TObject; const ACol, ARow: Integer);
    procedure ResizeProc(Sender: TObject);
    procedure TapProc(Sender: TObject; const Point: TPointF);
  private
    { Private declarations }
    FItemGridOptions: TTreeViewItem;
    FItemCellReturn: TTreeViewItem;
    FDefaultDrawing: TTreeViewItem;
    FTreeUpdating: Boolean;
    FBitmaps: array of TBitmap;
    FStrings: array of string;
    FPopups: array of string;
    FDates: array of TDateTime;
    FTimes: array of TDateTime;
    procedure UpdateGridByTreeView;
    procedure UpdateTreeView;
    procedure PropogateOptions;
    procedure MemoAddLine(const EventName, Text: string);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

procedure TMainForm.FormCreate(Sender: TObject);

  procedure InitStringValue(const ColumnIndex: Integer; var RowIndex: Integer; const Caption, Value: string);
  begin
    StringGrid1.Cells[2, RowIndex] := Caption;
    StringGrid1.Cells[3, RowIndex] := '''' + Value + '''';
    StringGrid1.Cells[ColumnIndex, RowIndex] := Value;
    Inc(RowIndex);
  end;

var
  I: Integer;
  Item: TTreeViewItem;
  Column: TColumn;
begin
  ImageList1.CacheSize := 20;
  VertScrollBox1.Visible := False;
  {$REGION TreeView updating}
  FItemGridOptions := TTreeViewItem.Create(Self);
  FItemGridOptions.Text := 'TCustomGrid.Options';
  TreeView1.AddObject(FItemGridOptions);
  for I := Integer(Low(TGridOption)) to Integer(High(TGridOption)) do
  begin
    Item := TTreeViewItem.Create(Self);
    Item.Text := GetEnumName(TypeInfo(TGridOption), I);
    FItemGridOptions.AddObject(Item);
  end;
  FItemGridOptions.Expand;
  FItemCellReturn := TTreeViewItem.Create(Self);
  FItemCellReturn.Text := 'TGridModel.CellReturnAction';
  TreeView1.AddObject(FItemCellReturn);
  for I := Integer(Low(TCellReturnAction)) to Integer(High(TCellReturnAction)) do
  begin
    Item := TTreeViewItem.Create(Self);
    Item.Text := GetEnumName(TypeInfo(TCellReturnAction), I);
    FItemCellReturn.AddObject(Item);
  end;
  FItemCellReturn.Expand;
  FDefaultDrawing := TTreeViewItem.Create(Self);
  FDefaultDrawing.Text := 'TCustomGrid.DefaultDrawing';
  TreeView1.AddObject(FDefaultDrawing);
  UpdateTreeView;
  {$ENDREGION}
  {$REGION Grid updating}
  Grid1.RowCount := StringGrid1.RowCount;
  for I := 0 to StringGrid1.ColumnCount - 1 do
  begin
    Grid1.AddObject(StringGrid1.Columns[I].Clone(Self));
    if Grid1.Columns[Grid1.ColumnCount - 1] is TProgressColumn then
      TProgressColumn(Grid1.Columns[Grid1.ColumnCount - 1]).Max := Grid1.RowCount;
  end;
  FDMemTable1.Open;
  for I := 0 to Min(Grid1.ColumnCount, Grid2.ColumnCount) - 1 do
  begin
    Column := Grid2.Columns[I];
    Column.Header := StringGrid1.Columns[I].Header;
    Column.Width := StringGrid1.Columns[I].Width;
    if Column is TProgressColumn then
      TProgressColumn(Column).Max := Grid2.RowCount;
  end;

  Column := TImageColumn.Create(Self);
  Column.Header := Column.ClassName;
  Grid1.AddObject(Column);
  SetLength(FBitmaps, Grid1.RowCount);
  for I := Low(FBitmaps) to High(FBitmaps) do
    FBitmaps[I] := nil;
  SetLength(FStrings, Grid1.RowCount);
  SetLength(FPopups, Grid1.RowCount);
  SetLength(FDates, Grid1.RowCount);
  SetLength(FTimes, Grid1.RowCount);
  for I := 0 to Grid1.RowCount - 1 do
  begin
    FDates[I] := NaN;
    FTimes[I] := NaN;
  end;
  {$ENDREGION}
  {$REGION String grid updating}
  for I := 0 to StringGrid1.RowCount - 1 do
  begin
    StringGrid1.Cells[0, I] := FloatToStr(I + 1 + (Random * 0.1));
    StringGrid1.Cells[1, I] := IntToStr(I mod ImageList1.Count);
    StringGrid1.Cells[2, I] := 'Row # ' + I.ToString;
  end;
  I := 1;
  InitStringValue(4, I, 'Progress', '');
  InitStringValue(4, I, 'Progress', '0');
  InitStringValue(4, I, 'Progress Integer', '100');
  InitStringValue(4, I, 'Progress Float', FloatToStr(333.33));
  InitStringValue(4, I, 'Progress 50%', '500');
  InitStringValue(4, I, 'Progress 100%', '1000');
  InitStringValue(4, I, 'Progress more than 100%', '10000');
  InitStringValue(4, I, 'Progress less than 0%', '-1');
  InitStringValue(4, I, 'Progress error value', 'Error');

  InitStringValue(5, I, 'Check column', '');
  InitStringValue(5, I, 'Check column', 't');
  InitStringValue(5, I, 'Check column', 'true');
  InitStringValue(5, I, 'Check column', 'False');
  InitStringValue(5, I, 'Check column', 'y');
  InitStringValue(5, I, 'Check column', 'N');
  InitStringValue(5, I, 'Check column', 'F');
  InitStringValue(5, I, 'Check column', '0');
  InitStringValue(5, I, 'Check column', '1');
  InitStringValue(5, I, 'Check column', '-1');

  InitStringValue(6, I, 'Date and Time', FormatDateTime(DateColumn1.Format + ' ' + FormatSettings.ShortTimeFormat,
    EncodeDate(1961, 04, 12) + EncodeTime(09, 07, 5, 333)));
  InitStringValue(6, I, 'Date', DateToStr(EncodeDate(1957, 10, 04)));
  InitStringValue(6, I, 'Long Date Format', FormatDateTime(FormatSettings.LongDateFormat, EncodeDate(1988, 11, 15)));
  InitStringValue(6, I, 'Error Date', 'Error');

  InitStringValue(7, I, 'Date and Time', DateTimeToStr(EncodeDate(1961, 04, 12) + EncodeTime(09, 07, 5, 333)));
  InitStringValue(7, I, 'Time', TimeToStr(EncodeTime(12, 45, 33, 777)));
  InitStringValue(7, I, 'Long Time Format', FormatDateTime(FormatSettings.LongTimeFormat, EncodeTime(18, 15, 22, 555)));
  InitStringValue(7, I, 'Error Time', 'Error');

  InitStringValue(8, I, 'Popup by Index', '1');
  InitStringValue(8, I, 'Popup by Text', 'interesting text');
  InitStringValue(8, I, 'Popup by Text', 'text');
  {$ENDREGION}
  if Grid2.Columns[0] is TCurrencyColumn then
    TCurrencyColumn(Grid2.Columns[0]).DecimalDigits := 0;
  FDMemTable1.DisableControls;
  for I := 0 to 200 do
  begin
    FDMemTable1.Append;
    FDMemTable1Index.AsInteger := I + 1;
    FDMemTable1ImageIndex.AsInteger := I mod ImageList1.Count;
    FDMemTable1String.AsString := 'Row # ' + IntToStr(I);
  end;
  FDMemTable1.First;
  FDMemTable1Index.ReadOnly := True;
  FDMemTable1.EnableControls;
  PropogateOptions;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to Length(FBitmaps) - 1 do
    FreeAndNil(FBitmaps[I]);
end;

procedure TMainForm.Grid1GetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
var
  ColumnClass: TColumnClass;
begin
  ColumnClass := TColumnClass(Grid1.Columns[ACol].ClassType);
  if ColumnClass = TImageColumn then
    Value := FBitmaps[ARow]
  else if ColumnClass = TColumn then
  begin
    if FBitmaps[ARow] = nil then
      Value := 'nil'
    else if FBitmaps[ARow].IsEmpty then
      Value := SEmpty
    else
      Value := Format('Width: %d; Height: %d', [FBitmaps[ARow].Width, FBitmaps[ARow].Height]);
  end
  else if ColumnClass = TCheckColumn then
  begin
    if FBitmaps[ARow] = nil then
      Value := TValue.Empty
    else if FBitmaps[ARow].IsEmpty then
      Value := False
    else
      Value := True;
  end
  else if ColumnClass = TCurrencyColumn then
    Value := ARow + 1 + (Random * 0.1)
  else if ColumnClass = TGlyphColumn then
    Value := ARow mod ImageList1.Count
  else if ColumnClass = TStringColumn then
    Value := FStrings[ARow]
  else if ColumnClass = TPopupColumn then
    Value := FPopups[ARow]
  else if ColumnClass = TProgressColumn then
    Value := ARow
  else if ColumnClass = TDateColumn then
  begin
    if IsNan(FDates[ARow]) then
      Value := TValue.Empty
    else
      Value := FDates[ARow]
  end
  else if ColumnClass = TTimeColumn then
  begin
    if IsNan(FTimes[ARow]) then
      Value := TValue.Empty
    else
      Value := FTimes[ARow];
  end;
end;

procedure TMainForm.Grid1SetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
var
  ColumnClass: TColumnClass;
begin
  ColumnClass := TColumnClass(Grid1.Columns[ACol].ClassType);
  if ColumnClass = TImageColumn then
  begin
    if not Value.IsEmpty and Value.IsObject and (Value.AsObject is TBitmap) then
    begin
      if FBitmaps[ARow] = nil then
        FBitmaps[ARow] := TBitmap.Create;
      FBitmaps[ARow].Assign(TBitmap(Value.AsObject));
    end;
  end
  else if ColumnClass = TStringColumn then
    FStrings[ARow] := Value.ToString
  else if ColumnClass = TPopupColumn then
    FPopups[ARow] := Value.ToString
  else if ColumnClass = TDateColumn then
  begin
    if Value.IsEmpty or not Value.TryAsType<TDateTime>(FDates[ARow]) then
      FDates[ARow] := NaN;
  end
  else if ColumnClass = TTimeColumn then
  begin
    if Value.IsEmpty or not Value.TryAsType<TDateTime>(FTimes[ARow]) then
      FTimes[ARow] := NaN;
  end;
end;

procedure TMainForm.UpdateTreeView;
var
  S: string;
  I: Integer;
begin
  if not FTreeUpdating then
  begin
    FTreeUpdating := True;
    try
      for I := 0 to FItemGridOptions.Count - 1 do
      begin
        try
          S := FItemGridOptions.Items[I].Text;
          FItemGridOptions.Items[I].Enabled := True;
          FItemGridOptions.Items[I].IsChecked := TGridOption(GetEnumValue(TypeInfo(TGridOption), S))
            in StringGrid1.Options;
        except
          FItemGridOptions.Items[I].Enabled := False;
          FItemGridOptions.Items[I].IsChecked := False;
        end;
      end;
      FItemGridOptions.IsChecked := (StringGrid1.Options <> TGridModel.DefaultGridOptions);
      for I := 0 to FItemCellReturn.Count - 1 do
      begin
        try
          S := FItemCellReturn.Items[I].Text;
          FItemCellReturn.Items[I].Enabled := True;
          FItemCellReturn.Items[I].IsChecked := TCellReturnAction(GetEnumValue(TypeInfo(TCellReturnAction), S))
            = StringGrid1.Model.CellReturnAction;
        except
          FItemCellReturn.Items[I].Enabled := False;
          FItemCellReturn.Items[I].IsChecked := False;
        end;
      end;
      FItemCellReturn.IsChecked := (StringGrid1.Model.CellReturnAction <> TCellReturnAction.None);
      FDefaultDrawing.IsChecked := StringGrid1.DefaultDrawing;
    finally
      FTreeUpdating := False;
    end;
  end;
end;

procedure TMainForm.UpdateGridByTreeView;
var
  S: string;
  Options: TGridOptions;
  ReturnAction: TCellReturnAction;
  I: Integer;
begin
  if not FTreeUpdating then
  begin
    Options := [];
    for I := 0 to FItemGridOptions.Count - 1 do
    begin
      if FItemGridOptions.Items[I].IsChecked then
      begin
        S := FItemGridOptions.Items[I].Text;
        Options := Options + [TGridOption(GetEnumValue(TypeInfo(TGridOption), S))];
      end;
    end;
    FItemGridOptions.IsChecked := (Options <> TGridModel.DefaultGridOptions);
    ReturnAction := TCellReturnAction.None;
    for I := 0 to FItemCellReturn.Count - 1 do
    begin
      if FItemCellReturn.Items[I].IsChecked then
      begin
        S := FItemCellReturn.Items[I].Text;
        ReturnAction := TCellReturnAction(GetEnumValue(TypeInfo(TCellReturnAction), S));
      end;
    end;
    FItemCellReturn.IsChecked := (ReturnAction <> TCellReturnAction.None);

    StringGrid1.Options := Options;
    StringGrid1.Model.CellReturnAction := ReturnAction;
    StringGrid1.DefaultDrawing := FDefaultDrawing.IsChecked;
  end;
  UpdateTreeView;
  PropogateOptions;
end;

procedure TMainForm.PropogateOptions;
  procedure InternalPropogate(const Grid: TGrid);
  var
    I: Integer;
  begin
    Grid.Options := StringGrid1.Options;
    Grid.Model.CellReturnAction := StringGrid1.Model.CellReturnAction;
    Grid.ControlType := StringGrid1.ControlType;
    for I := 0 to Min(Grid.ColumnCount, StringGrid1.ColumnCount) - 1 do
    begin
      Grid.Columns[I].Visible := StringGrid1.Columns[I].Visible;
      Grid.Columns[I].Width := StringGrid1.Columns[I].Width;
      Grid.Columns[I].Enabled := StringGrid1.Columns[I].Enabled;
      Grid.Columns[I].ReadOnly := StringGrid1.Columns[I].ReadOnly;
    end;
    Grid.DefaultDrawing := StringGrid1.DefaultDrawing;
    Grid.OnHeaderClick := StringGrid1.OnHeaderClick;
    Grid.OnSelChanged := StringGrid1.OnSelChanged;
    Grid.OnSelectCell := StringGrid1.OnSelectCell;
    Grid.OnDrawColumnCell := StringGrid1.OnDrawColumnCell;
    Grid.OnDrawColumnBackground := StringGrid1.OnDrawColumnBackground;
    Grid.OnDrawColumnHeader := StringGrid1.OnDrawColumnHeader;
    Grid.OnColumnMoved := StringGrid1.OnColumnMoved;
    Grid.OnEditingDone := StringGrid1.OnEditingDone;
    Grid.OnResize := StringGrid1.OnResize;
    Grid.OnTap := StringGrid1.OnTap;
  end;
begin
  InternalPropogate(Grid1);
  InternalPropogate(Grid2);
end;

procedure TMainForm.DrawColumnHeaderProc(Sender: TObject; const Canvas: TCanvas; const Column: TColumn;
  const Bounds: TRectF);
var
  OldKind: TBrushKind;
  OldColor: TAlphaColor;
  R: TRectF;
begin
  if (Sender is TCustomGrid) and (Column is TStringColumn) then
  begin
    OldKind := Canvas.Stroke.Kind;
    OldColor := Canvas.Stroke.Color;
    Canvas.Stroke.Kind := TBrushKind.Solid;
    R := Bounds;
    R.Inflate(-1 / 2 * Canvas.Scale, -1 / 2 * Canvas.Scale);
    Canvas.Stroke.Color := TAlphaColorRec.Lawngreen;
    Canvas.DrawRect(R, 0, 0, AllCorners, 1);
    Canvas.Stroke.Kind := OldKind;
    Canvas.Stroke.Color := OldColor;
  end;
end;

procedure TMainForm.DrawColumnBackgroundProc(Sender: TObject; const Canvas: TCanvas; const Column: TColumn;
  const Bounds: TRectF; const Row: Integer; const Value: TValue; const State: TGridDrawStates);
var
  OldKind: TBrushKind;
  OldColor: TAlphaColor;
  R: TRectF;
begin
  if (Sender is TCustomGrid) and (Column is TStringColumn) then
  begin
    OldKind := Canvas.Fill.Kind;
    OldColor := Canvas.Fill.Color;
    R := Bounds;
    R.Inflate(-1, -1);
    if TGridDrawState.Focused in State then
      Canvas.Fill.Color := TAlphaColorRec.Yellow
    else
      Canvas.Fill.Color := TAlphaColorRec.Antiquewhite;
    Canvas.Fill.Kind := TBrushKind.Solid;
    Canvas.FillRect(R, 0, 0, AllCorners, 1);
    Canvas.Fill.Color := OldColor;
    Canvas.Fill.Kind := OldKind;
  end;
end;

procedure TMainForm.DrawColumnCellProc(Sender: TObject; const Canvas: TCanvas; const Column: TColumn;
  const Bounds: TRectF; const Row: Integer; const Value: TValue; const State: TGridDrawStates);
var
  S: TGridDrawState;
  R: TRectF;
  OldKind: TBrushKind;
begin
  if (Sender is TCustomGrid) and (Column is TStringColumn) then
  begin
    OldKind := Canvas.Stroke.Kind;
    Canvas.Stroke.Kind := TBrushKind.Solid;
    R := Bounds;
    R.Inflate(1 / 2 * Canvas.Scale, 1 / 2 * Canvas.Scale);
    Canvas.Stroke.Color := TAlphaColorRec.Aqua;
    Canvas.DrawRect(R, 0, 0, AllCorners, 1);
    Canvas.Stroke.Kind := OldKind;
    OldKind := Canvas.Fill.Kind;
    for S := Low(TGridDrawState) to High(TGridDrawState) do
      if S in State then
      begin
        R := TRectF.Create(TPointF.Create(Bounds.Left + Integer(S) * 3, Bounds.Top - 1), 3, 3);
        case s of
          TGridDrawState.Selected: Canvas.Fill.Color := TAlphaColorRec.Red;
          TGridDrawState.Focused: Canvas.Fill.Color := TAlphaColorRec.Green;
          TGridDrawState.RowSelected: Canvas.Fill.Color := TAlphaColorRec.Blue;
        end;
        Canvas.Fill.Kind := TBrushKind.Solid;
        Canvas.FillRect(R, 0, 0, AllCorners, 1);
      end;
    Canvas.Fill.Kind := OldKind;
  end;
end;

procedure TMainForm.MemoAddLine(const EventName, Text: string);
begin
  Memo1.Lines.Add(Format('%s(%s)', [EventName, Text]));
  Memo1.SelStart := Length(Memo1.Text) - 1;
  Memo1.SelLength := 0;
end;

procedure TMainForm.SelChangedProc(Sender: TObject);
var
  S: string;
begin
  if Sender is TControl then
    S := TControl(Sender).Name
  else
    S := '';
  if Sender is TCustomGrid then
    S := Format('[%s] %d; %d', [S, TCustomGrid(Sender).Col, TCustomGrid(Sender).Row]);
  MemoAddLine('OnSelChanged', S);
end;

procedure TMainForm.SelectCellProc(Sender: TObject; const ACol, ARow: Integer; var CanSelect: Boolean);
var
  S: string;
begin
  if Sender is TControl then
    S := TControl(Sender).Name
  else
    S := '';
  S := Format('[%s] %d; %d', [S, ACol, ARow]);
  if (Sender is TCustomGrid) and (ACol >= 0) and (TCustomGrid(Sender).Columns[ACol] is TGlyphColumn) then
  begin
    S := S + ' CanSelect = False';
    CanSelect := False;
  end;
  MemoAddLine('OnSelectCell', S);
end;

procedure TMainForm.TapProc(Sender: TObject; const Point: TPointF);
var
  S: string;
  LocalPoint: TPointF;
  Col, Row: Integer;
begin
  if Sender is TControl then
    S := TControl(Sender).Name
  else
    S := '';
  if Sender is TCustomGrid then
  begin
    S := Format('[%s] %d; %d', [S, Round(Point.X), Round(Point.Y)]);
    LocalPoint := TCustomGrid(Sender).AbsoluteToLocal(Point);
    if TCustomGrid(Sender).CellByPoint(LocalPoint.X, LocalPoint.Y, Col, Row) then
      S := S + Format(' Col: %d; Row: %d', [Col, Row])
    else
      S := S + ' Col: ?; Row: ?'
  end;
  MemoAddLine('OnTap', S);
end;

procedure TMainForm.ResizeProc(Sender: TObject);
var
  S: string;
begin
  if Sender is TControl then
    S := TControl(Sender).Name
  else
    S := '';
  MemoAddLine('OnResize', S);
end;

procedure TMainForm.EditingDoneProc(Sender: TObject; const ACol, ARow: Integer);
var
  S: string;
begin
  if Sender is TControl then
    S := TControl(Sender).Name
  else
    S := '';
  S := Format('[%s] %d; %d', [S, ACol, ARow]);
  MemoAddLine('OnEditingDone', S);
end;

procedure TMainForm.ColumnMovedProc(Column: TColumn; FromIndex, ToIndex: Integer);
var
  S: string;
begin
  if Column <> nil then
  begin
    S := Column.Header;
    if Column.PresentedControl <> nil then
      S := '[' + Column.PresentedControl.Name + '] ' + S;
  end
  else
    S := 'nil';
  S := Format('%s From: %d to %d', [S, FromIndex, ToIndex]);
  MemoAddLine('OnColumnMoved', S);
end;

procedure TMainForm.HeaderClickProc(Column: TColumn);
var
  S: string;
begin
  if Column <> nil then
  begin
    S := Column.Header;
    if Column.PresentedControl <> nil then
      S := '[' + Column.PresentedControl.Name + '] ' + S;
  end
  else
    S := 'nil';
  MemoAddLine('OnHeaderClick', S);
end;

procedure TMainForm.VertScrollBox1Resize(Sender: TObject);
begin
  if VertScrollBox1.Width <= Splitter1.MinSize then
  begin
    VertScrollBox1.Visible := False;
    Splitter1.Visible := False;
  end;
end;

procedure TMainForm.TreeView1ChangeCheck(Sender: TObject);
var
  I: Integer;
begin
  if not FTreeUpdating then
  begin
    FTreeUpdating := True;
    try
      for I := 0 to FItemCellReturn.Count - 1 do
      begin
        if FItemCellReturn.Items[I] <> Sender then
          FItemCellReturn.Items[I].IsChecked := False;
      end;
    finally
      FTreeUpdating := False;
    end;
  end;
  UpdateGridByTreeView;
end;

procedure TMainForm.actnControlTypeExecute(Sender: TObject);
var
  NewControlType: TControlType;
begin
  if StringGrid1.ControlType <> TControlType.Platform then
    NewControlType := TControlType.Platform
  else
    NewControlType := TControlType.Styled;
  StringGrid1.ControlType := NewControlType;
  PropogateOptions;
end;

procedure TMainForm.actnControlTypeUpdate(Sender: TObject);
begin
  actnControlType.Checked := (StringGrid1.ControlType = TControlType.Platform);
end;

procedure TMainForm.actnEnableColumnExecute(Sender: TObject);
begin
  StringGrid1.Columns[2].Enabled := not StringGrid1.Columns[2].Enabled;
  PropogateOptions;
end;

procedure TMainForm.actnEnableColumnUpdate(Sender: TObject);
begin
  actnEnableColumn.Checked := StringGrid1.Columns[2].Enabled;
end;

procedure TMainForm.actnOptionsExecute(Sender: TObject);
begin
  if not VertScrollBox1.Visible then
  begin
    Splitter1.Position.X := Width - VertScrollBox1.Width - Splitter1.Width;
    UpdateTreeView;
  end;
  VertScrollBox1.Visible := not VertScrollBox1.Visible;
  Splitter1.Visible := VertScrollBox1.Visible;
end;

procedure TMainForm.actnOptionsUpdate(Sender: TObject);
begin
  actnOptions.Checked := VertScrollBox1.Visible;
end;

procedure TMainForm.actnReadOnlyColumnExecute(Sender: TObject);
begin
  StringGrid1.Columns[2].ReadOnly := not StringGrid1.Columns[2].ReadOnly;
  PropogateOptions;
end;

procedure TMainForm.actnReadOnlyColumnUpdate(Sender: TObject);
begin
  actnReadOnlyColumn.Checked := StringGrid1.Columns[2].ReadOnly;
end;

procedure TMainForm.actnVisibleColumnExecute(Sender: TObject);
begin
  StringGrid1.Columns[1].Visible := not StringGrid1.Columns[1].Visible;
  PropogateOptions;
end;

procedure TMainForm.actnVisibleColumnUpdate(Sender: TObject);
begin
  actnVisibleColumn.Checked := StringGrid1.Columns[1].Visible;
end;

procedure TMainForm.actnWidthColumnExecute(Sender: TObject);
begin
  if StringGrid1.Columns[1].Width = 0 then
    StringGrid1.Columns[1].Width := 32
  else
    StringGrid1.Columns[1].Width := 0;
  PropogateOptions;
end;

procedure TMainForm.actnWidthColumnUpdate(Sender: TObject);
begin
  actnWidthColumn.Checked := StringGrid1.Columns[1].Width = 0;
end;

end.
