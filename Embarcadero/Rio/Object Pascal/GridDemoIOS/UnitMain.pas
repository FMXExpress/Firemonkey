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
  System.SysUtils, System.Types, System.TypInfo, System.UITypes, FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  System.Rtti, FMX.Grid.Style, FMX.Forms, FMX.Grid, System.ImageList, FMX.ImgList, FMX.StdCtrls, System.Classes, System.Actions,
  FMX.Graphics, FMX.ActnList, FMX.Controls, FMX.ScrollBox, FMX.Memo, FMX.Layouts, FMX.TabControl, FMX.ListView, FMX.Types,
  FMX.Controls.Presentation, FMX.MultiView, Data.Bind.Controls, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, Fmx.Bind.Grid, System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.Components, Data.Bind.Grid,
  Data.Bind.DBScope, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client, Fmx.Bind.Navigator, FMX.Grid.iOS;

resourcestring
  StrHideEventLog = 'Hide event log';
  StrShowEventLog = 'Show event log';

type
  TFormMain = class(TForm)
    ListView1: TListView;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    Panel1: TPanel;
    LayoutEventLog: TLayout;
    Label1: TLabel;
    ActionList1: TActionList;
    Memo1: TMemo;
    LogButton: TSpeedButton;
    ActnEventLog: TControlAction;
    SpeedButton2: TSpeedButton;
    ActnClearEventLog: TControlAction;
    ToolBar1: TToolBar;
    OptionsButton: TSpeedButton;
    ImageList1: TImageList;
    CaptionLabel: TLabel;
    StringGrid1: TStringGrid;
    Grid1: TGrid;
    Grid2: TGrid;
    CurrencyColumn1: TCurrencyColumn;
    GlyphColumn1: TGlyphColumn;
    StringColumn1: TStringColumn;
    Splitter1: TSplitter;
    Layout1: TLayout;
    Column1: TColumn;
    ProgressColumn1: TProgressColumn;
    CheckColumn1: TCheckColumn;
    DateColumn1: TDateColumn;
    TimeColumn1: TTimeColumn;
    PopupColumn1: TPopupColumn;
    BindNavigator1: TBindNavigator;
    FDMemTable1: TFDMemTable;
    FDMemTable1Index: TIntegerField;
    FDMemTable1ImageIndex: TIntegerField;
    FDMemTable1String: TStringField;
    FDMemTable1Column: TStringField;
    FDMemTable1Currency: TCurrencyField;
    FDMemTable1CheckBox: TBooleanField;
    FDMemTable1Date: TDateField;
    FDMemTable1Time: TTimeField;
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkGridToDataSourceBindSourceDB1: TLinkGridToDataSource;
    LayoutOptions: TLayout;
    ActnListView: TAction;
    procedure ActnEventLogExecute(Sender: TObject);
    procedure ActnEventLogUpdate(Sender: TObject);
    procedure ActnClearEventLogExecute(Sender: TObject);
    procedure ActnClearEventLogUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListView1Change(Sender: TObject);
    procedure LayoutEventLogResize(Sender: TObject);
    procedure ListView1ItemClick(const Sender: TObject; const AItem: TListViewItem);
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    procedure Grid1GetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
    procedure Grid1SetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
    procedure ActnListViewUpdate(Sender: TObject);
    procedure ActnListViewExecute(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ColumnMovedProc(Column: TColumn; FromIndex, ToIndex: Integer);
    procedure EditingDoneProc(Sender: TObject; const ACol, ARow: Integer);
    procedure HeaderClickProc(Column: TColumn);
    procedure ResizeProc(Sender: TObject);
    procedure SelChangedProc(Sender: TObject);
    procedure SelectCellProc(Sender: TObject; const ACol, ARow: Integer; var CanSelect: Boolean);
    procedure TapProc(Sender: TObject; const Point: TPointF);
  private
    FItemControlType: TListViewItem;
    FItemDefaultDraving: TListViewItem;
    FItemOption: array of TListViewItem;
    FItemReturnAction: array of TListViewItem;
    FItemWidth: TListViewItem;
    FStoredWidth: Single;
    FItemVisible: TListViewItem;
    FItemReadonly: TListViewItem;
    FItemEnabled: TListViewItem;
    FBoolImageIndex: array [Boolean] of Integer;
    FListViewUpdating: Boolean;
    FBitmaps: array of TBitmap;
    FStrings: array of string;
    FPopups: array of string;
    FDates: array of TDateTime;
    FTimes: array of TDateTime;
    procedure UpdateListView;
    procedure UpdateWidthValue;
    procedure PropogateOptions;
    procedure MemoAddLine(const EventName, Text: string);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses
  System.RTLConsts, System.Math, FMX.MultiResBitmap;

{$R *.fmx}

procedure TFormMain.MemoAddLine(const EventName, Text: string);
begin
  Memo1.Lines.Add(Format('%s(%s)', [EventName, Text]));
  Memo1.SelStart := Length(Memo1.Text) - 1;
  Memo1.SelLength := 0;
end;

procedure TFormMain.SelChangedProc(Sender: TObject);
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

procedure TFormMain.SelectCellProc(Sender: TObject; const ACol, ARow: Integer; var CanSelect: Boolean);
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

procedure TFormMain.TapProc(Sender: TObject; const Point: TPointF);
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

procedure TFormMain.ResizeProc(Sender: TObject);
var
  S: string;
begin
  if Sender is TControl then
    S := TControl(Sender).Name
  else
    S := '';
  MemoAddLine('OnResize', S);
end;

procedure TFormMain.EditingDoneProc(Sender: TObject; const ACol, ARow: Integer);
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

procedure TFormMain.ColumnMovedProc(Column: TColumn; FromIndex, ToIndex: Integer);
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

procedure TFormMain.HeaderClickProc(Column: TColumn);
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

procedure TFormMain.ActnEventLogExecute(Sender: TObject);
begin
  LayoutEventLog.Visible := not LayoutEventLog.Visible;
  Splitter1.Position.Y := TabControl1.Height;
  Splitter1.Visible := LayoutEventLog.Visible;
end;

procedure TFormMain.ActnEventLogUpdate(Sender: TObject);
begin
  if Sender is TCustomAction then
  begin
    if LayoutEventLog.Visible then
      TCustomAction(Sender).Hint := StrHideEventLog
    else
      TCustomAction(Sender).Hint := StrShowEventLog;
    TCustomAction(Sender).Checked := LayoutEventLog.Visible;
  end;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  if LayoutOptions.Visible then
  begin
    LayoutOptions.Height := Round(ClientHeight / 2);
    if LayoutEventLog.Visible then
      LayoutEventLog.Height := Min(Round((ClientHeight - LayoutOptions.Height) / 2), LayoutEventLog.Height);
  end
  else
    if LayoutEventLog.Visible then
      LayoutEventLog.Height := Min(Round(2/3 * ClientHeight), LayoutEventLog.Height);
end;

procedure TFormMain.ActnListViewExecute(Sender: TObject);
begin
  LayoutOptions.Height := Round(ClientHeight / 2);
  LayoutOptions.Visible := not LayoutOptions.Visible;
  FormResize(nil);
  if ListView1.Visible then
  begin
    ListView1.SetFocus;
    UpdateListView;
  end;
end;

procedure TFormMain.ActnListViewUpdate(Sender: TObject);
begin
  if Sender is TCustomAction then
    TCustomAction(Sender).Checked := LayoutOptions.Visible;
end;

procedure TFormMain.FormCreate(Sender: TObject);
  procedure InitStringValue(const ColumnIndex: Integer; var RowIndex: Integer; const Caption, Value: string);
  begin
    StringGrid1.Cells[2, RowIndex] := Caption;
    StringGrid1.Cells[3, RowIndex] := '''' + Value + '''';
    StringGrid1.Cells[ColumnIndex, RowIndex] := Value;
    Inc(RowIndex);
  end;
var
  I: Integer;
  Item: TListViewItem;
  Bitmap: TBitmap;
  SourceItem: TCustomSourceItem;
  BitmapItem: TCustomBitmapItem;
  Layer: TLayer;
  Column: TColumn;
begin
  ImageList1.CacheSize := 20;
  {$REGION 'ListView updating'}
  Bitmap := TCheckColumn.CreateBitmap(StringGrid1.Model,  True, True);
  try
    SourceItem := ImageList1.Source.Add;
    SourceItem.MultiResBitmap.SizeKind := TSizeKind.Source;
    if SourceItem.MultiResBitmap.Count = 0 then
      BitmapItem := SourceItem.MultiResBitmap.Add
    else
      BitmapItem := SourceItem.MultiResBitmap[0];
    BitmapItem.Bitmap.Assign(Bitmap);
    Layer := ImageList1.Destination.Add.Layers.Add;
    Layer.Name := SourceItem.Name;
    Layer.SourceRect.Rect := TRectF.Create(TPointF.Zero, Round(BitmapItem.Width / BitmapItem.Scale),
      Round(BitmapItem.Height / BitmapItem.Scale));
    FBoolImageIndex[False] := -1;
    FBoolImageIndex[True] := ImageList1.Count - 1;
  finally
    FreeAndNil(Bitmap);
  end;

  Item := ListView1.Items.Add;
  Item.Purpose := TListItemPurpose.Header;
  Item.Text := StringGrid1.ClassType.ClassParent.ClassName + '.' + 'Options';
  SetLength(FItemOption, Integer(High(TGridOption)) + 1);
  for I := Integer(Low(TGridOption)) to Integer(High(TGridOption)) do
  begin
    FItemOption[I] := ListView1.Items.Add;
    FItemOption[I].Text := GetEnumName(TypeInfo(TGridOption), I);
  end;

  Item := ListView1.Items.Add;
  Item.Purpose := TListItemPurpose.Header;
  Item.Text := StringGrid1.ClassType.ClassParent.ClassName + '.Model.CellReturnAction';
  SetLength(FItemReturnAction, Integer(High(TCellReturnAction)) + 1);
  for I := Integer(Low(TCellReturnAction)) to Integer(High(TCellReturnAction)) do
  begin
    FItemReturnAction[I] := ListView1.Items.Add;
    FItemReturnAction[I].Text := GetEnumName(TypeInfo(TCellReturnAction), I);
  end;

  Item := ListView1.Items.Add;
  Item.Purpose := TListItemPurpose.Header;
  Item.Text := StringGrid1.ClassType.ClassParent.ClassName;

  FItemControlType := ListView1.Items.Add;
  FItemControlType.Text := 'ControlType';
  FItemDefaultDraving := ListView1.Items.Add;
  FItemDefaultDraving.Text := 'DefaultDrawing';

  Item := ListView1.Items.Add;
  Item.Purpose := TListItemPurpose.Header;
  Item.Text := StringGrid1.ClassType.ClassParent.ClassName + '.Columns[1]';
  FItemWidth := ListView1.Items.Add;
  FItemWidth.Text := 'Width';
  FItemVisible := ListView1.Items.Add;
  FItemVisible.Text := 'Visible';

  Item := ListView1.Items.Add;
  Item.Purpose := TListItemPurpose.Header;
  Item.Text := StringGrid1.ClassType.ClassParent.ClassName + '.Columns[2]';
  FItemReadonly := ListView1.Items.Add;
  FItemReadonly.Text := 'ReadOnly';
  FItemEnabled := ListView1.Items.Add;
  FItemEnabled.Text := 'Enabled';
  {$ENDREGION}

  {$REGION 'String grid updating'}
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

  {$REGION 'Grid updating'}
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

  {$REGION 'FDMemTable updating'}
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
  {$ENDREGION}

  UpdateListView;
  PropogateOptions;
end;

procedure TFormMain.LayoutEventLogResize(Sender: TObject);
begin
  if LayoutEventLog.Height <= Splitter1.MinSize then
  begin
    LayoutEventLog.Visible := False;
    Splitter1.Visible := False;
  end;
end;

procedure TFormMain.ListView1Change(Sender: TObject);
begin
  UpdateListView;
end;

procedure TFormMain.ListView1ItemClick(const Sender: TObject; const AItem: TListViewItem);
var
  I: Integer;
begin
  try
    for I := Low(FItemOption) to High(FItemOption) do
      if AItem = FItemOption[I] then
      begin
        if TGridOption(I) in StringGrid1.Options then
          StringGrid1.Options := StringGrid1.Options - [TGridOption(I)]
        else
          StringGrid1.Options := StringGrid1.Options + [TGridOption(I)];
        Exit;
      end;
    for I := Low(FItemReturnAction) to High(FItemReturnAction) do
      if AItem = FItemReturnAction[I] then
      begin
        StringGrid1.Model.CellReturnAction := TCellReturnAction(I);
        Exit;
      end;
    if AItem = FItemWidth then
    begin
      if StringGrid1.Columns[1].Width > 0 then
        StringGrid1.Columns[1].Width := 0
      else
        StringGrid1.Columns[1].Width := FStoredWidth;
      Exit;
    end;
    if AItem = FItemVisible then
    begin
      StringGrid1.Columns[1].Visible := not StringGrid1.Columns[1].Visible;
      Exit;
    end;
    if AItem = FItemReadonly then
    begin
      StringGrid1.Columns[2].Readonly := not StringGrid1.Columns[2].Readonly;
      Exit;
    end;
    if AItem = FItemEnabled then
    begin
      StringGrid1.Columns[2].Enabled := not StringGrid1.Columns[2].Enabled;
      Exit;
    end;
  finally
    UpdateListView;
    PropogateOptions;
  end;
end;

procedure TFormMain.UpdateWidthValue;
begin
  FItemWidth.Detail := IntToStr(Round(StringGrid1.Columns[1].Width));
  //FItemWidth.ImageIndex := FBoolImageIndex[StringGrid1.Columns[1].Width > 0];
  if FItemWidth.Objects.AccessoryObject <> nil then
    FItemWidth.Objects.AccessoryObject.Visible := StringGrid1.Columns[1].Width > 0;
  if StringGrid1.Columns[1].Width > 0 then
    FStoredWidth := StringGrid1.Columns[1].Width;
end;

procedure TFormMain.PropogateOptions;
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

procedure TFormMain.UpdateListView;
  procedure UpdateViewItem(const Item: TListViewItem; const AType: PTypeInfo; const AValue: Integer;
    const AChecked: Boolean);
  begin
    Item.Detail := GetEnumName(AType, AValue);
    //Item.ImageIndex := FBoolImageIndex[AChecked];
    if Item.Objects.AccessoryObject <> nil then
      Item.Objects.AccessoryObject.Visible := AChecked;
  end;
  procedure UpdateViewItemBool(const Item: TListViewItem; const AChecked: Boolean);
  begin
    UpdateViewItem(Item, TypeInfo(Boolean), Ord(AChecked), AChecked);
  end;
var
  I: Integer;
begin
  if not FListViewUpdating then
  begin
    FListViewUpdating := True;
    try
      ListView1.BeginUpdate;
      try
        FItemControlType.Detail := GetEnumName(TypeInfo(TControlType), Ord(StringGrid1.ControlType));
        UpdateViewItem(FItemControlType, TypeInfo(TControlType), Ord(StringGrid1.ControlType),
          StringGrid1.ControlType = TControlType.Platform);
        if StringGrid1.ControlType = TControlType.Platform then
          FItemControlType.ImageIndex := 3
        else
          FItemControlType.ImageIndex := -1;
        if FItemControlType.Objects.AccessoryObject <> nil then
          FItemControlType.Objects.AccessoryObject.Visible := False;
        UpdateViewItemBool(FItemDefaultDraving, StringGrid1.DefaultDrawing);
        for I := Integer(Low(TGridOption)) to Integer(High(TGridOption)) do
        begin
          UpdateViewItemBool(FItemOption[I], TGridOption(I) in StringGrid1.Options);
        end;
        for I := Integer(Low(TCellReturnAction)) to Integer(High(TCellReturnAction)) do
        begin
          UpdateViewItemBool(FItemReturnAction[I], StringGrid1.Model.CellReturnAction = TCellReturnAction(I));
        end;
        UpdateWidthValue;
        UpdateViewItemBool(FItemVisible, StringGrid1.Columns[1].Visible);
        UpdateViewItemBool(FItemReadonly, StringGrid1.Columns[2].ReadOnly);
        UpdateViewItemBool(FItemEnabled, StringGrid1.Columns[2].Enabled);
      finally
        ListView1.EndUpdate;
      end;
    finally
      FListViewUpdating := False;
    end;
  end;
end;

procedure TFormMain.ActionList1Update(Action: TBasicAction; var Handled: Boolean);
begin
  UpdateWidthValue;
end;

procedure TFormMain.ActnClearEventLogExecute(Sender: TObject);
begin
  Memo1.Text := EmptyStr;
end;

procedure TFormMain.ActnClearEventLogUpdate(Sender: TObject);
begin
  if Sender is TCustomAction then
    TCustomAction(Sender).Enabled := not Memo1.Text.IsEmpty;
end;

procedure TFormMain.Grid1GetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
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

procedure TFormMain.Grid1SetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
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

end.
