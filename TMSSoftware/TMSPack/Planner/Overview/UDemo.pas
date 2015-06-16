unit UDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.TMSBaseControl, FMX.TMSPlannerBase, FMX.TMSPlannerData, FMX.TMSPlanner,
  FMX.Layouts, FMX.StdCtrls, FMX.ListBox, FMX.TMSBitmapContainer;

type
  TForm8 = class(TForm)
    CornerButton1: TCornerButton;
    CornerButton2: TCornerButton;
    CornerButton3: TCornerButton;
    Layout1: TLayout;
    Layout2: TLayout;
    Label1: TLabel;
    TMSFMXPlanner1: TTMSFMXPlanner;
    CornerButton4: TCornerButton;
    ComboBox1: TComboBox;
    Label2: TLabel;
    Label3: TLabel;
    CheckBox1: TCheckBox;
    Label4: TLabel;
    CheckBox2: TCheckBox;
    TMSFMXBitmapContainer1: TTMSFMXBitmapContainer;
    Layout3: TLayout;
    ComboBox2: TComboBox;
    Label5: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure CornerButton2ApplyStyleLookup(Sender: TObject);
    procedure CornerButton3Click(Sender: TObject);
    procedure CornerButton4Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure TMSFMXPlanner1ItemAnchorClick(Sender: TObject;
      AItem: TTMSFMXPlannerItem; AAnchor: string);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure TMSFMXPlanner1BeforeDrawPosition(Sender: TObject;
      ACanvas: TCanvas; ARect: TRectF; APosition: Integer;
      AKind: TTMSFMXPlannerCacheItemKind; var AAllow, ADefaultDraw: Boolean);
    procedure TMSFMXPlanner1BeforeDrawTimeText(Sender: TObject;
      ACanvas: TCanvas; ARect: TRectF; AValue: Double; ARow: Integer;
      ASubUnit: Boolean; AKind: TTMSFMXPlannerCacheItemKind; AText: string;
      var AAllow: Boolean);
    procedure TMSFMXPlanner1BeforeDrawPositionText(Sender: TObject;
      ACanvas: TCanvas; ARect: TRectF; APosition: Integer;
      AKind: TTMSFMXPlannerCacheItemKind; AText: string; var AAllow: Boolean);
    procedure TMSFMXPlanner1BeforeDrawGroupText(Sender: TObject;
      ACanvas: TCanvas; ARect: TRectF; AGroup, AStartPosition,
      AEndPosition: Integer; AKind: TTMSFMXPlannerCacheItemKind; AText: string;
      var AAllow: Boolean);
    procedure TMSFMXPlanner1AfterSelectItem(Sender: TObject;
      AItem: TTMSFMXPlannerItem);
    procedure ComboBox2Change(Sender: TObject);
  private
    { Private declarations }
    FIsIphone: Boolean;
  public
    { Public declarations }
    procedure ChangeMode;
    procedure ChangeDisplayUnit;
    procedure AddItems;
    procedure DrawCustomPositionText(ACanvas: TCanvas; APosition: Integer);
  end;

var
  Form8: TForm8;

implementation

{$R *.fmx}

uses
  UIConsts, DateUtils, Math, FMX.Objects
  {$IFDEF IOS}
  , iOSApi.UIKit
  {$ENDIF}
  ;

procedure TForm8.AddItems;
var
  dt: TDateTime;
  it: TTMSFMXPlannerItem;
  c: Double;
  I: Integer;
begin
  Layout2.Height := 40;
  Layout2.Width := 75 * 3;
  CornerButton4.Width := 85;
  CornerButton4.Height := 40;
  CornerButton1.Width := 75;
  CornerButton2.Width := 75;
  CornerButton3.Width := 75;
  CornerButton4.Position.Y := Layout2.Position.Y;

  TMSFMXPlanner1.BeginUpdate;

  dt := Now;
  TMSFMXPlanner1.Items.Clear;
  c := 0;
  if (TMSFMXPlanner1.Mode = pmMultiDay) then
    c := 2;

  it := TMSFMXPlanner1.AddOrUpdateItem(Int(dt) + EncodeTime(6, 30, 0, 0), Int(dt) + EncodeTime(13, 0, 0, 0), 'Meeting with John',
    '<u>Necessities</u><br><ul><li>Notebook<li>Digital lineout<li>Model artwork</ul>');
  it.Color := MakeColor(255, 246, 210);
  it.TitleColor := MakeColor(255, 211, 39);
  it.FontColor := MakeColor(190, 165, 59);

  it := TMSFMXPlanner1.AddOrUpdateItem(Int(dt) + c + EncodeTime(13, 30, 0, 0), Int(dt) + c + EncodeTime(16, 30, 0, 0), 'Presentation', 'The new A3');
  it.Resource := 2;
  it.Color := MakeColor(210, 239, 255);
  it.TitleColor := MakeColor(61, 185, 249);
  it.FontColor := MakeColor(17, 111, 159);

  it := TMSFMXPlanner1.AddOrUpdateItem(Int(dt) + c + 1 + EncodeTime(20, 30, 0, 0), Int(dt) + c + 1 + EncodeTime(21, 30, 0, 0), 'Audi Conditions', '<ul><li>Update iOS application<li>Change Audi packs<li>Change online terms</ul>');
  it.Resource := 2;
  it.Color := MakeColor(210, 239, 255);
  it.TitleColor := MakeColor(61, 185, 249);
  it.FontColor := MakeColor(17, 111, 159);

  it := TMSFMXPlanner1.AddOrUpdateItem(Int(dt) + c + 1 + EncodeTime(9, 30, 0, 0), Int(dt) + c + 1 + EncodeTime(12, 45, 0, 0), 'Audi - Mercedes fusion', 'Meeting with Bruno Fierens for approval<br><a href="http://www.tmssoftware.com">http://www.tmssoftware.com</a>');
  it.Resource := 2;
  it.Color := MakeColor(210, 239, 255);
  it.TitleColor := MakeColor(61, 185, 249);
  it.FontColor := MakeColor(17, 111, 159);

  it := TMSFMXPlanner1.AddOrUpdateItem(Int(dt) + 1 + C + EncodeTime(5, 45, 0, 0), Int(dt) + 1 + c + EncodeTime(7, 35, 0, 0), 'Test drive',
    'Test drive of the new BMW i8');
  it.Color := MakeColor(255, 246, 210);
  it.TitleColor := MakeColor(255, 211, 39);
  it.FontColor := MakeColor(190, 165, 59);

  it := TMSFMXPlanner1.AddOrUpdateItem(Int(dt) + 1 + C + EncodeTime(9, 30, 0, 0), Int(dt) + 1 + c + EncodeTime(11, 35, 0, 0), 'Reminder',
    'Trip to Brussels to present the future of BMW');
  it.Color := MakeColor(255, 246, 210);
  it.TitleColor := MakeColor(255, 211, 39);
  it.FontColor := MakeColor(190, 165, 59);

  c := 0;
  if TMSFMXPlanner1.Mode = pmMultiResDay then
    c := -5;

  it := TMSFMXPlanner1.AddOrUpdateItem(Int(dt) + c + 6 + EncodeTime(12, 35, 0, 0), Int(dt) + c + 6 + EncodeTime(16, 50, 0, 0), 'Exposition', 'Mercedes exposition on the AMG GT Coupé');
  it.Resource := 1;
  it.Color := MakeColor(222, 255, 210);
  it.TitleColor := MakeColor(122, 223, 86);
  it.FontColor := MakeColor(114, 172, 93);

  it := TMSFMXPlanner1.AddOrUpdateItem(Int(dt) + c + 5 + EncodeTime(8, 35, 0, 0), Int(dt) + c + 5 + EncodeTime(10, 50, 0, 0), 'Meeting', 'Meeting with sponsors for 2015');
  it.Resource := 1;
  it.Color := MakeColor(222, 255, 210);
  it.TitleColor := MakeColor(122, 223, 86);
  it.FontColor := MakeColor(114, 172, 93);

  for I := 0 to TMSFMXPlanner1.Items.Count - 1 do
  begin
    it := TMSFMXPlanner1.Items[I];
    if (TMSFMXPlanner1.Mode = pmMonth) or (TMSFMXPlanner1.Mode = pmMultiMonth) then
    begin
      Randomize;
      if TMSFMXPlanner1.Mode = pmMultiMonth then
        c := Random(100)
      else
        c := RandomRange(-20, 20);

      it.StartTime := it.StartTime + c;
      it.EndTime := it.EndTime + c + 6;
    end;

    {$IFNDEF IOS}
    it.Hint := it.Title;
    {$ENDIF}
    it.TitleFontColor := it.FontColor;
    it.ActiveColor := it.TitleColor;
    it.ActiveTitleFontColor := it.TitleFontColor;
    it.ActiveFontColor := it.FontColor;
  end;

  TMSFMXPlanner1.EndUpdate;
end;

procedure TForm8.ChangeDisplayUnit;
begin
  if (TMSFMXPlanner1.Mode <> pmMonth) and (TMSFMXPlanner1.Mode <> pmMultiMonth) then
  begin
    TMSFMXPlanner1.BeginUpdate;
    case ComboBox1.ItemIndex of
      0: TMSFMXPlanner1.TimeLine.DisplayUnit := 5;
      1: TMSFMXPlanner1.TimeLine.DisplayUnit := 10;
      2: TMSFMXPlanner1.TimeLine.DisplayUnit := 12;
      3: TMSFMXPlanner1.TimeLine.DisplayUnit := 15;
      4: TMSFMXPlanner1.TimeLine.DisplayUnit := 20;
      5: TMSFMXPlanner1.TimeLine.DisplayUnit := 30;
      6: TMSFMXPlanner1.TimeLine.DisplayUnit := 60;
    end;
    TMSFMXPlanner1.TimeLine.DisplayEnd := 10000;
    TMSFMXPlanner1.EndUpdate;
  end;
end;

procedure TForm8.ChangeMode;
begin
  TMSFMXPlanner1.BeginUpdate;
  if CornerButton4.IsPressed then
  begin
    if CornerButton1.IsPressed then
    begin
      TMSFMXPlanner1.Mode := pmMultiResDay;
      TMSFMXPlanner1.Positions.Count := 3;
      TMSFMXPlanner1.Positions.Format := 'dddd d';
    end
    else if CornerButton2.IsPressed then
    begin
      TMSFMXPlanner1.Mode := pmMultiResDay;
      if FIsIphone then
        TMSFMXPlanner1.Positions.Count := 3
      else
        TMSFMXPlanner1.Positions.Count := 6;

      TMSFMXPlanner1.Positions.Format := 'dddd d';
    end
    else if CornerButton3.IsPressed then
    begin
      TMSFMXPlanner1.Mode := pmMonth;
      TMSFMXPlanner1.Positions.Count := 3;
      TMSFMXPlanner1.Positions.Format := 'mmmm';
    end;
  end
  else
  begin
    if CornerButton1.IsPressed then
    begin
      TMSFMXPlanner1.Mode := pmMultiDay;
      TMSFMXPlanner1.Positions.Count := 1;
      TMSFMXPlanner1.Positions.Format := 'dddd d';
    end
    else if CornerButton2.IsPressed then
    begin
      TMSFMXPlanner1.Mode := pmMultiDay;
      TMSFMXPlanner1.Positions.Count := 7;
      TMSFMXPlanner1.Positions.Format := 'dddd d';
    end
    else if CornerButton3.IsPressed then
    begin
      TMSFMXPlanner1.Mode := pmMultiMonth;
      TMSFMXPlanner1.Positions.Count := 5;
      TMSFMXPlanner1.Positions.Format := 'mmmm';
    end;
  end;

  TMSFMXPlanner1.EndUpdate;

  ChangeDisplayUnit;
  AddItems;
end;

procedure TForm8.CheckBox1Change(Sender: TObject);
begin
  TMSFMXPlanner1.Interaction.MultiSelect := CheckBox1.IsChecked;
end;

procedure TForm8.CheckBox2Change(Sender: TObject);
begin
  if Assigned(TMSFMXPlanner1.ActiveItem) then
    TMSFMXPlanner1.ActiveItem.FixedResource := CheckBox2.IsChecked;
end;

procedure TForm8.ComboBox1Change(Sender: TObject);
begin
  ChangeDisplayUnit;
end;

procedure TForm8.ComboBox2Change(Sender: TObject);
begin
  TMSFMXPlanner1.OrientationMode := TTMSFMXPlannerOrientationMode(ComboBox2.ItemIndex);
end;

procedure TForm8.CornerButton2ApplyStyleLookup(Sender: TObject);
var
  b: TCornerButton;
  r: TRectangle;
begin
  b := (Sender as TCornerButton);
  r := b.FindStyleResource('background') as TRectangle;
  r.Visible := b.IsPressed;
  r.Children[0].Free;
  r.Fill.Kind := TBrushKind.bkSolid;
end;

procedure TForm8.CornerButton3Click(Sender: TObject);
begin
  CornerButton1.IsPressed := False;
  CornerButton2.IsPressed := False;
  CornerButton3.IsPressed := False;
  (Sender as TCornerButton).IsPressed := True;
  CornerButton1.NeedStyleLookup;
  CornerButton1.ApplyStyleLookup;
  CornerButton2.NeedStyleLookup;
  CornerButton2.ApplyStyleLookup;
  CornerButton3.NeedStyleLookup;
  CornerButton3.ApplyStyleLookup;
  ChangeMode;
end;

procedure TForm8.CornerButton4Click(Sender: TObject);
begin
  if CornerButton4.Tag = 0 then
  begin
    CornerButton4.IsPressed := True;
    CornerButton4.Tag := 1;
  end
  else
  begin
    CornerButton4.IsPressed := False;
    CornerButton4.Tag := 0;
  end;

  CornerButton4.NeedStyleLookup;
  CornerButton4.ApplyStyleLookup;
  ChangeMode;
end;

procedure TForm8.DrawCustomPositionText(ACanvas: TCanvas; APosition: Integer);
begin
  if Int(Now) = TMSFMXPlanner1.PositionToDateTime(APosition) then
  begin
    ACanvas.Fill.Color := MakeColor(50, 142, 238);
    ACanvas.Font.Style := [TFontStyle.fsBold];
  end
  else if DayOfTheWeek(TMSFMXPlanner1.PositionToDateTime(APosition)) in [6, 7] then
    ACanvas.Fill.Color := MakeColor(203, 64, 64);
end;

procedure TForm8.FormCreate(Sender: TObject);
var
  dt: TDateTime;
  I: Integer;
begin
  dt := Now;
  TMSFMXPlanner1.Stroke.Kind := TBrushKind.bkNone;
  TMSFMXPlanner1.BeginUpdate;
  TMSFMXPlanner1.BitmapContainer := TMSFMXBitmapContainer1;

  TMSFMXPlanner1.Interaction.KeyboardInsertMode := pkimSelection;
  TMSFMXPlanner1.Interaction.MouseInsertMode := pmimDialogAfterSelection;
  TMSFMXPlanner1.Interaction.UpdateMode := pumDialog;
  TMSFMXPlanner1.ModeSettings.StartTime := dt;

  Fill.Color := MakeColor(237, 243, 248);
  Fill.Kind := TBrushKind.bkSolid;
  TMSFMXPlanner1.Fill.Assign(Fill);

  TMSFMXPlanner1.TimeLineAppearance.LeftStroke.Kind := TBrushKind.bkNone;
  TMSFMXPlanner1.TimeLineAppearance.LeftFontColor := MakeColor(115, 115, 115);
  TMSFMXPlanner1.TimeLineAppearance.LeftFont.Style := [TFontStyle.fsBold];
  TMSFMXPlanner1.TimeLineAppearance.LeftFont.Size := 18;
  TMSFMXPlanner1.TimeLineAppearance.LeftSubUnitFontSize := 12;

  TMSFMXPlanner1.Positions.Count := 7;
  TMSFMXPlanner1.Positions.Format := 'dddd d';
  TMSFMXPlanner1.PositionsAppearance.TopStroke.Kind := TBrushKind.bkNone;
  TMSFMXPlanner1.PositionsAppearance.TopFontColor := MakeColor(115, 115, 115);
  TMSFMXPlanner1.PositionsAppearance.TopFont.Size := 16;
  TMSFMXPlanner1.PositionsAppearance.TopFontColor := MakeColor(115, 115, 115);

  TMSFMXPlanner1.GroupsAppearance.TopStroke.Kind := TBrushKind.bkNone;
  TMSFMXPlanner1.GroupsAppearance.TopFontColor := MakeColor(115, 115, 115);
  TMSFMXPlanner1.GroupsAppearance.TopFont.Size := 16;
  TMSFMXPlanner1.GroupsAppearance.TopFontColor := MakeColor(115, 115, 115);

  TMSFMXPlanner1.GridCellAppearance.Fill.Kind := TBrushKind.bkSolid;
  TMSFMXPlanner1.Margins.Left := 10;
  TMSFMXPlanner1.Margins.Bottom := 10;
  TMSFMXPlanner1.StretchScrollBars := False;

  Label1.Width := 1000;
  Label1.AutoSize := True;
  Label1.Font.Size := 24;
  Label1.FontColor := MakeColor(115, 115, 115);
  Label1.Text := FormatDateTime('d mmmm, yyyy', dt) + ' (week ' + inttostr(WeekOf(dt))+')';
  Label1.Align := TAlignLayout.alBottom;
  Label1.Margins.Bottom := 10;
  Label1.Margins.Left := TMSFMXPlanner1.TimeLineAppearance.LeftSize + TMSFMXPlanner1.Margins.Left;
  TMSFMXPlanner1.ItemsAppearance.MoveAreaColor := MakeColor(claDarkgray, 0.5);
  TMSFMXPlanner1.ItemsAppearance.SizeAreaColor := MakeColor(claDarkgray, 0.5);

  CornerButton2.IsPressed := True;

  {$IFDEF IOS}
  if TUIDevice.Wrap(TUIDevice.OCClass.currentDevice).userInterfaceIdiom = UIUserInterfaceIdiomPhone then
  begin
    FIsIphone := True;
    CornerButton2.IsPressed := True;
    CornerButton4.IsPressed := True;
    Layout1.Visible := False;
    for I := 0 to TMSFMXPlanner1.Resources.Count - 1 do
    begin
      TMSFMXPlanner1.Resources[I].Name := TMSFMXPlanner1.Resources[I].Text;
      if TMSFMXPlanner1.Resources[I].Name = 'Mercedes' then
        TMSFMXPlanner1.Resources[I].Text := '<img src="'+TMSFMXPlanner1.Resources[I].Text+'" width="100"/>'
      else
        TMSFMXPlanner1.Resources[I].Text := '<img src="'+TMSFMXPlanner1.Resources[I].Text+'" height="40"/>'
    end;
  end
  else
  begin
    for I := 0 to TMSFMXPlanner1.Resources.Count - 1 do
    begin
      TMSFMXPlanner1.Resources[I].Name := TMSFMXPlanner1.Resources[I].Text;
      TMSFMXPlanner1.Resources[I].Text := '<img src="'+TMSFMXPlanner1.Resources[I].Text+'" height="35"/>';
    end;
  end;
  {$ELSE}
  for I := 0 to TMSFMXPlanner1.Resources.Count - 1 do
  begin
    TMSFMXPlanner1.Resources[I].Name := TMSFMXPlanner1.Resources[I].Text;
    TMSFMXPlanner1.Resources[I].Text := '<img src="'+TMSFMXPlanner1.Resources[I].Text+'" height="45"/>';
  end;
  {$ENDIF}
  ChangeMode;
  TMSFMXPlanner1.TimeLine.ViewStart := Int(Now) + EncodeTime(5, 0, 0, 0);
  TMSFMXPlanner1.EndUpdate;
  AddItems;
end;

procedure TForm8.TMSFMXPlanner1AfterSelectItem(Sender: TObject;
  AItem: TTMSFMXPlannerItem);
begin
  CheckBox2.IsChecked := TMSFMXPlanner1.ActiveItem.FixedResource;
end;

procedure TForm8.TMSFMXPlanner1BeforeDrawGroupText(Sender: TObject;
  ACanvas: TCanvas; ARect: TRectF; AGroup, AStartPosition,
  AEndPosition: Integer; AKind: TTMSFMXPlannerCacheItemKind; AText: string;
  var AAllow: Boolean);
begin
  if TMSFMXPlanner1.Mode = pmMultiResDay then
    DrawCustomPositionText(ACanvas, AStartPosition);
end;

procedure TForm8.TMSFMXPlanner1BeforeDrawPosition(Sender: TObject;
  ACanvas: TCanvas; ARect: TRectF; APosition: Integer;
  AKind: TTMSFMXPlannerCacheItemKind; var AAllow, ADefaultDraw: Boolean);
begin
  if TMSFMXPlanner1.Mode = pmMultiResDay then
  begin
    ACanvas.Stroke.Kind := TBrushKind.bkSolid;
    ACanvas.Stroke.Color := claDarkgray;
    ADefaultDraw := False;
    ACanvas.DrawLine(ARect.TopLeft, PointF(ARect.Right, ARect.Top), 1);
  end;
end;

procedure TForm8.TMSFMXPlanner1BeforeDrawPositionText(Sender: TObject;
  ACanvas: TCanvas; ARect: TRectF; APosition: Integer;
  AKind: TTMSFMXPlannerCacheItemKind; AText: string; var AAllow: Boolean);
begin
  if (TMSFMXPlanner1.Mode = pmMultiDay) or (TMSFMXPlanner1.Mode = pmDay) then
    DrawCustomPositionText(ACanvas, APosition);
end;

procedure TForm8.TMSFMXPlanner1BeforeDrawTimeText(Sender: TObject;
  ACanvas: TCanvas; ARect: TRectF; AValue: Double; ARow: Integer;
  ASubUnit: Boolean; AKind: TTMSFMXPlannerCacheItemKind; AText: string;
  var AAllow: Boolean);
begin
  if ASubUnit then
    ACanvas.Font.Style := [];

  if (TMSFMXPlanner1.Mode = pmMonth) then
  begin
    if Int(Now) = AValue then
    begin
      ACanvas.Fill.Color := MakeColor(50, 142, 238);
      ACanvas.Font.Style := [TFontStyle.fsBold];
    end
    else if DayOfTheWeek(AValue) in [6, 7] then
      ACanvas.Fill.Color := MakeColor(203, 64, 64);
  end;
end;

procedure TForm8.TMSFMXPlanner1ItemAnchorClick(Sender: TObject;
  AItem: TTMSFMXPlannerItem; AAnchor: string);
begin
  ShowMessage(AAnchor);
end;

end.
