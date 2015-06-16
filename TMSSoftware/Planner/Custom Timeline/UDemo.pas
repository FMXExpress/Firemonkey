unit UDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TMSBaseControl, FMX.TMSPlannerBase, FMX.TMSPlannerData, FMX.TMSPlanner,
  FMX.TMSBitmapContainer, FMX.ListBox;

type
  TForm1 = class(TForm)
    TMSFMXPlanner1: TTMSFMXPlanner;
    TMSFMXBitmapContainer1: TTMSFMXBitmapContainer;
    Panel1: TPanel;
    ComboBox1: TComboBox;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure TMSFMXPlanner1AfterDrawPositionEmptySpace(Sender: TObject;
      ACanvas: TCanvas; ARect: TRectF;
      ASpace: TTMSFMXPlannerPositionEmptySpace);
    procedure TMSFMXPlanner1GetTimeText(Sender: TObject; AValue: Double;
      ARow: Integer; ASubUnit: Boolean; AKind: TTMSFMXPlannerCacheItemKind;
      var AText: string);
    procedure ComboBox1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ChangeMode;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  UIConsts, DateUtils;

procedure TForm1.ChangeMode;
var
  I: Integer;
  it: TTMSFMXPlannerItem;
begin
  TMSFMXPlanner1.BeginUpdate;
  TMSFMXPlanner1.CustomDateTimes.Clear;
  case ComboBox1.ItemIndex of
    0:
    begin
      for I := 0 to 15 do
        TMSFMXPlanner1.CustomDateTimes.Add(I);

      TMSFMXPlanner1.TimeLineAppearance.Stretch := True;
      TMSFMXPlanner1.ItemsAppearance.TextHorizontalTextAlign := ptaTrailing;
      TMSFMXPlanner1.Positions.Count := 3;
    end;
    1:
    begin
      for I := 0 to 44 do
        TMSFMXPlanner1.CustomDateTimes.Add(I);

      TMSFMXPlanner1.TimeLineAppearance.Stretch := False;
      TMSFMXPlanner1.ItemsAppearance.TextHorizontalTextAlign := ptaLeading;
      TMSFMXPlanner1.Positions.Count := 4;
    end;
  end;

  TMSFMXPlanner1.EndUpdate;

  TMSFMXPlanner1.BeginUpdate;
  TMSFMXPlanner1.Items.Clear;
  case ComboBox1.ItemIndex of
    0:
    begin
      TMSFMXPlanner1.AddOrUpdateItem(0, 12.5, '', '<img height="35" src="Ariel"/> Ariel 12.5 seconds').Resource := 0;
      TMSFMXPlanner1.AddOrUpdateItem(0, 10, '', '<img height="35" src="Pepsi"/> Pepsi 10 seconds').Resource := 0;
      TMSFMXPlanner1.AddOrUpdateItem(0, 8, '', '<img height="35" src="Coca Cola"/> Coca Cola 8 seconds').Resource := 0;
      TMSFMXPlanner1.AddOrUpdateItem(0, 12, '', '<img height="35" src="Jack Wolfskin"/> Jack Wolfskin 12 seconds').Resource := 0;

      TMSFMXPlanner1.AddOrUpdateItem(0, 10, '', '<img height="35" src="Ariel"/> Ariel 10 seconds').Resource := 1;
      TMSFMXPlanner1.AddOrUpdateItem(0, 12.5, '', '<img height="35" src="Pepsi"/> Pepsi 12.5 seconds').Resource := 1;
      TMSFMXPlanner1.AddOrUpdateItem(0, 7, '', '<img height="35" src="Coca Cola"/> Coca Cola 7 seconds').Resource := 1;
      TMSFMXPlanner1.AddOrUpdateItem(0, 6, '', '<img height="35" src="Burger King"/> Burger King 6 seconds').Resource := 1;

      TMSFMXPlanner1.AddOrUpdateItem(0, 6.5, '', '<img height="35" src="Ariel"/> Ariel 6.5 seconds').Resource := 2;
      TMSFMXPlanner1.AddOrUpdateItem(0, 10, '', '<img height="35" src="Burger King"/> Burger King 10 seconds').Resource := 2;
      TMSFMXPlanner1.AddOrUpdateItem(0, 8, '', '<img height="35" src="Coca Cola"/> Coca Cola 8 seconds').Resource := 2;
      TMSFMXPlanner1.AddOrUpdateItem(0, 12, '', '<img height="35" src="Jack Wolfskin"/> Jack Wolfskin 12 seconds').Resource := 2;

      TMSFMXPlanner1.AddOrUpdateItem(0, 10.5, '', '<img height="35" src="Jack Wolfskin"/> Jack Wolfskin 10.5 seconds').Resource := 3;
      TMSFMXPlanner1.AddOrUpdateItem(0, 10, '', '<img height="35" src="Burger King"/> Burger King 10 seconds').Resource := 3;
      TMSFMXPlanner1.AddOrUpdateItem(0, 8, '', '<img height="35" src="Ariel"/> Ariel 8 seconds').Resource := 3;
      TMSFMXPlanner1.AddOrUpdateItem(0, 8, '', '<img height="35" src="Coca Cola"/> Coca Cola 8 seconds').Resource := 3;
    end;
    1:
    begin
      it := TMSFMXPlanner1.AddOrUpdateItem(0, 12.5, '', '<img height="80" src="Ariel"/> Ariel 12.5 seconds');
      it.Resource := 0;
      it := TMSFMXPlanner1.AddOrUpdateItem(it.EndTime, it.EndTime + 9.5, '', '<img height="80" src="Pepsi"/> Pepsi 9.5 seconds');
      it.Resource := 0;
      it := TMSFMXPlanner1.AddOrUpdateItem(it.EndTime, it.EndTime + 7, '', '<img height="80" src="Coca Cola"/> Coca Cola 7 seconds');
      it.Resource := 0;
      it := TMSFMXPlanner1.AddOrUpdateItem(it.EndTime, it.EndTime + 12, '', '<img height="80" src="Jack Wolfskin"/> Jack Wolfskin 12 seconds');
      it.Resource := 0;

      it := TMSFMXPlanner1.AddOrUpdateItem(0, 10, '', '<img height="80" src="Pepsi"/> Pepsi 10 seconds');
      it.Resource := 1;
      it := TMSFMXPlanner1.AddOrUpdateItem(it.EndTime, it.EndTime + 12, '', '<img height="80" src="Jack Wolfskin"/> Jack Wolfskin 12 seconds');
      it.Resource := 1;
      it := TMSFMXPlanner1.AddOrUpdateItem(it.EndTime, it.EndTime + 7, '', '<img height="80" src="Coca Cola"/> Coca Cola 7 seconds');
      it.Resource := 1;
      it := TMSFMXPlanner1.AddOrUpdateItem(it.EndTime, it.EndTime + 6, '', '<img height="80" src="Burger King"/> Burger King 6 seconds');
      it.Resource := 1;

      it := TMSFMXPlanner1.AddOrUpdateItem(0, 6.5, '', '<img height="80" src="Burger King"/> Burger King 6.5 seconds');
      it.Resource := 2;
      it := TMSFMXPlanner1.AddOrUpdateItem(it.EndTime, it.EndTime + 11.5, '', '<img height="80" src="Coca Cola"/> Coca Cola 11.5 seconds');
      it.Resource := 2;
      it := TMSFMXPlanner1.AddOrUpdateItem(it.EndTime, it.EndTime + 8, '', '<img height="80" src="Ariel"/> Ariel 8 seconds');
      it.Resource := 2;
      it := TMSFMXPlanner1.AddOrUpdateItem(it.EndTime, it.EndTime + 12, '', '<img height="80" src="Jack Wolfskin"/> Jack Wolfskin 12 seconds');
      it.Resource := 2;

      it := TMSFMXPlanner1.AddOrUpdateItem(0, 8, '', '<img height="80" src="Coca Cola"/> Coca Cola 8 seconds');
      it.Resource := 3;
      it := TMSFMXPlanner1.AddOrUpdateItem(it.EndTime, it.EndTime + 10, '', '<img height="80" src="Jack Wolfskin"/> Jack Wolfskin 10 seconds');
      it.Resource := 3;
      it := TMSFMXPlanner1.AddOrUpdateItem(it.EndTime, it.EndTime + 8.5, '', '<img height="80" src="Ariel"/> Ariel 8.5 seconds');
      it.Resource := 3;
      it := TMSFMXPlanner1.AddOrUpdateItem(it.EndTime, it.EndTime + 11.5, '', '<img height="80" src="Burger King"/> Burger King 11.5 seconds');
      it.Resource := 3;
    end;
  end;
  TMSFMXPlanner1.EndUpdate;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  ChangeMode;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  res: TTMSFMXPlannerResource;
begin
  TMSFMXPlanner1.BeginUpdate;
  TMSFMXPlanner1.Fill.Color := claLightslategray;
  TMSFMXPlanner1.Stroke.Color := claGhostWhite;
  TMSFMXPlanner1.TimeLineAppearance.LeftFontColor := claGhostwhite;
  TMSFMXPlanner1.GridCellAppearance.VerticalStroke.Color := claGhostwhite;
  TMSFMXPlanner1.GridCellAppearance.HorizontalStroke.Color := claGhostwhite;
  TMSFMXPlanner1.PositionsAppearance.TopStroke.Color := claGhostwhite;
  TMSFMXPlanner1.GroupsAppearance.TopStroke.Color := claGhostwhite;
  TMSFMXPlanner1.TimeLineAppearance.LeftStroke.Color := claGhostwhite;
  TMSFMXPlanner1.GroupsAppearance.BottomStroke.Color := claGhostwhite;
  TMSFMXPlanner1.TimeLineAppearance.RightStroke.Color := claGhostwhite;
  TMSFMXPlanner1.ItemsAppearance.TextVerticalTextAlign := ptaCenter;
  TMSFMXPlanner1.ModeSettings.InActiveDays := [];
  TMSFMXPlanner1.Mode := pmCustom;
  TMSFMXPlanner1.TimeLineAppearance.LeftSize := 50;
  TMSFMXPlanner1.TimeLineAppearance.LeftHorizontalTextAlign := ptaTrailing;
  TMSFMXPlanner1.TimeLine.DisplayUnitSize := 50;
  TMSFMXPlanner1.OrientationMode := pomHorizontal;
  TMSFMXPlanner1.BitmapContainer := TMSFMXBitmapContainer1;
  TMSFMXPlanner1.PositionsAppearance.TopVerticalTextMode := pvtmNone;
  TMSFMXPlanner1.PositionsAppearance.TopSize := 150;

  TMSFMXPlanner1.Resources.Clear;
  res := TMSFMXPlanner1.Resources.Add;
  res.Name := 'National Geographic';
  res.Text := '<img width="140" src="'+res.Name+'"/>';

  res := TMSFMXPlanner1.Resources.Add;
  res.Name := 'BBC';
  res.Text := '<img width="140"src="'+res.Name+'"/>';

  res := TMSFMXPlanner1.Resources.Add;
  res.Name := 'Discovery Channel';
  res.Text := '<img width="140" src="'+res.Name+'"/>';

  res := TMSFMXPlanner1.Resources.Add;
  res.Name := 'Nickelodeon';
  res.Text := '<img width="140" src="'+res.Name+'"/>';

  TMSFMXPlanner1.Interaction.ShowSelection := False;
  TMSFMXPlanner1.Interaction.ReadOnly := True;
  TMSFMXPlanner1.ItemsAppearance.ShowItemHelpers := False;
  TMSFMXPlanner1.SelectItem(nil);
  TMSFMXPlanner1.EndUpdate;
  ChangeMode;
end;

procedure TForm1.TMSFMXPlanner1AfterDrawPositionEmptySpace(Sender: TObject;
  ACanvas: TCanvas; ARect: TRectF; ASpace: TTMSFMXPlannerPositionEmptySpace);
begin
  if ASpace = ppesTopLeft then
  begin
    ACanvas.Fill.Color := claWhite;
    ACanvas.Font.Size := 16;
    ACanvas.FillText(ARect, 'Commercials per channel (sec)', True, 1, [], TTextAlign.taCenter);
  end;
end;

procedure TForm1.TMSFMXPlanner1GetTimeText(Sender: TObject; AValue: Double;
  ARow: Integer; ASubUnit: Boolean; AKind: TTMSFMXPlannerCacheItemKind;
  var AText: string);
begin
  AText := FloatToStr(AValue);
end;

end.
