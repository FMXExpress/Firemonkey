unit UDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TMSBaseControl, FMX.TMSPlannerBase, FMX.TMSPlannerData, FMX.TMSPlanner,
  FMX.ListBox, FMX.Layouts, UIConsts, FMX.TMSBitmapContainer, FMX.Objects,
  FMX.Colors;

type
  TForm1 = class(TForm)
    TMSFMXPlanner1: TTMSFMXPlanner;
    Panel4: TPanel;
    Label1: TLabel;
    ComboBox1: TComboBox;
    TMSFMXBitmapContainer1: TTMSFMXBitmapContainer;
    Rectangle1: TRectangle;
    Rectangle2: TRectangle;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    lblPatient: TLabel;
    imgPatient: TImage;
    Panel2: TPanel;
    chkOperation: TCheckBox;
    Label4: TLabel;
    Image1: TImage;
    Label5: TLabel;
    cboCategory: TColorComboBox;
    Label6: TLabel;
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TMSFMXPlanner1AfterDrawItem(Sender: TObject; ACanvas: TCanvas;
      ARect: TRectF; AItem: TTMSFMXPlannerItem);
    procedure TMSFMXPlanner1IsDateTimeInActive(Sender: TObject;
      ADateTime: TDateTime; APosition: Integer; var AInActive: Boolean);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure TMSFMXPlanner1GetCustomContentPanel(Sender: TObject;
      AItem: TTMSFMXPlannerItem; var AContentPanel: TControl);
    procedure TMSFMXPlanner1CustomContentPanelToItem(Sender: TObject;
      AContentPanel: TControl; AItem: TTMSFMXPlannerItem);
    procedure TMSFMXPlanner1ItemToCustomContentPanel(Sender: TObject;
      AItem: TTMSFMXPlannerItem; AContentPanel: TControl);
    procedure TMSFMXPlanner1BeforeOpenInplaceEditor(Sender: TObject; AStartTime,
      AEndTime: TDateTime; APosition: Integer; AItem: TTMSFMXPlannerItem;
      var ACanOpen: Boolean);
    procedure TMSFMXPlanner1GetItemTitleText(Sender: TObject;
      AItem: TTMSFMXPlannerItem; AMode: TTMSFMXPlannerGetTextMode;
      var ATitle: string);
    procedure TMSFMXPlanner1BeforeDrawItemTitleText(Sender: TObject;
      ACanvas: TCanvas; ARect: TRectF; AItem: TTMSFMXPlannerItem;
      ATitle: string; var AAllow: Boolean);
    procedure TMSFMXPlanner1AfterOpenUpdateDialog(Sender: TObject; AStartTime,
      AEndTime: TDateTime; APosition: Integer; AItem: TTMSFMXPlannerItem);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  DateUtils;

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
  TMSFMXPlanner1.EditItem(TMSFMXPlanner1.Items[0]);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if TMSFMXPlanner1.IsEditing then
    TMSFMXPlanner1.CancelEditing;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  TMSFMXPlanner1.Interaction.UpdateMode := TTMSFMXPlannerUpdateMode(ComboBox1.ItemIndex);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  lbl: TLabel;
  res: TTMSFMXPlannerResource;
  it: TTMSFMXPlannerItem;
  dt: TDateTime;
begin
  Image1.Bitmap.Assign(TMSFMXBitmapContainer1.FindBitmap('warning'));
  label4.Text := 'Change the editing mode with the combobox on the top left. Click on the item to select it and click again to start editing.'+#13#10#13#10+
  '1. The patiënts for Dr. Sheryl Simmons and Dr. Mark Hall can be moved, sized and edited through the dialog and inplace editor.'+#13#10+
  '2. The patiënts for Dr. Gregory House cannot be moved nor sized, editing is limited to the dialog mode only and the content of the dialog is replaced with a custom dialog.';

  TMSFMXPlanner1.BitmapContainer := TMSFMXBitmapContainer1;
  TMSFMXPlanner1.Interaction.UpdateMode := pumDialog;
  TMSFMXPlanner1.Resources.Clear;
  TMSFMXPlanner1.Mode := pmDay;
  dt := Now;
  TMSFMXPlanner1.ModeSettings.StartTime := dt;

  TMSFMXPlanner1.PositionsAppearance.TopFont.Size := 18;
  TMSFMXPlanner1.PositionsAppearance.TopFont.Style := [TFontStyle.fsBold];

  res := TMSFMXPlanner1.Resources.Add;
  res.Name := 'Dr. Sheryl Simmons';
  res.Text := '<img width="36" src="nurse"/> ' + res.Name;
  res := TMSFMXPlanner1.Resources.Add;
  res.Name := 'Dr. Gregory House';
  res.Text := '<img width="36" src="surgeon"/> ' + res.Name;
  res := TMSFMXPlanner1.Resources.Add;
  res.Name := 'Dr. Mark Hall';
  res.Text := '<img width="36" src="doctor"/> ' + res.Name;

  TMSFMXPlanner1.ItemsAppearance.Stroke.Kind := TBrushKind.bkSolid;
  TMSFMXPlanner1.ItemsAppearance.Stroke.Color := claDarkgray;
  TMSFMXPlanner1.ItemsAppearance.ActiveStroke.Color := claGreen;
  TMSFMXPlanner1.ItemsAppearance.ActiveStroke.Kind := TBrushKind.bkSolid;
  TMSFMXPlanner1.Interaction.MouseEditMode := pmemSingleClickOnSelectedItem;

  TMSFMXPlanner1.TimeLine.DisplayStart := 8;
  TMSFMXPlanner1.TimeLine.DisplayEnd := 41;

  TMSFMXPlanner1.DefaultItem.Color := claWhitesmoke;
  TMSFMXPlanner1.DefaultItem.ActiveColor := claLightgreen;
  TMSFMXPlanner1.DefaultItem.ActiveFontColor := claGreen;
  TMSFMXPlanner1.DefaultItem.ActiveTitleFontColor := claGreen;

  TMSFMXPlanner1.Items.Clear;
  it := TMSFMXPlanner1.Items.Add;
  it.StartTime := Int(dt) + EncodeTime(8, 0, 0, 0);
  it.EndTime := it.StartTime + EncodeTime(1, 30, 0, 0);
  it.Title := 'John Appleseed';
  it.Text := 'Examination of the heart';
  it.DataString := 'heart';

  it := TMSFMXPlanner1.Items.Add;
  it.StartTime := Int(dt) + EncodeTime(9, 0, 0, 0);
  it.EndTime := it.StartTime + EncodeTime(2, 30, 0, 0);
  it.Title := 'Angela Parks';
  it.Text := '';

  it := TMSFMXPlanner1.Items.Add;
  it.Resource := 1;
  it.StartTime := Int(dt) + EncodeTime(9, 30, 0, 0);
  it.EndTime := it.StartTime + EncodeTime(3, 0, 0, 0);
  it.Movable := False;
  it.Sizeable := False;
  it.Title := 'John Appleseed';
  it.Text := 'Bypass surgery of the heart<br><ul><li>Heart diagram<li>Scalpel<li>Anesthesia</ul>';
  it.DataBoolean := False;
  it.DataString := 'heart';

  it := TMSFMXPlanner1.Items.Add;
  it.Resource := 2;
  it.StartTime := Int(dt) + EncodeTime(14, 30, 0, 0);
  it.EndTime := it.StartTime + EncodeTime(1, 30, 0, 0);
  it.Title := 'John Appleseed';
  it.Text := 'Heart surgery recovery';
  it.DataString := 'heart';

  lbl := TMSFMXPlanner1.GetEditingDialog.ResourceLabel;
  if Assigned(lbl) then
    lbl.Text := 'Doctor';

  Fill.Color := claWhite;
  Fill.Kind := TBrushKind.bkSolid;
  Rectangle1.Fill.Assign(TMSFMXPlanner1.GridCellAppearance.InActiveFill);
  Rectangle2.Fill.Assign(TMSFMXPlanner1.GridCellAppearance.Fill);

  TMSFMXPlanner1.TimeLine.ViewStart := Int(dt) + EncodeTime(6, 0, 0, 0);
end;

procedure TForm1.TMSFMXPlanner1AfterDrawItem(Sender: TObject; ACanvas: TCanvas;
  ARect: TRectF; AItem: TTMSFMXPlannerItem);
var
  bmp: TBitmap;
  rbmp: TRectF;
begin
  if TMSFMXPlanner1.HasItem(AItem.StartTime, AItem.EndTime, TMSFMXPlanner1.ResourceToPosition(AItem.Resource), AItem.Index, False) then
  begin
    bmp := TMSFMXBitmapContainer1.FindBitmap('warning');
    if Assigned(bmp) then
    begin
      rbmp := RectF(ARect.Right - 26, ARect.Bottom - 26, ARect.Right - 2, ARect.Bottom - 2);
      ACanvas.DrawBitmap(bmp, RectF(0, 0, bmp.Width, bmp.Height), rbmp, 1);
    end;
  end
  else if AItem.DataBoolean then
  begin
    bmp := TMSFMXBitmapContainer1.FindBitmap('ok');
    if Assigned(bmp) then
    begin
      rbmp := RectF(ARect.Left + 2, ARect.Bottom - 26, ARect.Left + 26, ARect.Bottom - 2);
      ACanvas.DrawBitmap(bmp, RectF(0, 0, bmp.Width, bmp.Height), rbmp, 1);
    end;
  end;
end;

procedure TForm1.TMSFMXPlanner1AfterOpenUpdateDialog(Sender: TObject;
  AStartTime, AEndTime: TDateTime; APosition: Integer;
  AItem: TTMSFMXPlannerItem);
begin
  if AItem.Resource = 1 then
    TMSFMXPlanner1.GetEditingDialog(AItem.Index).ButtonRemove.Visible := False;
end;

procedure TForm1.TMSFMXPlanner1BeforeDrawItemTitleText(Sender: TObject;
  ACanvas: TCanvas; ARect: TRectF; AItem: TTMSFMXPlannerItem; ATitle: string;
  var AAllow: Boolean);
begin
  if AItem.DataBoolean then
    ACanvas.Font.Size := 12;
end;

procedure TForm1.TMSFMXPlanner1BeforeOpenInplaceEditor(Sender: TObject;
  AStartTime, AEndTime: TDateTime; APosition: Integer;
  AItem: TTMSFMXPlannerItem; var ACanOpen: Boolean);
begin
  ACanOpen := APosition <> 1;
end;

function FindChild(AContainer: TControl; AName: String): TControl;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to AContainer.ControlsCount - 1 do
  begin
    if AContainer.Controls[I].Name = AName then
    begin
      Result := AContainer.Controls[I];
      Break;
    end;
    Result := FindChild(AContainer.Controls[I], AName);
  end;
end;

procedure TForm1.TMSFMXPlanner1CustomContentPanelToItem(Sender: TObject;
  AContentPanel: TControl; AItem: TTMSFMXPlannerItem);
var
  c: TCheckBox;
  b: TColorComboBox;
begin
  c := FindChild(AContentPanel, 'chkOperation') as TCheckBox;
  b := FindChild(AContentPanel, 'cboCategory') as TColorComboBox;
  AItem.BeginUpdate;
  AItem.Color := b.Color;
  AItem.DataBoolean := c.IsChecked;
  AItem.EndUpdate;
end;

procedure TForm1.TMSFMXPlanner1GetCustomContentPanel(Sender: TObject;
  AItem: TTMSFMXPlannerItem; var AContentPanel: TControl);
begin
  if Assigned(AItem) and (AItem.Resource = 1) then
    AContentPanel := Panel1;
end;

procedure TForm1.TMSFMXPlanner1GetItemTitleText(Sender: TObject;
  AItem: TTMSFMXPlannerItem; AMode: TTMSFMXPlannerGetTextMode;
  var ATitle: string);
begin
  if AItem.DataBoolean and (AMode = pgtmDrawing) then
    ATitle := AItem.Title + ' [Operation Successful]';
end;

procedure TForm1.TMSFMXPlanner1IsDateTimeInActive(Sender: TObject;
  ADateTime: TDateTime; APosition: Integer; var AInActive: Boolean);
var
  dt: TDateTime;
begin
  dt := TMSFMXPlanner1.ModeSettings.StartTime;
  case APosition of
    0:
    begin
      AInActive := AInActive or ((CompareDateTime(ADateTime, Int(dt) + EncodeTime(11, 30, 0, 0)) in [GreaterThanValue, EqualsValue]) and
        (CompareDateTime(ADateTime, Int(dt) + EncodeTime(13, 30, 0, 0)) = LessThanValue)) or (CompareDateTime(ADateTime, Int(dt) + EncodeTime(18, 30, 0, 0)) in [GreaterThanValue, EqualsValue]);
    end;
    1:
    begin
      AInActive := AInActive or ((CompareDateTime(ADateTime, Int(dt) + EncodeTime(11, 30, 0, 0)) in [GreaterThanValue, EqualsValue]) and
        (CompareDateTime(ADateTime, Int(dt) + EncodeTime(13, 30, 0, 0)) = LessThanValue)) or (CompareDateTime(ADateTime, Int(dt) + EncodeTime(20, 30, 0, 0)) in [GreaterThanValue, EqualsValue]);

      AInActive := AInActive and not (CompareDateTime(ADatetime, Int(dt) + EncodeTime(7, 0, 0, 0)) = EqualsValue);
      AInActive := AInActive and not (CompareDateTime(ADatetime, Int(dt) + EncodeTime(7, 30, 0, 0)) = EqualsValue);
    end;
    2:
    begin
      AInActive := AInActive or ((CompareDateTime(ADateTime, Int(dt) + EncodeTime(11, 30, 0, 0)) in [GreaterThanValue, EqualsValue]) and
        (CompareDateTime(ADateTime, Int(dt) + EncodeTime(13, 30, 0, 0)) = LessThanValue)) or (CompareDateTime(ADateTime, Int(dt) + EncodeTime(16, 30, 0, 0)) in [GreaterThanValue, EqualsValue]);
    end;
  end;
end;

procedure TForm1.TMSFMXPlanner1ItemToCustomContentPanel(Sender: TObject;
  AItem: TTMSFMXPlannerItem; AContentPanel: TControl);
var
  l: TLabel;
  c: TCheckBox;
  img: TImage;
  b: TColorComboBox;
begin
  l := FindChild(AContentPanel, 'lblPatient') as TLabel;
  l.Text := 'Patiënt: ' + AItem.Title;
  c := FindChild(AContentPanel, 'chkOperation') as TCheckBox;
  c.IsChecked := AItem.DataBoolean;
  img := FindChild(AContentPanel, 'imgPatient') as TImage;
  img.Bitmap.Assign(TMSFMXBitmapContainer1.FindBitmap(AItem.DataString));
  b := FindChild(AContentPanel, 'cboCategory') as TColorComboBox;
  b.Color := AItem.Color;
end;

end.
