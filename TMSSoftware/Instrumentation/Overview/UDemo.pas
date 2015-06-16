unit UDemo;

interface

uses
  SysUtils, Types, UITypes, Classes, Variants, FMX.TMSGauge, FMX.TMSScope,
  FMX.Controls, FMX.TMSSpinner, FMX.TMSLed, FMX.TMSLedmeter,
  FMX.TMS7segled, FMX.Filter.Effects, FMX.Objects, FMX.TMSBaseControl,
  FMX.TabControl, FMX.Forms, FMX.TMSMatrixlabel, FMX.Types, FMX.StdCtrls,
  FMX.Effects;

type
  TForm472 = class(TForm)
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    Timer1: TTimer;
    TMSFMXCircularGauge2: TTMSFMXCircularGauge;
    StyleBook1: TStyleBook;
    TMSFMXRotarySwitch1: TTMSFMXRotarySwitch;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    TMSFMX7SegLEDShape1: TTMSFMX7SegLEDShape;
    TMSFMXSetPointShape1: TTMSFMXSetPointShape;
    Label2: TLabel;
    Label3: TLabel;
    TMSFMXNeedleShape1: TTMSFMXNeedleShape;
    Label4: TLabel;
    TMSFMXSectionShape1: TTMSFMXSectionShape;
    Label5: TLabel;
    TMSFMXSetPointShape2: TTMSFMXSetPointShape;
    TMSFMXSetPointShape3: TTMSFMXSetPointShape;
    TMSFMXSetPointShape4: TTMSFMXSetPointShape;
    TMSFMX7SegLED1: TTMSFMX7SegLED;
    TMSFMXLEDMeter1: TTMSFMXLEDMeter;
    TMSFMXLEDScope1: TTMSFMXLEDScope;
    TMSFMXLEDBar1: TTMSFMXLEDBar;
    TMSFMXLEDBar2: TTMSFMXLEDBar;
    TMSFMXLEDBar3: TTMSFMXLEDBar;
    TMSFMXMatrixLabel1: TTMSFMXMatrixLabel;
    TMSFMXSpinner1: TTMSFMXSpinner;
    CheckBox1: TCheckBox;
    TMSFMXScope1: TTMSFMXScope;
    TMSFMXClock2: TTMSFMXClock;
    TMSFMXLinearGauge1: TTMSFMXLinearGauge;
    TMSFMXLinearGauge2: TTMSFMXLinearGauge;
    TMSFMXCircularGauge1: TTMSFMXCircularGauge;
    Timer3: TTimer;
    TMSFMXKnobSwitch1: TTMSFMXKnobSwitch;
    TMSFMXCompass1: TTMSFMXCompass;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TMSFMXClock1TimeChanged(Sender: TObject);
    procedure TMSFMXScope1NeedData(Sender: TObject; Channel: Integer;
      var Value: Single);
    procedure TabControl1Change(Sender: TObject);
    procedure Timer3Timer(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure TMSFMXSpinner1SelectedValueChanged(Sender: TObject;
      Column: Integer; SelectedValue: Double;
      RangeType: TTMSFMXSpinnerRangeType);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form472: TForm472;
  val1, val2, val3: Single;

implementation

{$R *.fmx}

procedure TForm472.CheckBox1Change(Sender: TObject);
begin
  TMSFMXClock2.Active := CheckBox1.IsChecked;
end;

procedure TForm472.FormCreate(Sender: TObject);
begin
  TabControl1.ActiveTab := TabItem1;
  TMSFMXScope1.Channels[0].Color := TAlphaColorRec.Red;
  TMSFMXScope1.Channels[1].Color := TAlphaColorRec.White;
  TMSFMXScope1.Channels[2].Color := TAlphaColorRec.Lime;
  Timer3.Enabled := False;
  Timer1.Enabled := True;
  TMSFMXClock2.ShowSeconds := False;
end;

procedure TForm472.TabControl1Change(Sender: TObject);
begin
  case TabControl1.ActiveTab.Index of
    0:
    begin
      Timer1.Enabled := True;
      Timer3.Enabled := False;
      TMSFMXScope1.Active := False;
      TMSFMXClock2.Active := False;
      TMSFMXMatrixLabel1.AutoScroll := False;
    end;
    1:
    begin
      Timer1.Enabled := False;
      Timer3.Enabled := True;
      TMSFMXScope1.Active := False;
      TMSFMXClock2.Active := False;
      TMSFMXMatrixLabel1.AutoScroll := True;
    end;
    2:
    begin
      TMSFMXMatrixLabel1.AutoScroll := False;
      Timer1.Enabled := False;
      Timer3.Enabled := False;
      TMSFMXScope1.Active := True;
      TMSFMXClock2.Active := True;
    end;
  end;
end;

procedure TForm472.Timer1Timer(Sender: TObject);
begin
  Randomize;
  TMSFMXCircularGauge1.Value := Random(100);
  Randomize;
  TMSFMXCircularGauge2.Value := Random(100);
  Randomize;
  TMSFMXLinearGauge1.Value := Random(100);
  Randomize;
  TMSFMXLinearGauge2.Value := Random(100);
  Randomize;
  TMSFMXKnobSwitch1.Value := Random(6);
  Randomize;
  TMSFMXRotarySwitch1.Value := Random(6);

  TMSFMXCircularGauge1.SetPoints[0].Value := Random(100);
  TMSFMXCircularGauge1.SetPoints[1].Value := Random(100);
  TMSFMXCircularGauge1.SetPoints[2].Value := Random(100);
  TMSFMXCircularGauge1.SetPoints[3].Value := Random(100);
end;

procedure TForm472.Timer3Timer(Sender: TObject);
var
  i: integer;
begin
  TMSFMX7SegLED1.Value := TMSFMX7SegLED1.Value + 0.1;
  Randomize;
  TMSFMXLEDMeter1.Value := Random(50);

  if TMSFMXLEDBar1.Value = TMSFMXLEDBar1.Count then
    TMSFMXLEDBar1.Value := 0
  else
    TMSFMXLEDBar1.Value := TMSFMXLedBar1.Value + 1;

  if TMSFMXLEDBar2.Value = TMSFMXLEDBar2.Count then
    TMSFMXLEDBar2.Value := 0
  else
    TMSFMXLEDBar2.Value := TMSFMXLedBar2.Value + 2;

  if TMSFMXLEDBar3.Value = TMSFMXLEDBar3.Count then
    TMSFMXLEDBar3.Value := 0
  else
    TMSFMXLEDBar3.Value := TMSFMXLedBar3.Value + 3;

  Randomize;
  for I := 0 to TMSFMXLEDScope1.Channels.Count - 1 do
  begin
    TMSFMXLEDScope1.Channels[I].Value := Random(50);
    if TMSFMXLEDScope1.Channels[I].PeakValue > 45 then
      TMSFMXLEDScope1.Channels[I].PeakValue := 0;
  end;
end;

procedure TForm472.TMSFMXClock1TimeChanged(Sender: TObject);
var
  h, m, s, ms: Word;
begin
  DecodeTime(TMSFMXClock2.ClockTime, h, m, s, ms);
  TMSFMXSpinner1.Columns[0].SelectedValue := h;
  TMSFMXSpinner1.Columns[1].SelectedValue := m;
end;

procedure TForm472.TMSFMXScope1NeedData(Sender: TObject; Channel: Integer;
  var Value: Single);
begin
  case Channel of
    0:
    begin
      Value := Sin(val1) * 100;
      val1 := val1 + 0.1;
    end;
    1:
    begin
      Value := cos(val2) * 50;
      val2 := val2 + 0.1;
    end;
    2:
    begin
      Value := ArcTan(val3) * 50;
      val3 := val3 + 0.1;
    end;
  end;
end;

procedure TForm472.TMSFMXSpinner1SelectedValueChanged(Sender: TObject;
  Column: Integer; SelectedValue: Double; RangeType: TTMSFMXSpinnerRangeType);
var
  h, m, s, ms: Word;
begin
  if not CheckBox1.IsChecked then
  begin
    DecodeTime(TMSFMXClock2.ClockTime, h, m, s, ms);
    case Column of
      0: h := Round(SelectedValue);
      1: m := Round(SelectedValue);
      2: s := Round(SelectedValue);
    end;

    TMSFMXClock2.ClockTime := EncodeTime(h, m, s, ms);
  end;
end;

end.
