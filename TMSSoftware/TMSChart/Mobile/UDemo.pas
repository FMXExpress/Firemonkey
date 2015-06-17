unit UDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TMSChart, UIConsts,
  DateUtils, Math
  {$if CompilerVersion > 25}
  ,FMX.Graphics
  {$endif}
  ;

type
  TForm1166 = class(TForm)
    TMSFMXChart1: TTMSFMXChart;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1166: TForm1166;

implementation

{$R *.fmx}

procedure TForm1166.FormCreate(Sender: TObject);
var
  I, J: Integer;
  s: TTMSFMXChartSerie;
begin
  TMSFMXChart1.BeginUpdate;
  TMSFMXChart1.Clear;
  TMSFMXChart1.Title.Text := 'Daily Car Sales';
  TMSFMXChart1.Title.Height := 30;
  {$if compilerversion > 26}
  TMSFMXChart1.Title.TextHorizontalAlignment := TTextAlign.Leading;
  {$else}
  TMSFMXChart1.Title.TextHorizontalAlignment := TTextAlign.taLeading;
  {$endif}
  TMSFMXChart1.Title.FontColor := claDarkslategray;
  TMSFMXChart1.Title.Stroke.Color := claDarkslategray;
  TMSFMXChart1.Title.Font.Size := 16;
  TMSFMXChart1.Title.Font.Style := TMSFMXChart1.Title.Font.Style + [TFontStyle.fsBold];
  TMSFMXChart1.Legend.Position := lpTopRight;
  TMSFMXChart1.Legend.Left := -3;
  TMSFMXChart1.Legend.Top := 3;
  TMSFMXChart1.Legend.Font.Size := 10;
  TMSFMXChart1.Legend.Visible := True;
  TMSFMXChart1.Legend.FontColor := claDarkslategray;
  TMSFMXChart1.Legend.Stroke.Color := claDarkslategray;
  TMSFMXChart1.YAxis.Stroke.Color := claDarkslategray;
  TMSFMXChart1.XAxis.Stroke.Color := claDarkslategray;
  TMSFMXChart1.YAxis.Positions := [ypLeft];
  TMSFMXChart1.XAxis.Positions := [xpBottom];
  TMSFMXChart1.Stroke.Color := claDarkslategray;
  TMSFMXChart1.Fill.Color := claWhite;

  TMSFMXChart1.SeriesMargins.Left := 0;
  TMSFMXChart1.SeriesMargins.Top := 0;
  TMSFMXChart1.SeriesMargins.Right := 0;
  TMSFMXChart1.SeriesMargins.Bottom := 0;

  TMSFMXChart1.Series.Clear;
  for I := 0 to 1 do
  begin
    s := TMSFMXChart1.Series.Add;
    if I = 0 then
    begin
      s.YValues.Positions := [ypLeft];
      s.XValues.Positions := [xpBottom];
      s.XGrid.Visible := True;
      s.YGrid.Visible := True;
      s.XValues.Title.Text := 'Week '+ inttostr(WeekOf(Now)) + ' of '+ FormatSettings.LongMonthNames[MonthOf(Now)];
      s.XValues.Title.FontColor := claDarkSlateGray;
      s.XValues.Title.Font.Size := 12;
      s.XValues.Title.Font.Style := s.YValues.Title.Font.Style + [TFontStyle.fsBold];
      {$if compilerversion > 26}
      s.XValues.Title.TextHorizontalAlignment := TTextAlign.Trailing;
      {$else}
      s.XValues.Title.TextHorizontalAlignment := TTextAlign.taTrailing;
      {$endif}
      s.XValues.Title.TextMargins.Right := 10;

      s.YValues.Title.Text := 'Amount of cars';
      s.YValues.Title.FontColor := claDarkSlateGray;
      s.YValues.Title.Font.Size := 12;
      s.YValues.Title.Font.Style := s.YValues.Title.Font.Style + [TFontStyle.fsBold];
      {$if compilerversion > 26}
      s.YValues.Title.TextHorizontalAlignment := TTextAlign.Leading;
      {$else}
      s.YValues.Title.TextHorizontalAlignment := TTextAlign.taLeading;
      {$endif}
    end
    else
    begin
      s.YValues.Positions := [];
      s.XValues.Positions := [];
    end;

    s.AutoXRange := arEnabled;
    s.AutoYRange := arDisabled;
    s.MinY := 0;
    s.MaxY := 300;
    s.Mode := smStatistical;
    s.ChartType := ctBar;

    case I of
      0:
      begin
        s.LegendText := 'Mercedes';

        {$if compilerversion > 26}
        s.Fill.Kind := TBrushKind.Gradient;
	{$else}
        s.Fill.Kind := TBrushKind.bkGradient;
	{$endif}
        s.Fill.Color := claLightslategray;
        s.Fill.Gradient.Points[0].Color := claLightslategray;
        s.Fill.Gradient.Points[1].Color := claDarkslategray;
        s.Stroke.Color := claBlack;
        s.Labels.FontColor := claWhite;
      end;
      1:
      begin
        s.LegendText := 'Audi';
        {$if compilerversion > 26}
        s.Fill.Kind := TBrushKind.Gradient;
	{$else}
        s.Fill.Kind := TBrushKind.bkGradient;
	{$endif}
        s.Fill.Color := claLimegreen;
        s.Fill.Gradient.Points[0].Color := claLimeGreen;
        s.Fill.Gradient.Points[1].Color := claGreenyellow;
        s.Stroke.Color := claDarkgreen;
      end;
    end;

    s.Labels.Fill.Color := s.Fill.Color;
    s.Labels.Stroke.Color := s.Stroke.Color;

    s.Fill.Gradient.StartPosition.X := 0;
    s.Fill.Gradient.StopPosition.X := 1.5;
    s.Fill.Gradient.StartPosition.Y := 0.5;
    s.Fill.Gradient.StopPosition.Y := 0.5;


    s.YValues.MajorUnitFormat := '%g';
    s.Labels.Visible := True;
    s.Labels.Format := '%g';
    s.XValues.MajorUnitFont.Size := 12;
    s.YValues.MajorUnitFont.Size := 12;
    s.XValues.MinorUnitFont.Size := 12;
    s.YValues.MinorUnitFont.Size := 12;
    s.XValues.MajorUnitFontColor := claDarkslategray;
    s.XValues.MajorUnitTickMarkColor := claDarkslategray;
    s.YValues.MajorUnitFontColor := claDarkslategray;
    s.YValues.MajorUnitTickMarkColor := claDarkslategray;

    for J := 1 to 7 do
      s.AddPoint(RandomRange(50, 250), FormatSettings.ShortDayNames[J]);
  end;


  s := TMSFMXChart1.Series.Add;
  s.ChartType := ctLine;
  s.ShowInLegend := False;
  s.Stroke.Color := claRed;
  s.AutoYRange := arDisabled;
  s.YValues.Positions := [];
  s.XValues.Positions := [];
  s.Mode := smStatistical;
  s.AutoXRange := arCommonZeroBased;
  s.Markers.Visible := True;
  s.Markers.Fill.Color := claRed;
  s.Markers.Stroke.Color := claDarkred;
  s.MinY := 0;
  s.MaxY := 300;

  for J := 1 to 7 do
  begin
    s.Points.Add.YValue := RandomRange(40, 150);
  end;

  TMSFMXChart1.EndUpdate;
end;

end.
