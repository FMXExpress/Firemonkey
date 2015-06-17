unit UDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TMSChart, FMX.TMSChartEditor, FMX.ListBox, UIConsts, Math, DateUtils
  {$if CompilerVersion > 25}
  ,FMX.Graphics
  {$endif}
  ;

type
  TForm1166 = class(TForm)
    TMSFMXChart1: TTMSFMXChart;
    Panel1: TPanel;
    Button1: TButton;
    TMSFMXChartEditorDialog1: TTMSFMXChartEditorDialog;
    Label2: TLabel;
    CheckBox1: TCheckBox;
    GroupBox1: TGroupBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    procedure Button1Click(Sender: TObject);
    procedure TMSFMXChart1SeriePointClick(Sender: TObject;
      APoint: TTMSFMXChartPoint);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TMSFMXChart1BeforeDrawSerieXValue(Sender: TObject;
      ACanvas: TCanvas; ASerie: TTMSFMXChartSerie;
      APosition: TTMSFMXChartXAxisPosition;
      var ADrawValue: TTMSFMXChartDrawXYValue; var ADefaultDraw: Boolean);
    procedure TMSFMXChart1GetSerieYValue(Sender: TObject;
      ASerie: TTMSFMXChartSerie; AIndex: Integer;
      AKind: TTMSFMXChartDrawXYValueKind; AValue: Double;
      var AValueString: string);
    procedure CheckBox1Change(Sender: TObject);
    procedure TMSFMXChart1BeforeDrawSerieMarker(Sender: TObject;
      ACanvas: TCanvas; ASerie: TTMSFMXChartSerie;
      var ADrawPoint: TTMSFMXChartDrawPoint; var ADefaultDraw: Boolean);
    procedure RadioButton1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure BuildChart;
  end;

var
  Form1166: TForm1166;

implementation

{$R *.fmx}

procedure TForm1166.BuildChart;
var
  I, J: Integer;
  s, si: TTMSFMXChartSerie;
  pt: TTMSFMXChartPoint;
  an: TTMSFMXChartAnnotation;
  c: TAlphaColor;
  idx: Integer;
begin
  TMSFMXChart1.BeginUpdate;
  TMSFMXChart1.Clear;
  idx := 0;
  if RadioButton2.IsChecked then
    idx := 1
  else if RadioButton3.IsChecked then
    idx := 2
  else if RadioButton4.IsChecked then
    idx := 3;

  case idx of
    0:
    begin
      TMSFMXChart1.Title.Text := 'Monthly Car Sales';
      TMSFMXChart1.Title.Height := 50;
      {$if CompilerVersion > 26}
      TMSFMXChart1.Title.TextHorizontalAlignment := TTextAlign.Leading;
      {$else}
      TMSFMXChart1.Title.TextHorizontalAlignment := TTextAlign.taLeading;
      {$endif}
      TMSFMXChart1.Title.FontColor := claDarkslategray;
      TMSFMXChart1.Title.Stroke.Color := claDarkslategray;
      TMSFMXChart1.Title.Font.Size := 20;
      TMSFMXChart1.Title.Font.Style := TMSFMXChart1.Title.Font.Style + [TFontStyle.fsBold];
      TMSFMXChart1.Legend.Position := lpTopRight;
      TMSFMXChart1.Legend.Left := -10;
      TMSFMXChart1.Legend.Font.Size := 15;
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
      for I := 0 to 2 do
      begin
        s := TMSFMXChart1.Series.Add;
        if I = 0 then
        begin
          s.YValues.Positions := [ypLeft];
          s.XValues.Positions := [xpBottom];
          s.XGrid.Visible := True;
          s.YGrid.Visible := True;
          s.XValues.Title.Text := 'Months of the year '+ inttostr(YearOf(Now));
          s.XValues.Title.FontColor := claDarkSlateGray;
          s.XValues.Title.Font.Size := 16;
          s.XValues.Title.Font.Style := s.YValues.Title.Font.Style + [TFontStyle.fsBold];
          {$if CompilerVersion > 26}
          s.XValues.Title.TextHorizontalAlignment := TTextAlign.Trailing;
	  {$else}
          s.XValues.Title.TextHorizontalAlignment := TTextAlign.taTrailing;
	  {$endif}
          s.XValues.Title.TextMargins.Right := 10;

          s.YValues.Title.Text := 'Amount of cars';
          s.YValues.Title.FontColor := claDarkSlateGray;
          s.YValues.Title.Font.Size := 16;
          s.YValues.Title.Font.Style := s.YValues.Title.Font.Style + [TFontStyle.fsBold];
          {$if CompilerVersion > 26}
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

        s.AutoXRange := arDisabled;
        s.AutoYRange := arDisabled;
        s.MinX := 0;
        s.MaxX := 11;
        s.MinY := 0;
        s.MaxY := 300;
        s.Mode := smStatistical;
        s.ChartType := ctBar;

        case I of
          0:
          begin
            s.LegendText := 'Mercedes';
	    {$if CompilerVersion > 26}
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
            s.LegendText := 'Bugatti';
	    {$if CompilerVersion > 26}
            s.Fill.Kind := TBrushKind.Gradient;
	    {$else}
            s.Fill.Kind := TBrushKind.bkGradient;
	    {$endif}
            s.Fill.Color := claOrange;
            s.Fill.Gradient.Points[0].Color := claOrange;
            s.Fill.Gradient.Points[1].Color := claOrangered;
            s.Stroke.Color := claDarkred;
          end;
          2:
          begin
            s.LegendText := 'Audi';
	    {$if CompilerVersion > 26}
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

        s.Bar.Spacing := 0;

        s.Labels.Fill.Color := s.Fill.Color;
        s.Labels.Stroke.Color := s.Stroke.Color;

        s.Fill.Gradient.StartPosition.X := 0;
        s.Fill.Gradient.StopPosition.X := 1.5;
        s.Fill.Gradient.StartPosition.Y := 0.5;
        s.Fill.Gradient.StopPosition.Y := 0.5;


        s.XValues.Angle := -90;
        s.YValues.MajorUnitFormat := '%g';
        s.Labels.Visible := True;
        s.Labels.Format := '%g';
        s.XValues.MajorUnitFont.Size := 14;
        s.YValues.MajorUnitFont.Size := 14;
        s.XValues.MinorUnitFont.Size := 14;
        s.YValues.MinorUnitFont.Size := 14;
        s.XValues.MajorUnitFontColor := claDarkslategray;
        s.XValues.MajorUnitTickMarkColor := claDarkslategray;
        s.YValues.MajorUnitFontColor := claDarkslategray;
        s.YValues.MajorUnitTickMarkColor := claDarkslategray;

        for J := 1 to 12 do
          s.AddPoint(RandomRange(50, 250), FormatSettings.LongMonthNames[J]);
      end;
    end;
    1:
    begin
      TMSFMXChart1.YAxis.Positions := [ypCenter];
      TMSFMXChart1.XAxis.Positions := [xpCenter];
      {$if CompilerVersion > 26}
      TMSFMXChart1.Title.TextHorizontalAlignment := TTextAlign.Center;
      {$else}
      TMSFMXChart1.Title.TextHorizontalAlignment := TTextAlign.taCenter;
      {$endif}
      TMSFMXChart1.Title.Text := 'Sine & Cosine';
      TMSFMXChart1.Title.FontColor := claDarkslategray;
      TMSFMXChart1.Title.Stroke.Color := claDarkslategray;
      TMSFMXChart1.Title.Font.Size := 20;
      TMSFMXChart1.Title.Font.Style := TMSFMXChart1.Title.Font.Style + [TFontStyle.fsBold];
      TMSFMXChart1.Legend.Visible := False;
      TMSFMXChart1.YAxis.Stroke.Color := claDarkslategray;
      TMSFMXChart1.XAxis.Stroke.Color := claDarkslategray;
      TMSFMXChart1.Stroke.Color := claDarkslategray;
      TMSFMXChart1.Fill.Color := claWhite;

      TMSFMXChart1.SeriesMargins.Left := 10;
      TMSFMXChart1.SeriesMargins.Top := 10;
      TMSFMXChart1.SeriesMargins.Right := 10;
      TMSFMXChart1.SeriesMargins.Bottom := 10;

      for I := 0 to 1 do
      begin
        s := TMSFMXChart1.Series.Add;
        s.ChartType := ctXYLine;
        s.AutoXRange := arDisabled;
        s.MinX := -5;
        s.MaxX := 5;
        s.AutoYRange := arCommon;
        if I = 0 then
        begin
          s.YValues.Positions := [ypCenter];
          s.XValues.Positions := [xpCenter];
          s.XGrid.Visible := True;
          s.YGrid.Visible := True;
          s.Stroke.Color := claRed;
          s.LegendText := 'Sine';
        end
        else
        begin
          s.YValues.Positions := [];
          s.XValues.Positions := [];
          s.Stroke.Color := claBlue;
          s.LegendText := 'Cosine';
        end;

        {$if CompilerVersion > 26}
        s.Markers.Fill.Kind := TBrushKind.Gradient;  
        {$else}
        s.Markers.Fill.Kind := TBrushKind.bkGradient;
        {$endif}
        s.Markers.Fill.Color := s.Stroke.Color;
        s.Markers.Fill.Gradient.Points[0].Color := s.Stroke.Color;
        s.Markers.Fill.Gradient.Points[1].Color := claWhite;
        s.Markers.Fill.Gradient.StopPosition.X := 1;
        s.Markers.Fill.Gradient.StopPosition.Y := 1;
        s.Markers.FillOpacity := 0.5;

        s.Markers.Stroke.Color := claBlack;

        s.XValues.MajorUnitFormat := '%.2f';
        s.Markers.Visible := True;

        for J := -500 to 500 do
        begin
          an := nil;
          if I = 0 then
          begin
            pt := s.AddXYPoint(J / 100, sin(J / 100));
            if J = -350 then
            begin
              an := pt.Annotations.Add;
              an.Text := 'The sine of an angle is the ratio '+#13#10+'of the length of the opposite side '+#13#10+'to the length of the hypotenuse.';
              an.OffsetY := -60;
              an.OffsetX := 100;
              an.BalloonDirection := bdDownLeft;
            end;
          end
          else
          begin
            pt := s.AddXYPoint(J / 100, cos(J / 100));
            if J = 200 then
            begin
              an := pt.Annotations.Add;
              an.Text := 'The cosine of an angle is the ratio '+#13#10+'of the length of the adjacent side '+#13#10+'to the length of the hypotenuse';
              an.OffsetX := 100;
              an.OffsetY := -30;
              an.BalloonDirection := bdDownLeft;
            end;
          end;

          if Assigned(an) then
          begin
            an.AutoSize := True;
            an.Shape := asBalloon;
            an.WordWrap := True;
            an.Font.Size := 10;
            an.FontColor := s.Stroke.Color;
            an.Stroke.Color := s.Stroke.Color;
            an.LineColor := s.Stroke.Color;
          end;
        end;
      end;
    end;
    2:
    begin
      TMSFMXChart1.Title.Text := 'Highest temperature in week ' + inttostr(WeekOf(Now)) + ' of ' + FormatSettings.LongMonthNames[MonthOf(Now)];
      TMSFMXChart1.Title.FontColor := claGreen;
      TMSFMXChart1.Title.Height := 50;
      TMSFMXChart1.Legend.Visible := True;
      TMSFMXChart1.YAxis.Positions := [ypLeft];
      TMSFMXChart1.XAxis.Positions := [xpBottom];
      TMSFMXChart1.Fill.Color := claLightgoldenrodyellow;

      TMSFMXChart1.SeriesMargins.Left := 0;
      TMSFMXChart1.SeriesMargins.Top := 0;
      TMSFMXChart1.SeriesMargins.Right := 0;
      TMSFMXChart1.SeriesMargins.Bottom := 0;

      for I := 0 to 2 do
      begin
        s := TMSFMXChart1.Series.Add;
        s.ChartType := ctStackedArea;
        s.AutoXRange := arCommon;
        s.AutoYRange := arCommonZeroBased;
        s.Enable3D := True;
        s.Mode := smStatistical;
        s.FillOpacity := 0.5;
        s.StrokeOpacity := 0.5;

        if I = 0 then
        begin
          s.YValues.Positions := [ypLeft];
          s.XValues.Positions := [xpBottom];
          s.XGrid.Visible := True;
          s.YGrid.Visible := True;
        end
        else
        begin
          s.YValues.Positions := [];
          s.XValues.Positions := [];
        end;

        s.Labels.Visible := True;
        s.Labels.Format := '%g C°';
        s.Labels.Mode := lmStacked;
        s.YValues.Title.Text := 'Temperature in degrees (C°)';
        s.YValues.Title.FontColor := claGreen;
        s.XValues.Title.Text := 'Week '+IntToStr(WeekOf(Now))+' of '+ FormatSettings.LongMonthNames[MonthOf(Now)];
        s.XValues.Title.FontColor := claGreen;
        s.YValues.MajorUnitFontColor := claGreen;
        s.XValues.MajorUnitFontColor := claGreen;
        s.YValues.MajorUnitFont.Size := 14;
        s.YValues.MinorUnitFontColor := claGreen;
        s.YValues.AutoUnits := False;
        s.YValues.MajorUnit := 5;
        s.YValues.MinorUnit := 1;
        s.YValues.MajorUnitFormat := '%g';
        s.MaxYOffsetPercentage := 10;

        case I of
          0:
          begin
            s.Fill.Color := claYellowgreen;
            s.Stroke.Color := claGreen;
            s.LegendText := 'Paris';
            for J := 1 to 7 do
              s.AddPoint(RandomRange(1, 10), FormatSettings.LongDayNames[J]);
          end;
          1:
          begin
            s.Fill.Color := claGreen;
            s.Stroke.Color := claDarkgreen;
            s.LegendText := 'Barcelona';
            for J := 1 to 7 do
              s.AddPoint(RandomRange(10, 20), FormatSettings.LongDayNames[J]);
          end;
          2:
          begin
            TAlphaColorRec(c).A := 255;
            TAlphaColorRec(c).R := 64;
            TAlphaColorRec(c).G := 93;
            TAlphaColorRec(c).B := 169;
            s.Fill.Color := c;
            s.Stroke.Color := c;

            s.LegendText := 'Shanghai';
            for J := 1 to 7 do
              s.AddPoint(RandomRange(10, 30), FormatSettings.LongDayNames[J]);
          end;
        end;
      end;
    end;
    3:
    begin
      TMSFMXChart1.Title.Text := 'Customer Satisfaction / Expenses';
      TMSFMXChart1.Title.Height := 50;
      {$if CompilerVersion > 26}
      TMSFMXChart1.Title.TextHorizontalAlignment := TTextAlign.Center;
      {$else}
      TMSFMXChart1.Title.TextHorizontalAlignment := TTextAlign.taCenter;
      {$endif}
      TMSFMXChart1.Title.FontColor := claDarkslategray;
      TMSFMXChart1.Title.Stroke.Color := claDarkslategray;
      TMSFMXChart1.Title.Font.Size := 20;
      TMSFMXChart1.Title.Font.Style := TMSFMXChart1.Title.Font.Style + [TFontStyle.fsBold];
      TMSFMXChart1.Legend.Position := lpTopRight;
      TMSFMXChart1.Legend.Left := -10;
      TMSFMXChart1.Legend.Font.Size := 15;
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
      s := TMSFMXChart1.Series.Add;
      s.ChartType := ctPie;
      s.AddPoint(9, MakeColor(254, 0, 0, 255), 'Very dissatisfied').Annotations.Add.Text := 'Very dissatisfied ' + Format('%g%%', [9.0]);
      s.AddPoint(50, MakeColor(76, 118, 32, 255), 'Very satisfied').Annotations.Add.Text := 'Very satisfied ' + Format('%g%%', [50.0]);
      s.AddPoint(14, MakeColor(113, 176, 46, 255), 'Somewhat satisfied').Annotations.Add.Text := 'Somewhat satisfied ' + Format('%g%%', [14.0]);
      s.AddPoint(27, MakeColor(255, 192, 0, 255), 'Somewhat dissatisfied').Annotations.Add.Text := 'Somewhat dissatisfied ' + Format('%g%%', [27.0]);
      s.Stroke.Color := claBlack;
      s.StrokeThickness := 0.5;
      s.Pie.Size := 350;
      s.Pie.AutoSize := False;
      s.Legend.Visible := True;
      s.LegendText := '';
      s.Pie.StartAngle := 90;
      s.Legend.Font.Size := 12;
      s.Pie.InnerSize := 200;
      s.Legend.Position := lpCenterCenter;
      s.Legend.Left := 0;
      s.Pie.Margins.Left := 75;
      si := TMSFMXChart1.Series.Add;
      si.ChartType := ctSizedPie;
      si.Pie.AutoSize := False;
      si.Labels.Visible := True;
      si.Labels.OffsetX := 0;
      si.Labels.OffsetY := 0;
      si.Labels.Format := '%.0m';
      {$if CompilerVersion > 26}
      si.Labels.Fill.Kind := TBrushKind.None;
      si.Labels.Stroke.Kind := TBrushKind.None;
      si.Stroke.Kind := TBrushKind.None;
      {$else}
      si.Labels.Fill.Kind := TBrushKind.bkNone;
      si.Labels.Stroke.Kind := TBrushKind.bkNone;
      si.Stroke.Kind := TBrushKind.bkNone;
      {$endif}
      si.Labels.FontColor := claWhite;
      si.Labels.Font.Size := 14;
      si.Labels.Font.Style := [TFontStyle.fsBold];
      si.Pie.Size := 300;
      si.LegendText := '';
      si.AddPoint(Random(100) + 40, MakeColor(69, 115, 167) , 'Meat and Fish').Explode := 30;
      si.AddPoint(Random(100) + 40, MakeColor(190, 20, 245), 'Fruit and Vegetables');
      si.AddPoint(Random(100) + 40, MakeColor(95, 52, 95), 'Meals out');
      si.AddPoint(Random(100) + 40, MakeColor(76, 132, 209), 'Groceries');
      si.AddPoint(Random(100) + 40, MakeColor(76, 200, 209), 'Alcohol');
      si.Legend.Font.Size := 14;
      si.Legend.Visible := True;
      si.Legend.Position := lpBottomCenter;
      si.Pie.Position := ppTopCenter;
      si.Pie.Margins.Top := 50;
      si.Pie.Margins.Bottom := 50;
    end;
  end;
  TMSFMXChart1.EndUpdate;
end;

procedure TForm1166.Button1Click(Sender: TObject);
begin
  TMSFMXChartEditorDialog1.Execute;
end;

procedure TForm1166.CheckBox1Change(Sender: TObject);
begin
  TMSFMXChart1.Interaction := CheckBox1.IsChecked;
  Label2.Visible := CheckBox1.IsChecked;
end;

procedure TForm1166.ComboBox1Change(Sender: TObject);
begin
  BuildChart;
end;

procedure TForm1166.FormCreate(Sender: TObject);
begin
  BuildChart;
end;

procedure TForm1166.RadioButton1Change(Sender: TObject);
begin
  Label2.Visible := not RadioButton3.IsChecked;
  CheckBox1.Visible := not RadioButton3.IsChecked;
  BuildChart;
end;

procedure TForm1166.TMSFMXChart1BeforeDrawSerieMarker(Sender: TObject;
  ACanvas: TCanvas; ASerie: TTMSFMXChartSerie;
  var ADrawPoint: TTMSFMXChartDrawPoint; var ADefaultDraw: Boolean);
begin
  ADefaultDraw := Frac(ADrawPoint.Reference.XValue) = 0;
end;

procedure TForm1166.TMSFMXChart1BeforeDrawSerieXValue(Sender: TObject;
  ACanvas: TCanvas; ASerie: TTMSFMXChartSerie;
  APosition: TTMSFMXChartXAxisPosition; var ADrawValue: TTMSFMXChartDrawXYValue;
  var ADefaultDraw: Boolean);
begin
  ADefaultDraw := not (RadioButton2.IsChecked and (ADrawValue.Value = 0));
end;

procedure TForm1166.TMSFMXChart1GetSerieYValue(Sender: TObject;
  ASerie: TTMSFMXChartSerie; AIndex: Integer;
  AKind: TTMSFMXChartDrawXYValueKind; AValue: Double; var AValueString: string);
begin
  if AKind = vkMinor then
    AValueString := '';
end;

procedure TForm1166.TMSFMXChart1SeriePointClick(Sender: TObject;
  APoint: TTMSFMXChartPoint);
var
  xv: String;
begin
  if RadioButton3.IsChecked then
    Exit;

  xv := APoint.XValueText;
  if xv = '' then
    xv := floattostr(APoint.XValue);
  ShowMessage(APoint.Serie.LegendText + ' Point at '+xv +' with value ' + floattostr(APoint.YValue) + ' Clicked');
end;

end.
