unit Unit1;
{$I TeeDefs.inc}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMXTee.Engine, FMXTee.Procs, FMXTee.Chart, FMX.Layouts, FMXTee.Commander,
  FMX.Edit, FMXTee.Series, System.UIConsts, FMXTee.Tools,
  {$IFDEF D21PARALLEL}
  System.Threading,
  {$ENDIF}
  FMX.Controls.Presentation, FMX.ComboEdit, FMX.ComboTrackBar;

type
  // Small class just to do an infinite loop:
  TUpdateThread=class(TThread)
  private
    IForm : TForm;
  public
    procedure Execute; override;
  end;

  TForm1 = class(TForm)
    TeeCommander1: TTeeCommander;
    Layout1: TLayout;
    Chart1: TChart;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    Timer1: TTimer;
    CBParallel: TCheckBox;
    TrackBar1: TComboTrackBar;
    Label1: TLabel;
    Label2: TLabel;
    ComboFlat1: TComboTrackBar;
    ChartTool1: TRepaintMonitor;
    Label3: TLabel;
    CheckBox1: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Chart1Resize(Sender: TObject);
    procedure Chart1AfterDraw(Sender: TObject);
    procedure Chart1Scroll(Sender: TObject);
    procedure Chart1UndoZoom(Sender: TObject);
    procedure Chart1Zoom(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure CheckBox4Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure CBParallelChange(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure ComboFlat1Change(Sender: TObject);
    procedure Label3Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
  private
    { Private declarations }
    IThread : TUpdateThread;

    Started : Boolean;

    procedure AddSeries;
    procedure AppIdle(Sender: TObject; var Done:Boolean);
    procedure DoStuff;
    procedure RefreshTotal;
    procedure ReuseXValues(DoReuse:Boolean);
    procedure UpdateData;
    procedure UpdateSeries(Index:Integer);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

// Number of data points per Series:
var
  Num_Points:Integer=10000;

// Create several series and add lots of random data:
procedure TForm1.AddSeries;
var t : Integer;
    tmp : TChartValue;
    tmpMin,tmpMax,
    tt : Integer;
begin
  Chart1.FreeAllSeries;

  for t:=0 to Round(TrackBar1.Value)-1 do
  with Chart1.AddSeries(TFastLineSeries) as TFastLineSeries do
  begin
    // Much faster to draw "All" points using Polyline, than
    // drawing each point as a separate line segment:
    DrawStyle:=flAll;

    // Applies to "old" GDI canvas only, increases paint speed:
    FastPen:=True;

    // Initialize series X and Y values:

    XValues.Count:=Num_Points;

    // Optimization:
    // As we are creating all Series with the same XValues, we can simply
    // reuse the first series XValues array into all the other series:

    if t=0 then
       SetLength(XValues.Value,Num_Points)
    else
       XValues.Value:=Chart1[0].XValues.Value;

    YValues.Count:=Num_Points;
    SetLength(YValues.Value,Num_Points);

    // Add Num_Points random data to series arrays:

    tmp:=t*1000+Random(400);

    // Use a 1000 range:
    tmpMin:=t*1000;
    tmpMax:=(t+1)*1000;

    for tt:=0 to Num_Points-1 do
    begin
      // X value is simply the point index.
      // Here we optimize filling the array only for the first Series,
      // as all series share the same X values:
      if t=0 then
         XValues.Value[tt]:=tt;

      tmp:=tmp+Random(60)-29.5;

      // Check random data is inside the range we want (1000)
      if tmp>tmpMax then
         tmp:=tmpMax
      else
      if tmp<tmpMin then
         tmp:=tmpMin;

      // Set Y value:
      YValues.Value[tt]:=tmp;
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // (FastCalc Applies to 32bit only).
  // Use 32bit assembler to increase speed:
  Chart1.Axes.FastCalc:=True;

  Chart1.Legend.Hide;

  // Enable or disable multi-CPU usage (speeds up FastLine series with "dsAll" DrawStyle)
  CBParallelChange(Self);

  {$IFDEF D12}
  //ListBox1.Items.Add('Direct2D');
  {$ENDIF}

  //ListBox1.ItemIndex:=0;

  ComboFlat1.Value:={$IFDEF ANDROID}1000{$ELSE}20000{$ENDIF};
  TrackBar1.Value:=10;

  AddSeries;

  // Hide grid lines:
  Chart1.Axes.Bottom.Grid.Hide;
  Chart1.Axes.Left.Grid.Hide;

  Chart1.Axes.Bottom.Texts.Selected.Hover.Font.Color:=clWhite;
  Chart1.Axes.Left.Texts.Selected.Hover.Font.Color:=clWhite;

  (*
  // Disable OpenGL lighting to increase speed:
  TeeOpenGL1.Light0.Visible:=False;
  TeeOpenGL1.ShadeQuality:=False;
  TeeOpenGL1.AmbientLight:=0; // <-- 0 means Full Light (Ambient = from 0 to 100)
  *)

  // Do not paint bevels:
  Chart1.BevelOuter:=bvNone;

  //ListBox1Click(Self);

  // Cosmetic change colors and text font colors:
  Chart1.Color:=claBlack;
//  Chart1.Title.Font.Color:=RGBA(200,150,50,0);
  Chart1.Axes.Left.Texts.Font.Color:=clWhite;
  Chart1.Axes.Bottom.Texts.Font.Color:=clWhite;

  // FormQuality not possible to change at runtime:
  CheckBox1.Visible:=False;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Application.OnIdle:=AppIdle;
end;

procedure TForm1.Label3Click(Sender: TObject);
begin
  TeeGotoURL(0,'http://www.steema.com');
end;

procedure TForm1.AppIdle(Sender: TObject; var Done:Boolean);
begin
  if not Started then
  begin
    CheckBox4Change(Self);
    Started:=True;
  end;
end;

type
  TChartAccess=class(TCustomAxisPanel);

// Experimental:
// Use all available CPU processors to calculate all Series data in parallel.
procedure TForm1.CBParallelChange(Sender: TObject);
begin
  if CBParallel.IsChecked then
     TChartAccess(Chart1).ParallelThreads:=TTeeCPU.NumberOfProcessors
  else
     TChartAccess(Chart1).ParallelThreads:=1;
end;

procedure TForm1.Chart1AfterDraw(Sender: TObject);
begin
  // Speed optimization:
  // After chart is displayed, tell all Series to conservate and reuse their
  // X coordinates, to avoid recalculating them again:
  ReuseXValues(True);
end;

procedure TForm1.Chart1Resize(Sender: TObject);
begin
  // When zooming or scrolling, or resizing the chart, force X coordinates
  // recalculation again:
  ReuseXValues(False);
end;

procedure TForm1.Chart1Scroll(Sender: TObject);
begin
  Chart1Resize(Self);
end;

procedure TForm1.Chart1UndoZoom(Sender: TObject);
begin
  Chart1Resize(Self);
end;

procedure TForm1.Chart1Zoom(Sender: TObject);
begin
  Chart1Resize(Self);
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  if CheckBox1.IsChecked then
     Quality:=TCanvasQuality.HighQuality
  else
     Quality:=TCanvasQuality.HighPerformance;
end;

procedure TForm1.CheckBox3Change(Sender: TObject);
var t : Integer;
begin
  // When DrawAllPoints = False, it means repeated X position points will
  // not be painted.
  for t:=0 to Chart1.SeriesCount-1 do
      TFastLineSeries(Chart1[t]).DrawAllPoints:=CheckBox3.IsChecked;
end;

procedure TForm1.CheckBox4Change(Sender: TObject);
begin
  // Switch between using a TTimer or a TThread to do the infinite loop:

  Timer1.Enabled:=not CheckBox4.IsChecked;

  if CheckBox4.IsChecked then
  begin
    IThread:=TUpdateThread.Create(True);
    IThread.FreeOnTerminate:=True;

    {$IFNDEF ANDROID}
    IThread.Priority:=tpNormal;
    {$ENDIF}

    IThread.IForm:=Self;

    {$IFDEF D9}
    IThread.Start;
    {$ELSE}
    IThread.Resume;
    {$ENDIF}
  end
  else
  if Assigned(IThread) then
     IThread.Terminate;
end;

procedure TForm1.ComboFlat1Change(Sender: TObject);
begin
  Num_Points:=10000; //StrToInt(ComboFlat1.Text);

  RefreshTotal;
  AddSeries;
end;

// Modify all series, all points "Y" values:

procedure TForm1.UpdateData;
var t : Integer;
begin
  if CBParallel.IsChecked then
     TTeeCPU.ParallelFor(0,Chart1.SeriesCount-1,UpdateSeries)
  else
  for t:=0 to Chart1.SeriesCount-1 do
      UpdateSeries(t);
end;

procedure TForm1.UpdateSeries(Index:Integer);
const
  BufferSize=100;

var tmpMin,
    tmpMax,
    t: Integer;
    tmp0 : Array[0..BufferSize-1] of TChartValue;
    val : TChartValue;
    tmp : TChartValues;
begin
  with Chart1[Index] do
  begin
    tmp:=YValues.Value;

    // In "Buffer mode", move data points to simulate data scrolling

    //if CBMode.IsChecked then
    begin
      // Backup the first points:
      for t := 0 to BufferSize-1 do
          tmp0[t]:=tmp[t];

      // Scroll data from right to left:
      for t := 0 to Num_Points-1-BufferSize do
          tmp[t]:=tmp[t+BufferSize];

      // Restore the first points to the last:
      for t := 0 to BufferSize-1 do
          tmp[Num_Points-1-BufferSize+t]:=tmp0[t];

    end
    (*
    else
    begin
      tmpMin:=Index*1000;
      tmpMax:=(Index+1)*1000;

      // Modify all values, increase them with a random quantity:
      for t:=0 to Num_Points-1 do
      begin
        val:=tmp[t]+Random(32)-15.5;

        if (val<tmpMax) and (val>tmpMin) then
           tmp[t]:=val;
      end;
    end
    *)
  end;
end;

procedure TForm1.RefreshTotal;
begin
  // Text1.Text:=
end;

type
  TFastAccess=class(TFastLineSeries);

// Speed optimization:
// Enable or disable reusing the X coordinates, to avoid recalculating them.
procedure TForm1.ReuseXValues(DoReuse:Boolean);
var t : Integer;
begin
  for t:=0 to Chart1.SeriesCount-1 do
      TFastAccess(Chart1[t]).ReuseXValues:=DoReuse;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  UpdateData;
  Chart1.Invalidate;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  RefreshTotal;
  AddSeries;
end;

procedure TForm1.DoStuff;
begin
  UpdateData;
  Chart1.Invalidate;
  Application.ProcessMessages;
end;

{ TUpdateThread }

// Simple infinite loop
procedure TUpdateThread.Execute;
begin
  inherited;

  while not Terminated do
  begin
    Synchronize(TForm1(IForm).DoStuff);
  end;
end;

end.
