unit Hampton_U;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Ani, FMX.Objects,
  FMX.Layouts, FMX.Memo, FMX.Menus, FMX.Types3D, FMX.Layers3D, FMX.StdCtrls,
  FMX.Viewport3D, FMX.Graphics;

type
  TForm1 = class(TForm)
    RoundRect1: TRoundRect;
    PathAni: TPathAnimation;
    Path1: TPath;
    cbUseSample: TCheckBox;
    Memo1: TMemo;
    Timer1: TTimer;
    btnSavePath: TButton;
    Image1: TImage;
    tbDuration: TTrackBar;
    Label1: TLabel;
    btnLoadPath: TButton;
    btnRunThePath: TButton;
    tbSampleRate: TTrackBar;
    Label2: TLabel;
    Circle1: TCircle;
    ColorAnimation1: TColorAnimation;
    btnLoadMaze: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    btnBacktrack: TButton;
    Circle2: TCircle;
    Circle3: TCircle;
    Circle4: TCircle;
    Circle5: TCircle;
    cbMoveTo: TCheckBox;
    Text1: TText;
    btnReset: TButton;
    Viewport3D1: TViewport3D;
    Image2: TImage;
    ColorAnimation2: TColorAnimation;
    btnSaveMaze: TButton;
    gbMazeRecord: TGroupBox;
    gbPathOptions: TGroupBox;
    procedure PathAniProcess(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure btnSavePathClick(Sender: TObject);
    procedure btnRunThePathClick(Sender: TObject);
    procedure btnLoadPathClick(Sender: TObject);
    procedure PathAniFinish(Sender: TObject);
    procedure btnBacktrackClick(Sender: TObject);
    procedure btnLoadMazeClick(Sender: TObject);
    procedure cbUseSampleChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure FloatAnimation1Finish(Sender: TObject);
    procedure btnSaveMazeClick(Sender: TObject);
  private
    { Private declarations }
    FCurveData: TPathData;
    FCumeData: TPathData;
    FMazeData: TPathData;
    { Public declarations }
  public
    CumeCount: integer;
    SampleSpotTrigger: boolean;
    FinishLine1, FinishLine2: TPointF;  // defines the finish
    procedure ReadMaze;
    function PointInLineSegment(Point1, Point2,Point3, Point4: TPointf):boolean;
      // holds first two params to CurveTo
    property CurveData: TPathData read FCurveData write FCurveData;
      // incrementally stored sampled path
    property CumeData: TPathData read FCumeData write FCumeData;
      // path for the maze
    property MazeData: TPathData read FMazeData write FMazeData;
      // path for the maze, sans MoveTos

  end;

var
  Form1: TForm1;
  SnapshotOn: boolean;  // Shows the timer has been triggered

implementation

{$R *.fmx}

uses System.uiconsts, Math;

procedure TForm1.btnBacktrackClick(Sender: TObject);
begin
  if not cbUseSample.IsChecked then
    MazeData.LineTo(MazeData.Points[MazeData.Count -2].point);
end;

procedure TForm1.btnLoadPathClick(Sender: TObject);
var
  PathStrs:TStringList;
  i: integer;
  IsMoveTo: boolean;
  MazePoints, AMazePoint: TStringList;
  P: TPointF;
begin
  OpenDialog1.InitialDir := GetCurrentDir;
  OpenDialog1.FileName := 'AnimPath.txt';
  OpenDialog1.Filter := '*.txt';
  if not OpenDialog1.Execute then
    exit;
  Path1.Data.Clear;
  PathStrs := TStringList.Create;
  MazePoints := TStringList.Create;
  AMazePoint := TStringList.Create;
  try
    if CumeData = nil  then
      CumeData := TPathData.Create
    else
      CumeData.Clear;
    PathStrs.LoadFromFile(OpenDialog1.FileName);
    if PathStrs[1][1] = 'L' then
      ShowMessage('Loaded file appears to be a maze file, not a path file.');
    for I := 0  to PathStrs.Count -1 do
      begin
        IsMoveTo := pos('M',PathStrs[i]) = 1;
        PathStrs[i] := copy(PathStrs[i], 2, Length(PathStrs[i])-1);
        AMazePoint.DelimitedText := PathStrs[i];
        P := TPointF.Create(StrToInt(AMazePoint[0]),StrToInt(AMazePoint[1]));
        if IsMoveTo then
        begin
          Path1.Data.MoveTo(P);
        end
        else
        begin

          if i mod 3 = 0 then
          begin
            Path1.Data.CurveTo(CumeData[0].Point, CumeData[1].Point, P);
            CumeData.Clear;
          end
          else
            CumeData.LineTo(P);
        end;
        AMazePoint.Clear;
      end;
  finally
    PathAni.Path := Path1.Data;
    Text1.Text := 'Loaded animation path '+ OpenDialog1.FileName;
    btnRunThePath.Enabled := true;
    PathStrs.Free;
    MazePoints.Free;
    AMazePoint.Free;
    CumeData.Clear;
  end;
end;

procedure TForm1.btnLoadMazeClick(Sender: TObject);
begin
  if MazeData = nil  then
    MazeData := TPathData.Create;
  OpenDialog1.FileName := 'MazePath1.txt';
  OpenDialog1.Filter := '*.txt';
  OpenDialog1.InitialDir := GetCurrentDir;
  if OpenDialog1.Execute then
    ReadMaze;
  if MazeData.Points[1].Kind = TPathPointKind.ppCurveTo then
    Text1.Text := 'Warning: Loaded path, not a maze!';
  Path1.Data := MazeData;
end;

procedure TForm1.btnResetClick(Sender: TObject);
begin
  Circle1.Position.Point := TPointF.Create(-1,-1);
  Circle2.Visible := false;
  Circle3.Visible := false;
  Circle4.Visible := false;
  Circle5.Visible := false;
  Path1.Data.Clear;
  if Assigned(CumeData) then
    CumeData.Clear;
  Path1.OnMouseMove := Panel1MouseMove;
  Text1.Text := 'Drag through the maze';
  ColorAnimation2.Enabled := false;
end;

procedure TForm1.btnRunThePathClick(Sender: TObject);
begin
  RoundRect1.Position.X := -1;
  RoundRect1.Position.Y := -1;
  PathAni.Duration := tbDuration.Value;
  if PathAni.Enabled = false then
    PathAni.Enabled := True;

//  PathAni.Start;
end;

procedure TForm1.btnSaveMazeClick(Sender: TObject);
var
  i: integer;
  MazePathStrings: TStringList;
  ActionType: String;
begin
  SaveDialog1.InitialDir := GetCurrentDir;
  SaveDialog1.FileName := 'AnimPath.txt';
  SaveDialog1.Filter := '*.txt';
  if not SaveDialog1.Execute then
    exit;
  if not cbUseSample.IsChecked then
  begin
    MazePathStrings:= TStringList.Create;
    try
      // put a new maze path in a file
      for I := 0  to MazeData.Count - 1 do
      begin
        if MazeData[i].Kind = TPathPointKind.ppMoveTo then
          ActionType := 'M'
        else
          ActionType := 'L';  // only using MoveTo/LineTo
        MazePathStrings.Add(ActionType + IntToStr(Round(MazeData[i].Point.X))+','+IntToStr(Round(MazeData[i].Point.Y)));
      end;
      MazePathStrings.SaveToFile(SaveDialog1.FileName);
    finally
      MazePathStrings.Free;
    end;
    exit;
  end;

end;

procedure TForm1.btnSavePathClick(Sender: TObject);
var
  i: integer;
  aStr: string;
  PathFileContents: TStringList;
//  MazePathStrings: TStringList;
  ActionType: String;
begin
  SaveDialog1.InitialDir := GetCurrentDir;
  SaveDialog1.FileName := 'AnimPath.txt';
  SaveDialog1.Filter := '*.txt';
  if not SaveDialog1.Execute then
    exit;

    // otherwise you're going to save a text version of the sampled path

  PathFileContents:= TStringList.Create;
  try
    for i := 0 to Path1.Data.Count - 1 do
    begin
      if Path1.Data[i].Kind = TPathPointKind.ppMoveTo then
        ActionType := 'M'
      else
        ActionType := 'C';  // only using MoveTo/CurveTo
      aStr := format('%s%d,%d',[ActionType, Round(Path1.Data[i].Point.X), Round(Path1.Data[i].Point.Y)]);
      PathFileContents.Add(aStr);
    end;
    PathFileContents.SaveToFile(SaveDialog1.FileName);
  finally
    PathFileContents.Free;
  end;

end;

procedure TForm1.cbUseSampleChange(Sender: TObject);
begin
  cbMoveTo.Enabled := not TCheckBox(Sender).IsChecked;
  btnSavePath.Enabled := TCheckBox(Sender).IsChecked;
  btnSaveMaze.Enabled := not TCheckBox(Sender).IsChecked;
  btnBacktrack.Enabled := not TCheckBox(Sender).IsChecked;
  btnReset.Visible := not TCheckBox(Sender).IsChecked;
  Label2.Enabled := not TCheckBox(Sender).IsChecked;
  if not (TCheckBox(Sender).IsChecked) and assigned(MazeData) then
    MazeData.Clear;
  if not (TCheckBox(Sender).IsChecked)  then
    Path1.OnMouseMove := nil;
//  tbSampleRate.Enabled := not TCheckBox(Sender).IsChecked;
end;

procedure TForm1.FloatAnimation1Finish(Sender: TObject);
begin
  Viewport3D1.Visible := false;
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  Text1.Text :=  'Drag through the maze, start at the entrance.'
end;

procedure TForm1.Panel1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if CumeData = nil  then
    CumeData := TPathData.Create;
  if MazeData = nil  then
    MazeData := TPathData.Create;
  if CurveData = nil  then
    CurveData := TPathData.Create;
  btnReset.Visible := false;
  ColorAnimation2.Enabled := false;

  if not cbUseSample.IsChecked then
  begin
      // we want to "record" points for a new maze
    if MazeData.Count = 0 then
    begin
      Text1.Text := 'Recording new maze...';
      MazeData.MoveTo(TPointF.Create(0,0));
    end;
    if cbMoveTo.IsChecked then
    begin
      MazeData.MoveTo(TPointF.Create(X,Y));
    end
    else
      MazeData.LineTo(TPointF.Create(X,Y));
    Path1.Data := MazeData;
    exit;
  end;

    // otherwise we're continuously sampling mouse movement;
    // load the maze path

  if MazeData.Count = 0 then
    ReadMaze;

    // put in a prefatory path to the maze entrance

  CumeData.MoveTo(TPointF.Create(0,0));
  CumeData.LineTo(TPointF.Create(0,Y));
  CumeData.LineTo(TPointF.Create(30,Y));
  CumeCount := 0;

    // reflect desired sample rate

  Timer1.Interval := Round( tbSampleRate.Value);
  Timer1.Enabled := true;
end;

procedure TForm1.Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
const
  CIRCLE_OFFSET=12;
var
  i: integer;
begin
  if not SnapshotOn then
     exit;
  if ssLeft in Shift then
  begin
    Text1.Text := 'Go!';
    for I := 0 to MazeData.Count -2 do
    begin

        // skip first point of each continuous sequence

      if MazeData.Points[i+1].Kind = TPathPointKind.ppMoveTo then
        continue;

          // have you crossed the finish line?

      if PointInLineSegment(TPointF.Create(X,Y),
        CumeData[CumeData.Count-1].Point, FinishLine1, FinishLine2) then
      begin
         Text1.Text := 'YAY!';
         ColorAnimation2.Enabled := true;
         btnReset.Visible := true;
         Circle1.Position.Point := TPointF.Create(-1,-1);
         Circle2.Visible := false;
         Circle3.Visible := false;
         Circle4.Visible := false;
         Circle5.Visible := false;
//         Path1.Data.Clear;
         Path1.OnMouseMove := Panel1MouseMove;
         Viewport3D1.Visible := true;
         btnSavePath.Enabled := true;
         btnRunThePath.Enabled := true;
         exit;
      end;

        // ... or have you crossed another line?

      if PointInLineSegment(TPointF.Create(X,Y),
        CumeData[CumeData.Count-1].Point, MazeData.Points[i].Point,MazeData.Points[i+1].Point)
      then
       begin
         Circle2.Visible := true;
         Circle3.Visible := true;
         Circle4.Visible := true;
         Circle5.Visible := true;

         Circle2.Position.Point := TPointF.Create(X-CIRCLE_OFFSET,Y-CIRCLE_OFFSET);
         Circle3.Position.Point := TPointF.Create( CumeData[CumeData.Count-1].Point.X-CIRCLE_OFFSET,CumeData[CumeData.Count-1].Point.y-CIRCLE_OFFSET);
         Circle4.Position.Point := TPointF.Create( MazeData.Points[i].Point.X-CIRCLE_OFFSET,MazeData.Points[i].Point.Y-CIRCLE_OFFSET);

         Circle5.Position.Point := TPointF.Create( MazeData.Points[i+1].Point.X-CIRCLE_OFFSET,MazeData.Points[i+1].Point.Y-CIRCLE_OFFSET);
         Text1.Text := 'You hit a wall!';
         btnReset.Visible := true;
         Path1.OnMouseMove := nil;
         break;
       end;
    end;

    inc(CumeCount);
    CurveData.LineTo(TPointF.Create(X,Y)); // use LineTo to save points for CurveTo
    if CumeCount = 3 then
    begin
      CumeData.CurveTo(CurveData[0].Point, CurveData[1].Point, TPointF.Create(X,Y));
      Path1.Data := CumeData;

      CumeCount := 0;
      CurveData.Clear;
    end;
    if not ColorAnimation1.Enabled then
      ColorAnimation1.Enabled := true;
    if SampleSpotTrigger then
    begin
      Circle1.Position.X := x;
      Circle1.Position.y := y;
      SampleSpotTrigger := false;
    end;
  end;
  SnapshotOn := false;

end;

procedure TForm1.Panel1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  i: integer;
  aStr: string;
begin
  Timer1.Enabled := false;
  for i := 0 to Path1.Data.Count - 1 do
  begin
    aStr := format('%d,%d',[Round(Path1.Data[i].Point.X), Round(Path1.Data[i].Point.Y)]);
    Memo1.Lines.Add(aStr);
  end;
  ColorAnimation1.Enabled := false;
  btnSavePath.Enabled := true;
  btnRunThePath.Enabled := true;
  Text1.Text := 'Game ended by mouse release, reset to start again.';
  btnReset.Visible := true;
end;

procedure TForm1.PathAniFinish(Sender: TObject);
begin
  TPathAnimation(Sender).Enabled := false;
  ColorAnimation2.Enabled := false;
end;

procedure TForm1.PathAniProcess(Sender: TObject);
begin
  PathAni.Path := Path1.Data;
end;

function TForm1.PointInLineSegment( Point1, Point2, Point3, Point4: TPointf): boolean;
var
  SegLength: single;
  SinValue, CosValue: single;
  AdjX: single;
  Intsct: single;
begin
  Result := false;

    // simple checks, non-null and no common points

  if (Point1.X = 0) or (Point1.Y = 0) or
     (Point2.X = 0) or (Point2.Y = 0) or
     (Point3.X = 0) or (Point3.Y = 0) or
     (Point4.X = 0) or (Point4.Y = 0) then
     exit;
  if ((Point1.X = Point2.X) and (Point1.Y = Point2.Y)) or
     (Point3.X = Point4.X) and (Point3.Y = Point4.Y) then
    exit;
  if ((Point1.X = Point3.X) and (Point1.Y = Point3.Y)) or
     ((Point2.X = Point3.X) and (Point2.Y = Point3.Y)) or
     ((Point2.X = Point4.X) and (Point2.Y = Point4.Y)) or
     ((Point1.X = Point4.X) and (Point1.Y = Point4.Y)) then
    exit;

    // center first point

  Point2.X := Point2.X - Point1.X;
  Point2.Y := Point2.Y - Point1.Y;
  Point3.X := Point3.X - Point1.X;
  Point3.Y := Point3.Y - Point1.Y;
  Point4.X := Point4.X - Point1.X;
  Point4.Y := Point4.Y - Point1.Y;

    // hypotenize

  SegLength := sqrt(Point2.X * Point2.X+ Point2.Y * Point2.Y);

  SinValue := Point2.X / SegLength;
  CosValue := Point2.Y / SegLength;

    // rotate the triangle

  AdjX := Point3.X * SinValue + Point3.Y * CosValue;
  Point3.Y := Point3.Y * SinValue - Point3.X * CosValue;
  Point3.X := AdjX;

  AdjX := Point4.X * SinValue + Point4.Y * CosValue;
  Point4.Y := Point4.Y * SinValue - Point4.X * CosValue;
  Point4.X := AdjX;

  if ((Point3.Y < 0) and (Point4.Y < 0 )) or
     ((Point3.Y >= 0) and (Point4.Y >= 0 ))
  then
    exit;

  Intsct := Point4.X + (Point3.X - Point4.X) * Point4.Y / (Point4.Y - Point3.Y);

    // check that the intersection is in segment range

  if (Intsct < 0) or (Intsct > SegLength) then
    exit;
  Result := true;
end;

procedure TForm1.ReadMaze;
var
  I: Integer;
  P: TPointF;
  IsMoveTo: boolean;  // we're only using MoveTo or LineTo
  MazePoints, AMazePoint, FinishLinePts, FLPoint: TStringList;
begin
  MazePoints := TStringList.Create;
  AMazePoint := TStringList.Create;
  FinishLinePts := TStringList.Create;
  FLPoint := TStringList.Create;
  try
    if FileExists(OpenDialog1.FileName) then
      MazePoints.LoadFromFile(OpenDialog1.FileName)
    else
    if FileExists('HamptonMaze.txt') then
    begin
      MazePoints.LoadFromFile('HamptonMaze.txt');
      Text1.Text := 'HamptonMaze.txt loaded';
    end
    else
      raise exception.Create('no maze file found!');
    if FileExists('FinishLine.txt') then
      FinishLinePts.LoadFromFile('FinishLine.txt')
    else
      raise exception.Create('no FinishLine.txt file found!');
    if MazePoints[1][1] = 'C' then
      ShowMessage('Loaded file appears to be a path file, not a maze file.');
    for I := 0  to MazePoints.Count -1 do
    begin
      IsMoveTo := pos('M',MazePoints[i]) = 1;
      MazePoints[i] := copy(MazePoints[i], 2, Length(MazePoints[i])-1);
      AMazePoint.DelimitedText := MazePoints[i];
      P := TPointF.Create(StrToInt(AMazePoint[0]),StrToInt(AMazePoint[1]));
      if IsMoveTo then
      begin
        MazeData.MoveTo(P);
      end
      else
      begin
        MazeData.LineTo(P);
      end;
      AMazePoint.Clear;
    end;
    for I := 0  to FinishLinePts.Count -1 do
    begin
      FinishLinePts[i] := copy(FinishLinePts[i], 2, Length(FinishLinePts[i])-1);
      FLPoint.DelimitedText := FinishLinePts[i];
      if i = 0 then
        FinishLine1 := TPointF.Create(StrToInt(FLPoint[0]),StrToInt(FLPoint[1]))
      else
        FinishLine2 := TPointF.Create(StrToInt(FLPoint[0]),StrToInt(FLPoint[1]));
      FLpoint.Clear;
    end;

  finally
    MazePoints.Free;
    AMazePoint.Free;
    FinishLinePts.Free;
    FLPoint.Free;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if CumeData = nil  then
    CumeData := TPathData.Create;
  SnapshotOn := true;
  if ColorAnimation1.Enabled = false then
    ColorAnimation1.Enabled := true;
  SampleSpotTrigger := true;
end;

initialization
   SnapshotOn := false;
finalization
   Form1.CumeData.Free;
end.
