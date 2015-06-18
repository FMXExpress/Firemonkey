unit LifeEngine;

interface

uses System.Types, System.SysUtils, System.Classes;

type
  TLifeBoard = array of array of Byte;
  TNeighborCount = 0..8;
  TNeighborCounts = set of TNeighborCount;

  ELifeEngine = class(Exception);

  TLifeEngine = class
  private
    type
      TLifeThread = class(TThread)
        FLifeEngine: TLifeEngine;
        FOriginalBoard, FNewBoard: TLifeBoard;
        FGensPerSecond: Double;
        FElapsed: Int64;
        FParallel: LongBool;
        FSurvival, FBirth: TNeighborCounts;
        FUpdating: Boolean;
        procedure SetParallel(Value: LongBool);
      protected
        procedure UpdateGeneration;
        procedure ProcessCells(Sender: TObject; AIndex: Integer);
        procedure Execute; override;
      public
        constructor Create(ALifeEngine: TLifeEngine);
        property Parallel: LongBool read FParallel write SetParallel;
      end;
  private
    FLifeBoard: TLifeBoard;
    FSurvival, FBirth: TNeighborCounts;
    FDescription: string;
    FLifeThread: TLifeThread;
    FParallel: Boolean;
    FBoardSize: TSize;
    FGensPerSecond, FMaxGensPerSecond: Double;
    FGraphCount: Int64;
    FGenerationCount: Int64;
    FUpdateRate: Integer;
    FOnUpdate: TNotifyEvent;
    function GetRunning: Boolean;
    function GetParallel: Boolean;
    procedure SetParallel(Value: Boolean);
    procedure DoUpdate;
    procedure SetUpdateRate(const Value: Integer);
  public
    constructor Create(const ABoardSize: TSize);
    destructor Destroy; override;
    procedure Clear;
    procedure LoadPattern(const AFileName: string); overload;
    procedure LoadPattern(AStream: TStream); overload;
    procedure Start;
    procedure Stop;
    property Description: string read FDescription;
    property LifeBoard: TLifeBoard read FLifeBoard;
    property Parallel: Boolean read GetParallel write SetParallel;
    property Running: Boolean read GetRunning;
    property GensPerSecond: Double read FGensPerSecond;
    property GenerationCount: Int64 read FGenerationCount;
    property MaxGensPerSecond: Double read FMaxGensPerSecond;
    property UpdateRate: Integer read FUpdateRate write SetUpdateRate;

    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
  end;

implementation

uses
  System.Math, System.Diagnostics, System.Threading;

{ TLifeEngine }

procedure TLifeEngine.Clear;
var
  NewBoard: TLifeBoard;
begin
  if Running then
    raise ELifeEngine.Create('Cannot clear life board while running');
  SetLength(NewBoard, FBoardSize.cx, FBoardSize.cy);
  FLifeBoard := NewBoard;
  FGenerationCount := 0;
end;

constructor TLifeEngine.Create(const ABoardSize: TSize);
begin
  inherited Create;
  FBoardSize := ABoardSize;
  FSurvival := [2,3];
  FBirth := [3];
  FUpdateRate := 60;
  Clear;
end;

destructor TLifeEngine.Destroy;
begin

  inherited;
end;

procedure TLifeEngine.DoUpdate;
begin
  if Assigned(FOnUpdate) then
    FOnUpdate(Self);
end;


function TLifeEngine.GetParallel: Boolean;
begin
  if FLifeThread <> nil then
    Result := FLifeThread.Parallel
  else
    Result := FParallel;
end;

function TLifeEngine.GetRunning: Boolean;
begin
  Result := FLifeThread <> nil;
end;

procedure TLifeEngine.LoadPattern(const AFileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadPattern(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TLifeEngine.LoadPattern(AStream: TStream);
var
  X, Y: Integer;
  Pattern, Data, Description: TStringList;
  Size: TSize;
  Offset, Origin: TPoint;
  Line: string;
  CurLine: Integer;

  function ProcessPattern(APattern, AData, ADescription: TStrings; var ASize: TSize;
    var AOffset: TPoint; var LineNum: Integer;
    var ASurvival, ABirth: TNeighborCounts): Boolean;
  var
    I: Integer;
    Line: string;
    InPattern: Boolean;

    function GetNumber(const ALine: string; var Index: Integer): Integer;
    var
      Start: Integer;
    begin
      while (Index <= Length(ALine)) and ((ALine[Index] < '0') or (ALine[Index] > '9')) and (ALine[Index] <> '-') and (ALine[Index] <> '+') do
         Inc(Index);
      Start := Index;
      while (Index <= Length(ALine)) and (((ALine[Index] >= '0') and (ALine[Index] <= '9')) or (ALine[Index] = '-') or (ALine[Index] = '+')) do
        Inc(Index);
      Result := StrToIntDef(Copy(ALine, Start, Index - Start), 0);
    end;

  begin
    FillChar(ASize, SizeOf(ASize), 0);
    FillChar(AOffset, SizeOf(AOffset), 0);
    InPattern := False;
    while LineNum < APattern.Count do
    begin
      Line := APattern[LineNum];
      if (LineNum = 0) and not SameText(Line, '#Life 1.05') then
        raise Exception.Create('Invalid .lif or .life format.')
      else if (Length(Line) > 0) and (Line[1] <> '#') then
      begin
        if not InPattern then
          AData.Clear;
        InPattern := True;
        Inc(ASize.cy);
        ASize.cx := Max(ASize.cx, Length(Line));
        AData.Add(Line);
      end else if (Length(Line) > 1) and (Line[1] = '#') then
      begin
        if InPattern then
          Break;
        if UpCase(Line[2]) = 'P' then
        begin
          I := 3;
          AOffset.X := GetNumber(Line, I);
          AOffset.Y := GetNumber(Line, I);
        end else if UpCase(Line[2]) = 'N' then
        begin
          ASurvival := [2,3];
          ABirth := [3];
        end else if UpCase(Line[2]) = 'R' then
        begin
          ASurvival := [];
          ABirth := [];
          I := 3;
          while (Line[I] < '0') or (Line[I] > '8') do Inc(I);
          while (Line[I] >= '0') and (Line[I] <= '8') do
          begin
            Include(ASurvival, TNeighborCount(StrToInt(Line[I])));
            Inc(I);
          end;
          if Line[I] = '/' then
            while (Line[I] >= '0') and (Line[I] <= '8') do
            begin
              Include(ABirth, TNeighborCount(StrToInt(Line[I])));
              Inc(I);
            end;
        end else if (UpCase(Line[2]) = 'D') or (UpCase(Line[2]) = 'C') then
           ADescription.Add(Copy(Line, 3, MaxInt));
      end;
      Inc(LineNum);
    end;
    Result := LineNum < APattern.Count;
    if not Running then
      DoUpdate;
  end;

begin
  Pattern := TStringList.Create;
  try
    Data := TStringList.Create;
    try
      Description := TStringList.Create;
      try
        Pattern.LoadFromStream(AStream);
        Pattern.Add('#E');
        CurLine := 0;
        while ProcessPattern(Pattern, Data, Description, Size, Offset, CurLine, FSurvival, FBirth) do
        begin
          if (Size.cx > Length(FLifeBoard)) or (Size.cy > Length(FLifeBoard[0])) then
            raise Exception.CreateFmt('Pattern too large for current board size.  Pattern Size (%d, %d)', [Size.cx, Size.cy]);
          Origin := Point(Length(FLifeBoard) div 2 + Offset.X, Length(FLifeBoard[0]) div 2 + Offset.Y);
          for Y := 0 to Data.Count - 1 do
          begin
            Line := Data[Y];
            for X := 1 to Length(Line) do
              if Line[X] = '*' then
                FLifeBoard[Origin.X + (X - 1), Origin.Y + Y] := 1
              else
                FLifeBoard[Origin.X + (X - 1), Origin.Y + Y] := 0;
          end;
        end;
        FDescription := Description.Text;
      finally
        Description.Free;
      end;
    finally
      Data.Free;
    end;
  finally
    Pattern.Free;
  end;
end;


procedure TLifeEngine.SetParallel(Value: Boolean);
begin
  if FLifeThread <> nil then
    FLifeThread.Parallel := Value;
  FParallel := Value;
end;

procedure TLifeEngine.SetUpdateRate(const Value: Integer);
begin
  AtomicExchange(FUpdateRate, Value);
end;

procedure TLifeEngine.Start;
begin
  if not Running then
  begin
    FLifeThread := TLifeThread.Create(Self);
    FLifeThread.Start;
  end else
    raise ELifeEngine.Create('Life Engine is already running');
end;

procedure TLifeEngine.Stop;
begin
  if Running then
    FreeAndNil(FLifeThread)
  else
    raise ELifeEngine.Create('Life Engine is not running');
end;

{ TLifeEngine.TLifeThread }

constructor TLifeEngine.TLifeThread.Create(ALifeEngine: TLifeEngine);
begin
  inherited Create(True);
  FLifeEngine := ALifeEngine;
  FSurvival := ALifeEngine.FSurvival;
  FBirth := ALifeEngine.FBirth;
  FParallel := ALifeEngine.Parallel;
end;

procedure TLifeEngine.TLifeThread.Execute;
var
  Update, Timer: TStopwatch;
  I: Integer;
begin
  NameThreadForDebugging('Life Thread');
  Update := TStopwatch.StartNew;
  while not Terminated do
  begin
    Timer := TStopwatch.StartNew;
    if Length(FNewBoard) > 0 then
    begin
      FOriginalBoard := FNewBoard;
      FNewBoard := nil;
    end else
      FOriginalBoard := FLifeEngine.FLifeBoard;
    SetLength(FNewBoard, Length(FOriginalBoard), Length(FOriginalBoard[0]));
    if FParallel then
    begin
        TParallel.For(Low(FOriginalBoard), High(FOriginalBoard),
          procedure (Value: Integer)
          begin
            ProcessCells(nil, Value);
          end);
    end else
      for I := Low(FOriginalBoard) to High(FOriginalBoard) do
        ProcessCells(nil, I);
    FGensPerSecond := {FFrequency / (EndTicks - StartTicks)} Timer.Frequency / Timer.ElapsedTicks;
    if Update.ElapsedTicks >= Update.Frequency div FLifeEngine.UpdateRate then
    begin
      FUpdating := True;
      Synchronize(UpdateGeneration);
      Assert(not FUpdating);
      Update := TStopwatch.StartNew;
    end;
    Inc(FLifeEngine.FGenerationCount);
    //Sleep(100);
    FElapsed := Timer.ElapsedTicks;
  end;
end;

procedure TLifeEngine.TLifeThread.ProcessCells(Sender: TObject; AIndex: Integer);
type
  TNeighbor = (nbAboveLeft, nbAbove, nbAboveRight, nbLeft, nbRight, nbBelowLeft, nbBelow, nbBelowRight);
const
  NeighborDelta: array[TNeighbor] of TPoint = (
    (X: -1; Y: -1),
    (X:  0; Y: -1),
    (X:  1; Y: -1),
    (X: -1; Y:  0),
    (X:  1; Y:  0),
    (X: -1; Y:  1),
    (X:  0; Y:  1),
    (X:  1; Y:  1));

  function NeighborIsOccupied(ANeighbor: TNeighbor; Column, Row: Integer): Boolean;
  var
    Delta: TPoint;

    function WrapCoord(Index: Integer; ALow, AHigh: Integer): Integer; inline;
    begin
      Result := Index;
      if Result < ALow then
        Result := AHigh
      else if Result > AHigh then
        Result := ALow;
    end;

  begin
    Delta := NeighborDelta[ANeighbor];
    Result :=  FOriginalBoard[WrapCoord(Column + Delta.X, Low(FOriginalBoard), High(FOriginalBoard)),
      WrapCoord(Row + Delta.Y, Low(FOriginalBoard[0]), High(FOriginalBoard[0]))] <> 0;
  end;

  function CountNeighbors(Column, Row: Integer): TNeighborCount;
  var
    N: TNeighbor;
  begin
    Result := 0;
    for N := Low(TNeighbor) to High(TNeighbor) do
      if NeighborIsOccupied(N, Column, Row) then
        Inc(Result);
  end;

var
  I: Integer;
  NeighborCount: TNeighborCount;
begin
  for I := Low(FOriginalBoard[AIndex]) to High(FOriginalBoard[AIndex]) do
  begin
    NeighborCount := CountNeighbors(AIndex, I);
    if (FOriginalBoard[AIndex, I] <> 0) and (NeighborCount in FSurvival) then
      FNewBoard[AIndex, I] := FOriginalBoard[AIndex, I]  // lives to next gen if occupied
    else if (FOriginalBoard[AIndex, I] = 0) and (NeighborCount in FBirth) then
      FNewBoard[AIndex, I] := 1  // comes to life
    else
      FNewBoard[AIndex, I] := 0; // always dies
  end;
end;


procedure TLifeEngine.TLifeThread.SetParallel(Value: LongBool);
begin
  AtomicExchange(Integer(FParallel), Integer(Value));
end;

procedure TLifeEngine.TLifeThread.UpdateGeneration;
begin
  FLifeEngine.FLifeBoard := FNewBoard;
  FOriginalBoard := nil;
  FNewBoard := nil;
  FLifeEngine.FGensPerSecond := FGensPerSecond;
  FLifeEngine.FGraphCount := FLifeEngine.FGraphCount + FElapsed;
  if FLifeEngine.FGraphCount > TStopwatch.Frequency * 2 then
  begin
    FLifeEngine.FMaxGensPerSecond := 0.0;
    FLifeEngine.FGraphCount := 0;
  end;
  FLifeEngine.FMaxGensPerSecond := Max(FLifeEngine.FMaxGensPerSecond, FLifeEngine.FGensPerSecond);
  FLifeEngine.DoUpdate;
  FUpdating := False;
end;

end.
