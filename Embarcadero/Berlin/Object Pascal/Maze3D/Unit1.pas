//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms3D, FMX.Types3D, FMX.Forms, FMX.Graphics,
  FMX.Dialogs, System.Math.Vectors, FMX.Controls3D, FMX.Layers3D, FMX.StdCtrls, FMX.Objects, FMX.Objects3D,
  FMX.MaterialSources, Datasnap.DSClientMetadata, System.Sensors, System.Sensors.Components,
  FMX.Platform, FMX.Controls.Presentation;

type
  TFlag = class(TCylinder)
  private
    FStand: TCone;
    FCloth: TCube;
    FTimer: TTimer;
    FR, FC: Integer;
    procedure PullFlag;
    procedure ForFlagTimer(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetMaterials(AForStand, AForCloth, AForMain: TMaterialSource);
    procedure Placement(AR, AC: Integer);
    function AlreadyBusy(AR, AC: Integer): Boolean;
    procedure Visited(AR, AC: Integer);
    function Checked: Boolean;
  end;

  TCell = record
    Top, Left, Right, Back: Boolean;
  public
    constructor Create(ATop, ALeft, ARight, ABack: Boolean);
    procedure OrCell(ACell: TCell);
    class function JustTop: TCell; static;
    class function JustLeft: TCell; static;
    class function JustRight: TCell; static;
    class function JustBack: TCell; static;
  end;

  TBug = record
    Row, Col: integer;
    Environment: TCell
  end;

  TWalls = class
  private
    FWallsArray: array of array of boolean;
    function GetWall(Row, Col: Integer): Boolean;
    procedure SetWall(Row, Col: Integer; const Value: Boolean);
  public
    procedure SetSize(ASize: Integer);
    property Wall[Row, Col: Integer]: Boolean read GetWall write SetWall; default;
  end;

  TMaze = class
  private
    FSize: Integer;
    FWallsDown: TWalls;
    FWallsCross: TWalls;
    FBugs: array of TBug;
    FChecked: TWalls;
    procedure MakeFence;
    procedure MakeNewBug(ARow, ACol: Integer; ADirection: TCell);
  public
    procedure OpenExit(Row, Col: Integer; ADown: Boolean);
    function Size: Integer;
    function Cell(Row, Col: Integer): TCell;
    function WallDown(Row, Col: Integer): Boolean;
    function WallCross(Row, Col: Integer): Boolean;
    procedure Generate(ASize: Integer; AFullGeneration: Boolean = True);
    procedure MakeIteration;
    function MazeFinished: Boolean;
  end;

  TForm1 = class(TForm3D)
    lBanner: TLayer3D;
    lmsMain: TLightMaterialSource;
    lightMain: TLight;
    poMain: TProxyObject;
    LightViolet: TLight;
    cylDisk: TCylinder;
    Sphere1: TSphere;
    cmlDisk: TColorMaterialSource;
    Sphere2: TSphere;
    cmlWalls: TColorMaterialSource;
    msAccel: TMotionSensor;
    tForAccel: TTimer;
    Label1: TLabel;
    tForBanner: TTimer;
    lGlass: TLayer3D;
    Image1: TImage;
    cmsGreen: TColorMaterialSource;
    procedure Form3DCreate(Sender: TObject);
    procedure Form3DKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure msAccelSensorChoosing(Sender: TObject; const Sensors: TSensorArray; var ChoseSensorIndex: Integer);
    procedure tForAccelTimer(Sender: TObject);
    procedure tForBannerTimer(Sender: TObject);
    procedure Form3DDestroy(Sender: TObject);
  private
  const
    cWidth = 0.5;
    cHeight = 1;
    cLength = 5;
    cPrivateArea = 1.5;
  private
    FFlags: array of TFlag;
    FGlass: Boolean;
    FMaze: TMaze;
    FPOCenter: TProxyObject;
    FGate: TCube;
    FGateDown: Boolean;
    FGateR: Integer;
    FGateC: Integer;
    procedure Create3DObjects;
    procedure InitNewGame;
    procedure ShowInfo;
    procedure FillRCPosition(var AR, AC: Integer);
    procedure CheckFlags;
    procedure LowerTheFlag( AR, AC: Integer);
    procedure OpenGate;
    function SetPositionX(APositionX: Single): Boolean;
    function SetPositionZ(APositionZ: Single): Boolean;
    procedure SetDirection(AAngle: Single);
    function CheckWin: Boolean;
    { Private declarations }
  public
    { Public declarations }
    function HandleAppEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
  end;

var
  Form1: TForm1;
  GlobalSize: Integer;
  StartPoint: TPointF;

implementation

uses
  System.Math, FMX.Consts, System.Devices
{$IFDEF Android}
, Androidapi.JNI.PowerManager
{$ENDIF}
  ;

{$R *.fmx}
{$R *.GGlass.fmx ANDROID}

procedure TForm1.CheckFlags;
var
  R, C: Integer;
  I: Integer;
  ShouldOpen: Boolean;
begin
  ShouldOpen := True;
  FillRCPosition(R, C);
  for I := 0 to FMaze.Size - 1 do
    if not FFlags[I].Checked then
      ShouldOpen := False;
  if ShouldOpen then
    OpenGate;
end;

function TForm1.CheckWin: Boolean;
var
  R, C: Integer;
begin
  Result := False;
  FillRCPosition(R, C);
  if (R < 0) or (C < 0) or (R >= FMaze.Size) or (C >= FMaze.Size) then
  begin
    ShowInfo;
    GlobalSize := GlobalSize + 2;
    Result := True;
  end;
end;

procedure TForm1.Create3DObjects;

  function CreateCyl(ARow, ACol: Integer): TCylinder;
  begin
    Result := TCylinder.Create(Self);
    Result.MaterialSource := lmsMain;
    Result.Width := cWidth;
    Result.Depth := cWidth;
    Result.Height := cHeight;
    Result.Parent := FPOCenter;
    Result.Position.X := -cLength * FMaze.Size / 2 + cLength * ACol;
    Result.Position.Z := -cLength * FMaze.Size / 2 + cLength * ARow;
    Result.Position.Y := 0;
  end;

  function CreateWall(ARow, ACol: Integer;ACross: Boolean): TCube;
  begin
    Result := TCube.Create(Self);
    Result.MaterialSource := lmsMain;
    if ACross then
      Result.RotationAngle.Y := 90;
    Result.Width := cWidth;
    Result.Depth := cLength;
    Result.Height := cHeight;
    Result.Parent := FPOCenter;
    if ACross then
    begin
      Result.Position.X := -cLength * FMaze.Size / 2 + cLength * ACol + cLength / 2;
      Result.Position.Z := -cLength * FMaze.Size / 2 + cLength * ARow;
    end
    else
    begin
      Result.Position.X := -cLength * FMaze.Size / 2 + cLength * ACol;
      Result.Position.Z := -cLength * FMaze.Size / 2 + cLength * ARow + cLength / 2;
    end;
    Result.Position.Y := 0;
  end;

  function RandomBool: Boolean;
  begin
    Result := Random(100) > 50;
  end;

  procedure SetExit(AR, AC: Integer);
  begin
    FGateR := AR;
    FGateC := AC;
  end;

var
  I, J: Integer;
  LR, LC: Integer;
  GoodPoint: Boolean;
  TempWall: TCube;
begin
  if FPOCenter <> nil then
    FPOCenter.DisposeOf;
  FPOCenter := TProxyObject.Create(Self);
  FPOCenter.Parent := Self;

  FGateDown := RandomBool;
  if FGateDown then
    if RandomBool then
      SetExit(Random(FMaze.Size), 0)
    else
      SetExit(Random(FMaze.Size), FMaze.Size)
  else
    if RandomBool then
      SetExit(0, Random(FMaze.Size))
    else
      SetExit(FMaze.Size, Random(FMaze.Size));
  for I := 0 to FMaze.Size - 1 do
  begin
    SetLength(FFlags, I + 1);
    FFlags[I] := TFlag.Create(Self);
    FFlags[I].Parent := FPOCenter;
    FFlags[I].SetMaterials(cmlDisk, cmsGreen, cmlDisk);
    GoodPoint := False;
    LR := -1;
    LC := -1;
    while not GoodPoint do
    begin
      GoodPoint := True;
      LR := Random(FMaze.Size);
      LC := Random(FMaze.Size);
      for J := 0 to I - 1 do
        if FFlags[J].AlreadyBusy(LR, LC) then
        begin
          GoodPoint := False;
          Break;
        end;
      if (LR = FMaze.Size div 2) and (LC = FMaze.Size div 2) then
        GoodPoint := False;
    end;
    FFlags[I].Placement( LR, LC);
    FFlags[I].Position.X := -cLength * FMaze.Size / 2 + cLength * LC + cLength / 2;
    FFlags[I].Position.Z := -cLength * FMaze.Size / 2 + cLength * LR + cLength / 2;
  end;

  FGate := nil;
  if (FMaze <> nil) and (FMaze.FSize > 0) then
  begin
    for J := 0 to FMaze.FSize do
      for I := 0 to FMaze.FSize do
      begin
        if FMaze.WallDown(I,J) then
        begin
          TempWall := CreateWall( I, J, False);
          if (I = FGateR) and (J = FGateC) and FGateDown then
            FGate := TempWall;
        end;
        if FMaze.WallCross(I,J) then
        begin
          TempWall := CreateWall( I, J, True);
          if (I = FGateR) and (J = FGateC) and not FGateDown then
            FGate := TempWall;
        end;
        CreateCyl( I, J);
      end;
  end;
end;

procedure TForm1.FillRCPosition(var AR, AC: Integer);
begin
  AR := FMaze.Size - Floor((FPOCenter.Position.Z + cLength * FMaze.Size / 2) / cLength) - 1;
  AC := FMaze.Size - Floor((FPOCenter.Position.X + cLength * FMaze.Size / 2) / cLength) - 1;
end;

procedure TForm1.Form3DCreate(Sender: TObject);
var
  iDevService: IFMXDeviceService;
  aFMXApplicationEventService: IFMXApplicationEventService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXApplicationEventService, IInterface(aFMXApplicationEventService)) then
    aFMXApplicationEventService.SetApplicationEventHandler(HandleAppEvent);

  Randomize;
  if TPlatformServices.Current.SupportsPlatformService(IFMXDeviceService, iDevService) then
    FGlass := iDevService.GetDeviceClass = TDeviceInfo.TDeviceClass.Glasses
  else
    FGlass := False;
  lGlass.Visible := FGlass;
  lBanner.Visible := False;
  GlobalSize := 3;
  InitNewGame;
end;

procedure TForm1.Form3DDestroy(Sender: TObject);
begin
{$IFDEF Android}
  ReleaseWakeLock;
{$ENDIF}
end;

procedure TForm1.Form3DKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
const
  cStep = 0.5;
begin
  if not tForBanner.Enabled then
    case Key of
      vkUp: SetPositionZ( - cStep);
      vkDown: SetPositionZ(cStep);
      vkLeft: SetPositionX( cStep);
      vkRight: SetPositionX( - cStep);
    end;
end;

function TForm1.HandleAppEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
begin
{$IFDEF Android}
  Result := True;
  case AAppEvent of
    TApplicationEvent.FinishedLaunching: AcquireWakeLock;
    TApplicationEvent.BecameActive: AcquireWakeLock;
    TApplicationEvent.WillBecomeInactive: ReleaseWakeLock;
    TApplicationEvent.EnteredBackground: ReleaseWakeLock;
    TApplicationEvent.WillBecomeForeground: AcquireWakeLock;
    TApplicationEvent.WillTerminate: ReleaseWakeLock;
  else
    Result := False;
  end;
{$ENDIF}
end;

procedure TForm1.InitNewGame;
begin
  FMaze := TMaze.Create;
  FMaze.Generate(GlobalSize, True);
  Create3DObjects;
  msAccel.Active := True;
end;

procedure TForm1.LowerTheFlag(AR, AC: Integer);
var
  I: Integer;
begin
  for I := 0 to FMaze.Size - 1 do
    if FFlags[I].AlreadyBusy( AR, AC) then
      FFlags[I].PullFlag;
  CheckFlags;
end;

procedure TForm1.msAccelSensorChoosing(Sender: TObject; const Sensors: TSensorArray; var ChoseSensorIndex: Integer);
var
  LSensor: TCustomSensor;
  I: Integer;
begin
  I := 0;
  for LSensor in Sensors do
  begin
    if TCustomMotionSensor(LSensor).SensorType = TMotionSensorType.Accelerometer3D then
    begin
      ChoseSensorIndex := I;
      Break;
    end;
    Inc(I);
  end;
end;

procedure TForm1.OpenGate;
begin
  FMaze.OpenExit(FGateR, FGateC, FGateDown);
  if FGate <> nil then
    FGate.Visible := False;
end;

{ TMaze }

function TMaze.Cell(Row, Col: Integer): TCell;
begin
  Result.Top := FWallsCross[Row + 1, Col];
  Result.Left := FWallsDown[Row, Col];
  Result.Right := FWallsDown[Row, Col + 1];
  Result.Back := FWallsCross[Row, Col];
end;

procedure TMaze.Generate(ASize: Integer; AFullGeneration: Boolean = True);

  procedure InitWalls(var AWalls: TWalls);
  begin
    if AWalls <> nil then
      AWalls.Free;
    AWalls := TWalls.Create;
    AWalls.SetSize(FSize + 1);
  end;

begin
  FSize := ASize;
  InitWalls(FWallsDown);
  InitWalls(FWallsCross);
  InitWalls(FChecked);
  MakeFence;
  MakeNewBug(Size - 1, Size div 2, TCell.Create(False, False, False, False));
  if AFullGeneration then
    while not MazeFinished do
      MakeIteration;
end;

procedure TMaze.MakeFence;
var
  I: Integer;
begin
  for I := 0 to FSize - 1 do
  begin
    FWallsDown[I, 0] := True;
    FWallsDown[I, FSize] := True;
    FWallsCross[0, I] := True;
    FWallsCross[FSize, I] := True;
  end;
end;

procedure TMaze.MakeNewBug(ARow, ACol: Integer; ADirection: TCell);
var
  NewBug: TBug;
begin
  NewBug.Row := ARow;
  NewBug.Col := ACol;
  NewBug.Environment := Cell(NewBug.Row, NewBug.Col);
  NewBug.Environment.OrCell(ADirection);
  SetLength(FBugs, Length(FBugs) + 1);
  FBugs[Length(FBugs) - 1] := NewBug;
  FChecked[NewBug.Row, NewBug.Col] := True;
  if not NewBug.Environment.Top then
    if FChecked[NewBug.Row + 1, NewBug.Col] then
      FWallsCross[NewBug.Row + 1, NewBug.Col] := True;
  if not NewBug.Environment.Left then
    if FChecked[NewBug.Row, NewBug.Col - 1] then
      FWallsDown[NewBug.Row, NewBug.Col] := True;
  if not NewBug.Environment.Right then
    if FChecked[NewBug.Row, NewBug.Col + 1] then
      FWallsDown[NewBug.Row, NewBug.Col + 1] := True;
  if not NewBug.Environment.Back then
    if FChecked[NewBug.Row - 1, NewBug.Col] then
      FWallsCross[NewBug.Row, NewBug.Col] := True;
  NewBug.Environment.OrCell(Cell(NewBug.Row, NewBug.Col));
end;

procedure TMaze.MakeIteration;

  function GetWorkingBug: TBug;
  var
    Next: Integer;
  begin
    Next := Random(Length(FBugs));
    Result := FBugs[Next];
    FBugs[Next] := FBugs[Length(FBugs) - 1];
    SetLength(FBugs, Length(FBugs) - 1);
  end;

var
  WorkingBug: TBug;
begin
  WorkingBug := GetWorkingBug;
  WorkingBug.Environment.OrCell(Cell(WorkingBug.Row, WorkingBug.Col));
  if not WorkingBug.Environment.Top then
    MakeNewBug(WorkingBug.Row + 1, WorkingBug.Col, TCell.JustBack);
  if not WorkingBug.Environment.Left then
    MakeNewBug(WorkingBug.Row, WorkingBug.Col - 1, TCell.JustRight);
  if not WorkingBug.Environment.Right then
    MakeNewBug(WorkingBug.Row, WorkingBug.Col + 1, TCell.JustLeft);
  if not WorkingBug.Environment.Back then
    MakeNewBug(WorkingBug.Row - 1, WorkingBug.Col, TCell.JustTop);
end;

function TMaze.MazeFinished: Boolean;
begin
  Result := Length(FBugs) = 0;
//  if Result then
//    FWallsCross[ 0, FSize div 2]:= False;
end;

procedure TMaze.OpenExit(Row, Col: Integer; ADown: Boolean);
begin
  if ADown then
    FWallsDown[Row, Col] := False
  else
    FWallsCross[Row, Col] := False;
end;

function TMaze.Size: Integer;
begin
  Result := FSize;
end;

function TMaze.WallCross(Row, Col: Integer): Boolean;
begin
  Result := FWallsCross[Row, Col];
end;

function TMaze.WallDown(Row, Col: Integer): Boolean;
begin
  Result := FWallsDown[Row, Col];
end;

{ TWalls }

function TWalls.GetWall(Row, Col: Integer): Boolean;
begin
  Result := FWallsArray[Row, Col];
end;

procedure TWalls.SetSize(ASize: Integer);
begin
  SetLength(FWallsArray, ASize, ASize);
end;

procedure TWalls.SetWall(Row, Col: Integer; const Value: Boolean);
begin
  FWallsArray[Row, Col] := Value;
end;

{ TCell }

constructor TCell.Create(ATop, ALeft, ARight, ABack: Boolean);
begin
  Self.Top := ATop;
  Self.Left := ALeft;
  Self.Right := ARight;
  Self.Back := ABack;
end;

procedure TForm1.SetDirection(AAngle: Single);
begin
  cylDisk.RotationAngle.Y := AAngle;
end;

function TForm1.SetPositionX(APositionX: Single): Boolean;
var
  NewPos, WallPos: Single;
  R, C: Integer;
begin
  FillRCPosition(R, C);

  if APositionX < 0 then
  begin
    if FMaze.WallDown(R, C + 1) then
    begin
      WallPos := cLength * FMaze.Size / 2 - cLength * (C + 1);
      NewPos := FPOCenter.Position.X + APositionX;
      if NewPos < (WallPos + cPrivateArea / 2) then
        NewPos := WallPos + cPrivateArea / 2;
    end
    else
      NewPos := FPOCenter.Position.X + APositionX;
  end
  else
  begin
    if FMaze.WallDown(R, C) then
    begin
      WallPos := cLength * FMaze.Size / 2 - cLength * (C);
      NewPos := FPOCenter.Position.X + APositionX;
      if NewPos > (WallPos - cPrivateArea / 2) then
        NewPos := WallPos - cPrivateArea / 2;
    end
    else
      NewPos := FPOCenter.Position.X + APositionX;
  end;
  FPOCenter.Position.X := NewPos;
  LowerTheFlag( R, C);
  Result := not CheckWin;
end;

function TForm1.SetPositionZ(APositionZ: Single): Boolean;
var
  NewPos, WallPos: Single;
  R, C: Integer;
begin
  FillRCPosition(R, C);

  if APositionZ < 0 then
  begin
    if FMaze.WallCross(R + 1, C) then
    begin
      WallPos := cLength * FMaze.Size / 2 - cLength * (R + 1);
      NewPos := FPOCenter.Position.Z + APositionZ;
      if NewPos < (WallPos + cPrivateArea / 2) then
        NewPos := WallPos + cPrivateArea / 2;
    end
    else
      NewPos := FPOCenter.Position.Z + APositionZ;
  end
  else
  begin
    if FMaze.WallCross(R, C) then
    begin
      WallPos := cLength * FMaze.Size / 2 - cLength * (R);
      NewPos := FPOCenter.Position.Z + APositionZ;
      if NewPos > (WallPos - cPrivateArea / 2) then
        NewPos := WallPos - cPrivateArea / 2;
    end
    else
      NewPos := FPOCenter.Position.Z + APositionZ;
  end;
  FPOCenter.Position.Z := NewPos;
  LowerTheFlag( R, C);
  Result := not CheckWin;
end;

procedure TForm1.ShowInfo;
begin
  tForAccel.Enabled := False;
  lBanner.RotationAngle.X := -100;
  lBanner.Visible := True;
  tForBanner.Enabled := True;
end;

procedure TForm1.tForAccelTimer(Sender: TObject);
const
  cDeadBandMin = 0.01;
  cAcceleration = 2;
var
  Sen: TCustomMotionSensor;

  function CalcAngle(const ACoord1: Single; const ACoord2: Single): Single;
  var
    AngleValue : Single;
  begin
    if ACoord1 <> 0 then
      AngleValue := ArcTan(ACoord2 / ACoord1)
    else
      AngleValue := Pi / 2;
    if ACoord1 > 0 then
      Result := AngleValue
    else
      Result := Pi + AngleValue;
    Result := RadToDeg(Result);
  end;

begin
  if msAccel.Active then
  begin
    Sen := msAccel.Sensor;

    if (Width < Height) or FGlass then
    begin
      if Abs(Sen.AccelerationZ) > cDeadBandMin then
        if not SetPositionZ(Sen.AccelerationZ * cAcceleration) then
          Exit;
      if Abs(Sen.AccelerationX) > cDeadBandMin then
        if not SetPositionX(- Sen.AccelerationX * cAcceleration) then
          Exit;
      SetDirection( - CalcAngle(Sen.AccelerationZ, Sen.AccelerationX) + 90);
    end
    else
    begin
      if Abs(Sen.AccelerationZ) > cDeadBandMin then
        if not SetPositionZ(Sen.AccelerationZ * cAcceleration) then
          Exit;
      if Abs(Sen.AccelerationY) > cDeadBandMin then
        if not SetPositionX(Sen.AccelerationY * cAcceleration) then
          Exit;
      SetDirection(CalcAngle(Sen.AccelerationZ, Sen.AccelerationY) + 90);
    end;
  end;
end;

procedure TForm1.tForBannerTimer(Sender: TObject);
begin
  lBanner.RotationAngle.X := lBanner.RotationAngle.X + 10;
  if Abs(lBanner.RotationAngle.X - 80) < 10 then
  begin
    lBanner.Visible := False;
    tForBanner.Enabled := False;
    tForAccel.Enabled := True;
    InitNewGame;
  end;
end;

class function TCell.JustBack: TCell;
begin
  Result.Create(False, False, False, True);
end;

class function TCell.JustLeft: TCell;
begin
  Result.Create(False, True, False, False);
end;

class function TCell.JustRight: TCell;
begin
  Result.Create(False, False, True, False);
end;

class function TCell.JustTop: TCell;
begin
  Result.Create(True, False, False, False);
end;

procedure TCell.OrCell(ACell: TCell);
begin
  Self.Top := Self.Top or ACell.Top;
  Self.Left := Self.Left or ACell.Left;
  Self.Right := Self.Right or ACell.Right;
  Self.Back := Self.Back or ACell.Back;
end;

{ TFlag }

function TFlag.AlreadyBusy(AR, AC: Integer): Boolean;
begin
  Result := (Self.FR = AR) and (Self.FC = AC);
end;

function TFlag.Checked: Boolean;
begin
  Result := FCloth.Position.Y > - 1.45;
end;

constructor TFlag.Create(AOwner: TComponent);
begin
  inherited;
  Self.Width := 0.1;
  Self.Depth := 0.1;
  Self.Height := 4;
  Self.Position.Y := -2;

  FStand := TCone.Create(AOwner);
  FStand.Parent := Self;
  FStand.Position.Y := 2;

  FCloth := TCube.Create(AOwner);
  FCloth.Parent := Self;
  FCloth.Position.X := 0.6;
  FCloth.Position.Y := - 1.5;
  FCloth.Depth := 0.1;

  FTimer := TTimer.Create(AOwner);
  FTimer.Parent := Self;
  FTimer.Enabled := False;
  FTimer.Interval := 100;
  FTimer.OnTimer := ForFlagTimer;
end;

procedure TFlag.ForFlagTimer(Sender: TObject);
begin
  FCloth.Position.Y := FCloth.Position.Y + 0.1;
  FTimer.Enabled := FCloth.Position.Y < 1;
end;

procedure TFlag.Placement(AR, AC: Integer);
begin
  FR := AR;
  FC := AC;
end;

procedure TFlag.PullFlag;
begin
  if not self.Checked then
    FTimer.Enabled := True;
end;

procedure TFlag.SetMaterials(AForStand, AForCloth, AForMain: TMaterialSource);
begin
  Self.MaterialSource := AForMain;
  FStand.MaterialSource := AForStand;
  FCloth.MaterialSource := AForCloth;
end;

procedure TFlag.Visited(AR, AC: Integer);
begin
  if (AR = FR) and (AC = FC) then
    PullFlag;
end;

end.
