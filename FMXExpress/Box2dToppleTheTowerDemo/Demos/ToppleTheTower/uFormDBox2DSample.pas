unit uFormDBox2DSample;

interface

{$I ..\..\Physics2D\Physics2D.inc}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, System.IniFiles,
  FMX.Layouts, FMX.Edit, FMX.ListBox, FMX.Objects, Math, System.IOUtils,
  uXCadencer, uDebugDrawerFM, uSimulation, uPhysics2D, uPhysics2DTypes,
  FMX.Media
{$IFDEF MSWINDOWS}
  , MMSystem, screenGameOver, screenStart, screenInstructions
{$ENDIF}
  ;

type
  TGameForm = class(TForm, IGUIForm)
    LayoutButtons: TLayout;
    btnPause: TButton;
    btnSingleStep: TButton;
    btnReset: TButton;
    pntbxDrawPanel: TPaintBox;
    Circle: TCircle;
    MediaPlayer: TMediaPlayer;
    ScoreLBL: TLabel;
    LaunchCircle: TCircle;
    ShotsLBL: TLabel;
    CrateOriginal: TRectangle;
    EndTimer: TTimer;
    StartScreen: TStartFrame;
    MusicPlayer: TMediaPlayer;
    Label1: TLabel;
    GameOverScreen: TGameOverFrame;
    InstructionsScreen: TInstructionsFrame;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SimulationOptionsChanged(Sender: TObject);
    procedure VisibilityOptionsChanged(Sender: TObject);
    procedure ModeOptionsChanged(Sender: TObject);
    procedure btnPauseClick(Sender: TObject);
    procedure btnSingleStepClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure pntbxDrawPanelPaint(Sender: TObject; Canvas: TCanvas);
    procedure FormResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure pntbxDrawPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure pntbxDrawPanelMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure pntbxDrawPanelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure pntbxDrawPanelMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
    procedure EndTimerTimer(Sender: TObject);
    procedure StartScreenButton1Click(Sender: TObject);
    procedure GameOverScreenButton1Click(Sender: TObject);
    procedure InstructionsScreenButton1Click(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FSimulation: TSimulation;
    lastp: TPointF;
    dumpPool: TStringList;
    FCadencer: TXCadencer;
    FDrawer: TDebugDrawerFM;
    FDeltaTime, FNewTime: Double;
    Bomb: TCircle;
    InitdOnce: Boolean;
    DataFilePath: String;
    procedure PlaySound(const Snd: String);
    procedure TimerProgress(const deltaTime, newTime: Double);
    procedure UpdatePauseButton;
    procedure ResetView;
    procedure UpdateDrawerTranslation;
    procedure PlayMusic(const Snd: String);
  public
    Shots: Integer;
    Score: Integer;
    HighScore: Integer;
    function GetCrate: TRectangle;
    procedure SetScore(I: Integer);
    procedure SetShots(I: Integer);
    procedure SimulationRecreateAndRun;
  end;

var
  GameForm: TGameForm;

implementation

{$R *.fmx}

uses System.Character, uMySimulation;

procedure TGameForm.PlaySound(const Snd: String);
begin
{$IFDEF MSWINDOWS}
sndPlaySound(Addr(Snd[1]),SND_NODEFAULT Or SND_ASYNC);
{$ENDIF}
{$IFDEF POSIX}
if MediaPlayer.FileName='' then
 begin
  MediaPlayer.FileName := Snd;
 end
else
 begin
   MediaPlayer.Stop;
 end;
if MediaPlayer.Media <> nil then
  MediaPlayer.Play;
{$ENDIF}
end;

procedure TGameForm.PlayMusic(const Snd: String);
begin
if MusicPlayer.FileName='' then
 begin
  MusicPlayer.FileName := Snd;
 end
else
 begin
   MusicPlayer.Stop;
 end;
if MusicPlayer.Media <> nil then
  MusicPlayer.Play;
end;

procedure TGameForm.GameOverScreenButton1Click(Sender: TObject);
begin
  MusicPlayer.Clear;
  PlayMusic(DataFilePath + 'music.mp3');
  SimulationRecreateAndRun;
end;

function TGameForm.GetCrate: TRectangle;
begin
  Result := CrateOriginal;
end;


procedure TGameForm.InstructionsScreenButton1Click(Sender: TObject);
begin
  InstructionsScreen.Visible := False;
  ResetView;
  VisibilityOptionsChanged(nil);
  SimulationOptionsChanged(nil);
  SimulationRecreateAndRun;
end;

procedure TGameForm.FormCreate(Sender: TObject);
begin
  FDeltaTime := 0;
  FNewTime := 0;

  Randomize;

  {$IFDEF MSWINDOWS}
  DataFilePath := ExtractFilePath(ParamStr(0));
  {$ELSE}
  DataFilePath := System.IOUtils.TPath.GetDocumentsPath + System.SysUtils.PathDelim;
  {$ENDIF}

  VisibilityOptionsChanged(self);
  SimulationOptionsChanged(self);
  ModeOptionsChanged(self);

  FCadencer := TXCadencer.Create;
  FCadencer.OnProgress := TimerProgress;

  FDrawer := TDebugDrawerFM.Create;
  FDrawer.Canvas := pntbxDrawPanel.Canvas;

  PlayMusic(DataFilePath + 'music.mp3');
end;

procedure TGameForm.FormDestroy(Sender: TObject);
begin
  FCadencer.Enabled := False;
  FCadencer.Free;
  FSimulation.Free;
  FDrawer.Free;
end;

procedure TGameForm.SetShots(I: Integer);
begin
  Shots := I;
  ShotsLBL.Text := IntToStr(I);
  if Shots=0 then
    EndTimer.Enabled := True;
end;


procedure TGameForm.SetScore(I: Integer);
begin
  Score := I;
  ScoreLBL.Text := IntToStr(I);
end;

procedure TGameForm.SimulationRecreateAndRun;
var
mRect: TRectF;
I: Integer;
currentBody: Tb2Body;
Obj: TFmxObject;
P: Pointer;
D: Double;
R: TRectangle;
C: TCircle;
begin
  GameOverScreen.Visible := False;
  SetScore(0);
  SetShots(3);
  InitdOnce := False;
  ResetView;
  if Assigned(FSimulation) then
    begin

    currentBody := FSimulation.m_world.GetBodyList;
    for I := 0 to FSimulation.m_world.GetBodyCount-1 do
     begin
      if Assigned(currentBody) then
        begin
        if currentBody.UserData<>nil then
         begin
          Obj := TFmxObject(currentBody.UserData);
          if (Obj is TRectangle) then
          begin
            R := TRectangle(currentBody.UserData);
            if (R.Tag = 2) then
            begin
              R.Free;
            end;
            if (R.Tag = 1) then
            begin
              R.Free;
            end;
          end;
          if (Obj is TCircle) then
          begin
            C := TCircle(currentBody.UserData);
            if (C.Tag = 3) then
            begin
              if C.Visible then
               C.Visible := False;
            end;

          end;

         end;
        end;
        currentBody := currentBody.GetNext;
     end;

     FSimulation.Free;
    end;
  FSimulation := TMySimulation.Create;
  FSimulation.m_DrawDebugInfo := False;
  FSimulation.m_InvertedY := False;
  FSimulation.SetDebugDrawer(FDrawer);
  FSimulation.SetGUIForm(self);
  FSimulation.SetGUIBomb(Circle);
  SimulationOptionsChanged(self);
  FCadencer.Reset;
  FCadencer.Enabled := True;
end;

procedure TGameForm.StartScreenButton1Click(Sender: TObject);
begin
StartScreen.Visible := False;
InstructionsScreen.Visible := True;
end;

procedure TGameForm.TimerProgress(const deltaTime, newTime: Double);
begin
  FDeltaTime := deltaTime;
  FNewTime := newTime;

  self.Invalidate;

  if not Assigned(FSimulation) then
    FCadencer.Enabled := False;
end;

procedure TGameForm.TimerTimer(Sender: TObject);
var
IniFile: TMemIniFile;
begin
Timer.Enabled := False;
IniFile := TMemIniFile.Create(DataFilePath + 'Settings.ini');
HighScore := IniFile.ReadInteger('Settings','HighScore',0);
IniFile.Free;
end;

procedure TGameForm.pntbxDrawPanelPaint(Sender: TObject;
  Canvas: TCanvas);
const DEFAULT_OPACITY: Double = 100;
var
mRect: TRectF;
I: Integer;
currentBody: Tb2Body;
Obj: TFmxObject;
P: Pointer;
D: Double;
R: TRectangle;
C: TCircle;
pos_x:Double;
pos_y:Double;
begin
  Canvas.BeginScene;
  try
    Canvas.Fill.Color := TAlphaColorRec.Black;
    mRect := pntbxDrawPanel.BoundsRect;
    Canvas.FillRect(mRect,0,0,[],0);//DEFAULT_OPACITY);

    if Assigned(FSimulation) then
    begin
      FDrawer.Canvas := Canvas;
      FSimulation.Step(Settings, FDeltaTime);
    end;
  finally
    Canvas.EndScene;
  end;

  if Assigned(FSimulation) then
   begin
    currentBody := FSimulation.m_world.GetBodyList;
    for I := 0 to FSimulation.m_world.GetBodyCount-1 do
     begin
      if Assigned(currentBody) then
        begin
        if currentBody.UserData<>nil then
         begin
          Obj := TFmxObject(currentBody.UserData);
          if (Obj is TRectangle) then
          begin
            R := TRectangle(currentBody.UserData);
            if (R.Tag = 2) then
            begin
              if InitdOnce=False then
               begin
                 R.Fill.Bitmap.Bitmap.Assign(CrateOriginal.Fill.Bitmap.Bitmap);
                 R.Fill.Kind := TBrushKind.bkBitmap;
                 R.Fill.Bitmap.WrapMode := TWrapMode.wmTileStretch;               
               end;
              //TRectangle(currentBody.UserData).Position.X := currentBody.GetPosition.x*FDrawer.ScaleX;
              //TRectangle(currentBody.UserData).Position.Y := -currentBody.GetPosition.y*FDrawer.ScaleY;
              if R.TagFloat>5 then
               begin
                FSimulation.m_world.DestroyBody(currentBody);
                R.Free;
                PlaySound(DataFilePath + 'pop.wav');
                SetScore(Score+1);
               end
              else
               begin
                R.Position.X := ((currentBody.GetPosition.x)*FDrawer.ScaleX)+FDrawer.OffsetX - (R.Width/2);
                R.Position.Y := (((-currentBody.GetPosition.y))*FDrawer.ScaleY)+(FDrawer.CanvasHeight-FDrawer.OffsetY) + LayoutButtons.Height - (R.Height/2);
                R.RotationAngle := -currentBody.GetAngle*(180/PI);
               end;
            end;
            if (R.Tag = 1) then
            begin
              R.Position.X := ((currentBody.GetPosition.x)*FDrawer.ScaleX)+FDrawer.OffsetX - (R.Width/2);
              R.Position.Y := (((-currentBody.GetPosition.y))*FDrawer.ScaleY)+(FDrawer.CanvasHeight-FDrawer.OffsetY) + LayoutButtons.Height - (R.Height/2);
              R.RotationAngle := -currentBody.GetAngle*(180/PI);
            end;
          end;
          if (Obj is TCircle) then
          begin
            C := TCircle(currentBody.UserData);
            if (C.Tag = 3) then
            begin
              if not C.Visible then
                C.Visible := True;
              C.Position.X := ((currentBody.GetPosition.x)*FDrawer.ScaleX)+FDrawer.OffsetX - (C.Width/2);
              C.Position.Y := (((-currentBody.GetPosition.y))*FDrawer.ScaleY)+(FDrawer.CanvasHeight-FDrawer.OffsetY) + LayoutButtons.Height - (C.Height/2);
              C.RotationAngle := -currentBody.GetAngle*(180/PI);


              if (1=2) then
              begin
                  //launcher
                  pos_x := currentBody.GetWorldCenter.x*FDrawer.ScaleX;
                  pos_y := currentBody.GetWorldCenter.y*FDrawer.ScaleY;
              end
              else 
               begin
                  // ball
                  pos_x := currentBody.GetWorldCenter.x*FDrawer.ScaleX;
                  pos_y := currentBody.GetWorldCenter.y*FDrawer.ScaleY;
               end;
             // pntbxDrawPanel.Position.X := pntbxDrawPanel.Width/2-pos_x;
             // pntbxDrawPanel.Position.Y := pntbxDrawPanel.Height/2-pos_y;


            end;

          end;

         end;
        end;
        currentBody := currentBody.GetNext;
     end;
     if InitdOnce=False then
      InitdOnce := True;
    end;

end;

procedure TGameForm.SimulationOptionsChanged(Sender: TObject);
begin
  Settings.enableSleep := True;
  Settings.enableWarmStarting := True;
  Settings.enableContinuousPhysics := True;
  Settings.enableSubStepping := False;
  if Assigned(FSimulation) then
  begin
    FSimulation.m_world.AllowSleeping := Settings.enableSleep;
    FSimulation.m_world.WarmStarting := Settings.enableWarmStarting;
    FSimulation.m_world.ContinuousPhysics := Settings.enableContinuousPhysics;
    FSimulation.m_world.SubStepping := Settings.enableSubStepping;
  end;
end;

procedure TGameForm.VisibilityOptionsChanged(Sender: TObject);
var
  flag: Tb2DrawBitsSet;
begin
  Settings.drawShapes := False;//True;
  Settings.drawJoints := False;
  Settings.drawAABBs := False;
  Settings.drawPairs := False;
  Settings.drawContactPoints := False;
  Settings.drawContactNormals := False;
  Settings.drawContactImpulse := False;
  Settings.drawFrictionImpulse := False;
  Settings.drawCOMs := False;
  Settings.drawStats := False;
  Settings.drawKeyInfo := False;

  flag := [];

  if Settings.drawShapes then
     Include(flag, e_shapeBit);

  if Settings.drawJoints then
     Include(flag, e_jointBit);

  if Settings.drawAABBs then
     Include(flag, e_aabbBit);

  if Settings.drawPairs then
     Include(flag, e_pairBit);

  if Settings.drawCOMs then
     Include(flag, e_centerOfMassBit);

  if Assigned(FDrawer) then
    FDrawer.m_drawFlags := flag;
end;

procedure TGameForm.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
var s: string;
begin
  if Key = vkEscape then
    Close

  else if Key = vkLeft then
   if ssCtrl in Shift then
     FSimulation.ShiftOrigin(MakeVector(2.0, 0))
    else
     begin
      FDrawer.OffsetX := FDrawer.OffsetX - 1;
     end

  else if Key = vkRight then
   if ssCtrl in Shift then
     FSimulation.ShiftOrigin(MakeVector(-2.0, 0))
    else
     begin
       FDrawer.OffsetX := FDrawer.OffsetX + 1;
     end

  else if Key = vkUp then
   if ssCtrl in Shift then
     FSimulation.ShiftOrigin(MakeVector(0.0, -2.0))
    else
     begin
       FDrawer.OffsetY := FDrawer.OffsetY + 1;
     end

  else if Key = vkDown then
   if ssCtrl in Shift then
     FSimulation.ShiftOrigin(MakeVector(0.0, 2.0))
    else
     begin
       FDrawer.OffsetY := FDrawer.OffsetY - 1;
     end


  else if Key = vkHome then
    ResetView

  else if (KeyChar = 'P') or (KeyChar = 'p') then
    btnPauseClick(nil)

  else if Assigned(FSimulation) then
  begin
    if KeyChar = ' ' then
      FSimulation.LaunchBomb(15.0,Circle)
    else
      FSimulation.Keyboard(Ord(KeyChar.ToUpper));
  end;
end;

procedure TGameForm.FormKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Assigned(FSimulation) then
    FSimulation.KeyboardUp(Ord(KeyChar.ToUpper));
end;

procedure TGameForm.FormResize(Sender: TObject);
begin
  ResetView;
end;

procedure TGameForm.UpdatePauseButton;
begin
  if Settings.pause then
    btnPause.Text := 'Start'
  else
    btnPause.Text := 'Pause';
end;

procedure TGameForm.ModeOptionsChanged(Sender: TObject);
begin
  Settings.realTime := True;
end;

procedure TGameForm.ResetView;
begin
  UpdateDrawerTranslation;
  FDrawer.ScaleX := 10;
  FDrawer.ScaleY := 10;
  FDrawer.CanvasHeight := pntbxDrawPanel.Height;

  FDrawer.OffsetY := FDrawer.OffsetY / 2; // move view centre lower
end;

procedure TGameForm.pntbxDrawPanelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  pv: TVector2;
begin
if (X > LaunchCircle.Position.X) AND (Y > LaunchCircle.Position.Y) AND (X < LaunchCircle.Position.X+LaunchCircle.Width) AND (Y < LaunchCircle.Position.Y+LaunchCircle.Height) AND (Shots>0) then
 begin
    if Button = TMouseButton.mbLeft then
    begin
      if Assigned(FSimulation) then
      begin
        pv := FDrawer.ConvertScreenToWorld(X, Y);
        //if ssShift in Shift then
          FSimulation.ShiftMouseDown(pv)
        //else
        //  FSimulation.MouseDown(pv);
      end;
    end
    else if Button = TMouseButton.mbRight then
    begin
      lastp.X := X;
      lastp.Y := Y;
    end;
 end;
end;

procedure TGameForm.pntbxDrawPanelMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Single);
var pv, diff: TVector2;
begin
  pv := FDrawer.ConvertScreenToWorld(X, Y);
  if Assigned(FSimulation) then
    FSimulation.MouseMove(pv);

  if ssRight in Shift then
  begin
    diff.x := lastp.x - X;
    diff.y := lastp.y - Y;

    // span view
    FDrawer.OffsetX := FDrawer.OffsetX - diff.x;
    FDrawer.OffsetY := FDrawer.OffsetY + diff.y;
    lastp.x := X;
    lastp.y := Y;
  end;
end;

procedure TGameForm.pntbxDrawPanelMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  pv: TVector2;
begin
  if Assigned(FSimulation) then
  begin
    pv := FDrawer.ConvertScreenToWorld(X, Y);
    if (Button = TMouseButton.mbLeft) AND (Shots>0) then
     begin
        FSimulation.MouseUp(pv);
        SetShots(Shots-1);
     end;
  end;
end;

procedure TGameForm.pntbxDrawPanelMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
var aScale: Double;
begin
  aScale := FDrawer.ScaleX;

  if WheelDelta < 0 then
    aScale := b2Max(aScale * 0.9, 0.01)
  else
    aScale := b2Min(aScale * 1.1, 1000.0);

  FDrawer.ScaleX := aScale;
  FDrawer.ScaleY := aScale;
end;

procedure TGameForm.UpdateDrawerTranslation;
begin
  if Assigned(FDrawer) then
  begin
    FDrawer.OffsetX := pntbxDrawPanel.Width / 2;
    FDrawer.OffsetY := pntbxDrawPanel.Height / 2;
  end;
end;

procedure TGameForm.btnPauseClick(Sender: TObject);
begin
  Settings.pause := not Settings.pause;
  UpdatePauseButton;
end;

procedure TGameForm.btnResetClick(Sender: TObject);
begin
  SimulationRecreateAndRun;
end;

procedure TGameForm.btnSingleStepClick(Sender: TObject);
begin
  Settings.pause := True;
  Settings.singleStep := True;
  UpdatePauseButton;
end;

procedure TGameForm.EndTimerTimer(Sender: TObject);
var
IniFile: TMemIniFile;
begin
EndTimer.Enabled := False;
GameOverScreen.ScoreLBL.Text := 'Score: ' + IntToStr(Score);
if Score>HighScore then
 begin
  HighScore := Score;
  IniFile := TMemIniFile.Create(DataFilePath + 'Settings.ini');
  IniFile.WriteInteger('Settings','HighScore',HighScore);
  IniFile.UpdateFile;
  IniFile.Free;
  PlaySound(DataFilePath + 'win.wav');
 end;
GameOverScreen.HighScoreLBL.Text := 'High Score: ' + IntToStr(HighScore);

GameOverScreen.Visible := True;
GameOverScreen.BringToFront;
end;

end.
