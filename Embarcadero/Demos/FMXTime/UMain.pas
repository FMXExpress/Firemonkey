(*************************************************************************)
(*                                                                       *)
(* Fmx Time                                                              *)
(*                                                                       *)
(* Author  : Thierry Laborde                                             *)
(* Email   : thierry.laborde@Embarcadero.com                             *)
(* Date    : 26 Février 2014                                             *)
(* Version : v1.1                                                        *)
(*                                                                       *)
(*************************************************************************)
unit UMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, math, system.Diagnostics, FMX.Styles,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Effects, FMX.Objects, FMX.Layouts, FMX.StdCtrls, FMX.Ani, FMX.Edit, UInfo;

type
  TFrmMain = class(TForm)
    LayouFitClock: TLayout;
    RectBackgroundClock: TRectangle;
    LayoutScrewTop: TLayout;
    CircleScrewTopRight: TCircle;
    RectScrewTopRight1: TRectangle;
    RectScrewTopRight2: TRectangle;
    CircleScrewTopLeft: TCircle;
    LayoutClock: TLayout;
    CircleRingClock: TCircle;
    CircleBackgroundClock: TCircle;
    InnerGlowEffBackgroundClock: TInnerGlowEffect;
    LayoutHour: TLayout;
    RectHour: TRectangle;
    LayoutMinute: TLayout;
    RectMinute: TRectangle;
    LayoutSecond: TLayout;
    RectSecond: TRectangle;
    CircleCenterClock: TCircle;
    GlowEffCircleCenterClock: TGlowEffect;
    TmrTime: TTimer;
    RectScrewTopLeft1: TRectangle;
    RectScrewTopLeft2: TRectangle;
    LayoutScrewBottom: TLayout;
    CircleScrewBottomRight: TCircle;
    RectScrewBottomRight1: TRectangle;
    RectScrewBottomRight2: TRectangle;
    CircleScrewBottomLeft: TCircle;
    RectScrewBottomLeft1: TRectangle;
    RectScrewBottomLeft2: TRectangle;
    ScaledLayoutBackgroundClock: TScaledLayout;
    CircleSecond: TCircle;
    LayoutTickHour: TLayout;
    RectTickHourTop: TRectangle;
    RectTickHourBottom: TRectangle;
    LayoutTickMinute: TLayout;
    RectTickMinuteTop: TRectangle;
    RectTickMinuteBottom: TRectangle;
    ShadowEffRectHour: TShadowEffect;
    ShadowEffRectMinute: TShadowEffect;
    ShadowEffSecond: TShadowEffect;
    LayoutScrewHorizontal: TLayout;
    CircleScrewMiddleRight: TCircle;
    RectScrewMiddleRight1: TRectangle;
    RectScrewMiddleRight2: TRectangle;
    CircleScrewMiddleLeft: TCircle;
    RectScrewMiddleLeft1: TRectangle;
    RectScrewMiddleLeft2: TRectangle;
    LayoutScrewVertical: TLayout;
    CircleScrewMiddleBottom: TCircle;
    RectScrewMiddleBottom1: TRectangle;
    RectScrewMiddleBottom2: TRectangle;
    CircleScrewMiddleTop: TCircle;
    RectScrewMiddleTop1: TRectangle;
    RectScrewMiddleTop2: TRectangle;
    RectBtClock: TRectangle;
    RectBtTimer: TRectangle;
    LblClock: TLabel;
    LblTimer: TLabel;
    FltAnimLayoutHour: TFloatAnimation;
    FltAnimLayoutMinute: TFloatAnimation;
    FltAnimLayoutSecond: TFloatAnimation;
    RectBtStartStop: TRectangle;
    RectBtReset: TRectangle;
    LblReset: TLabel;
    LblStartStop: TLabel;
    EdtTimer: TEdit;
    EdtDay: TEdit;
    EdtMonth: TEdit;
    LblFmxTime: TLabel;
    LayoutRingClock: TLayout;
    ScaledLayoutRectBackground: TScaledLayout;
    ScaledLayoutTickClock: TScaledLayout;
    BtInfo: TButton;
    LayoutMain: TLayout;
    procedure TmrTimeTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BtClockClick(Sender: TObject);
    procedure BtTimerClick(Sender: TObject);
    procedure FltAnimLayoutSecondFinish(Sender: TObject);
    procedure BtResetClick(Sender: TObject);
    procedure BtStartStopClick(Sender: TObject);
    procedure BtInfoClick(Sender: TObject);
  private
    BClockMode:Boolean;
    BInitTimer:Boolean;
    BStartTimer:Boolean;
    BPauseTimer:Boolean;
    StopWatch:TStopwatch;
    procedure InitClockTicks;
    procedure InitTimer;
    procedure InitBtStartStop(BStart: Boolean);
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.fmx}

procedure TFrmMain.TmrTimeTimer(Sender: TObject);
var
  H,M,S,Ms:Word;
  DtTimer:TDateTime;
  Text:String;
begin
  if not(BInitTimer) then
  begin
    if BClockMode then
    begin
      DecodeTime(now,H,M,S,Ms);
      LayoutHour.RotationAngle   := (H*30)+(M/2);
      LayoutMinute.RotationAngle := M*6;
      LayoutSecond.RotationAngle := S*6;

      Text:=FormatDateTime('dd',now);
      if Text<>EdtDay.text then
        EdtDay.text   := Text;
      Text:=FormatDateTime('mmm',now);
      if Text<>EdtMonth.text then
        EdtMonth.Text := FormatDateTime('mmm',now);
    end
    else
    begin
      if (BStartTimer) and not(BPauseTimer) then
      begin
        H := Trunc(StopWatch.Elapsed.Hours);
        M := Trunc(StopWatch.Elapsed.Minutes);
        S := Trunc(StopWatch.Elapsed.Seconds);
        Ms:= Trunc(StopWatch.Elapsed.Milliseconds);
        LayoutHour.RotationAngle   := (H*30)+(M/2);
        LayoutMinute.RotationAngle := M*6;
        LayoutSecond.RotationAngle := S*6;
        DtTimer                    := EncodeTime(H,M,S,Ms);
        EdtTimer.Text              := Copy(FormatDateTime('hh:nn:ss.zzz',DtTimer), 1 , 11);
      end;
    end;
  end;
end;


procedure TFrmMain.FltAnimLayoutSecondFinish(Sender: TObject);
begin
  BInitTimer               := False;
  TmrTime.Enabled          := True;
  RectBtReset.Visible      := True;
  RectBtStartStop.Visible  := True;
  EdtTimer.Text            := '00:00:00.00';
  EdtTimer.Visible         := True;
  InitBtStartStop(True);
end;

procedure TFrmMain.FormCreate(Sender: TObject);
var
  Style: TFMXObject;
begin
  {$IFNDEF WIN32}
     Self.BorderStyle:=TFmxFormBorderStyle.bsNone;
  {$ELSE}
     Self.BorderStyle:=TFmxFormBorderStyle.bsSizeable;
  {$ENDIF}

  Style := TStyleManager.LoadFromResource(HInstance, 'Jet', RT_RCDATA);
  if Style <> nil then
    TStyleManager.SetStyle(Style);
  BClockMode  := True;
  BInitTimer  := False;
  BStartTimer := False;
  BPauseTimer := False;
  InitClockTicks;
end;

procedure TFrmMain.InitClockTicks;
var
  i: Integer;
  Angle:Integer;
  layoutTick:TLayout;
begin
  for i := 3 to 60 do
  begin
    Angle:=i*6;
    if (Angle mod 5)=0 then
    begin
      layoutTick:=LayoutTickHour.Clone(LayoutClock) as TLayout;
    end
    else
    begin
      layoutTick:=LayoutTickMinute.Clone(LayoutClock) as TLayout;
    end;
    layoutTick.RotationAngle := Angle;
    layoutTick.Parent        := LayoutClock;
  end;
  LayoutClock.Repaint;
end;

procedure TFrmMain.BtClockClick(Sender: TObject);
begin
  if not(BClockMode) then
  begin
    BClockMode:=True;
    RectBtClock.Fill.Color  := TAlphaColorRec.Red;
    RectBtTimer.Fill.Color  := TAlphaColorRec.Black;
    RectBtReset.Visible     := False;
    RectBtStartStop.Visible := False;
    EdtTimer.Visible        := False;
    EdtDay.Visible          := True;
    EdtMonth.Visible        := True;
  end;
end;

procedure TFrmMain.BtInfoClick(Sender: TObject);
begin
  FrmInfo.LayoutBackInfo.Position.Y := Application.MainForm.Height;
  FrmInfo.LayoutBackInfo.Parent     := Self;
  FrmInfo.LayoutBackInfo.Visible    := True;
  FrmInfo.LayoutBackInfo.BringToFront;
  FrmInfo.FltAnimPositionY.StartValue := Application.MainForm.Height;
  FrmInfo.FltAnimPositionY.Start;
end;

procedure TFrmMain.BtResetClick(Sender: TObject);
begin
  InitTimer;
end;

procedure TFrmMain.BtStartStopClick(Sender: TObject);
begin
  if not(BStartTimer) then
  begin
    StopWatch                := TStopwatch.StartNew;
    BStartTimer              := True;
    InitBtStartStop(False);
  end
  else
  begin
    if not(BPauseTimer) then
    begin
      StopWatch.Stop;
      InitBtStartStop(True);
      RectBtReset.Fill.Color := TAlphaColorRec.Red;
    end
    else
    begin
      StopWatch.Start;
      InitBtStartStop(False);
    end;
    BPauseTimer:=not(BPauseTimer);
  end;
end;

procedure TFrmMain.BtTimerClick(Sender: TObject);
begin
  if BClockMode then
  begin
    EdtDay.Visible         := False;
    EdtMonth.Visible       := False;
    BClockMode             := False;
    BStartTimer            := False;
    BPauseTimer            := False;
    RectBtClock.Fill.Color := TAlphaColorRec.Black;
    RectBtTimer.Fill.Color := TAlphaColorRec.Red;
    InitTimer;
  end;
end;

procedure TfrmMain.InitTimer;
begin
  BInitTimer                     := True;
  BStartTimer                    := False;
  BPauseTimer                    := False;
  TmrTime.Enabled                := False;
  FltAnimLayoutHour.StartValue   := LayoutHour.RotationAngle;
  FltAnimLayoutMinute.StartValue := LayoutMinute.RotationAngle;
  FltAnimLayoutSecond.StartValue := LayoutSecond.RotationAngle;
  FltAnimLayoutHour.Start;
  FltAnimLayoutMinute.Start;
  FltAnimLayoutSecond.Start;
end;

procedure TfrmMain.InitBtStartStop(BStart:Boolean);
begin
  if BStart then
  begin
    LblStartStop.Text          := 'START';
    RectBtStartStop.Fill.Color := TAlphaColorRec.Black;
    RectBtReset.Fill.Color     := TAlphaColorRec.Black;
  end
  else
  begin
    LblStartStop.Text          := 'STOP';
    RectBtStartStop.Fill.Color := TAlphaColorRec.Green;
    RectBtReset.Fill.Color     := TAlphaColorRec.Red;
  end;
end;

end.
