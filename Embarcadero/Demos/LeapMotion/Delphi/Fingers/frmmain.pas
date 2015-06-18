unit frmmain;

{$ifdef fpc}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, System.UITypes, wsleap, leapdata;

type

  { TForm1 }

  TForm1 = class(TForm)
    BStart: TButton;
    BStop: TButton;
    Frame: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    LFPS: TLabel;
    LVersion: TLabel;
    LFrameCount: TLabel;
    Memo1: TMemo;
    PB: TPaintBox;
    Timer1: TTimer;
    Panel1: TPanel;
    procedure BStartClick(Sender: TObject);
    procedure BStopClick(Sender: TObject);
    procedure DoNewFrame(Sender: TObject; AFrame: TFrame);
    procedure DoVersion(Sender: TObject);
    procedure PBPaint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FLastFrame: leapdata.TFrame;
    FLastFrames, FFrames: Integer;
    FC: TWebsocketLeapController;
    procedure DumpFrame(AFrame: TFrame; Dump: TStrings);
    { private declarations }
    const Rainbow: array[0..9] of TColor = (
      TColors.Red, TColors.Blue, TColors.Orange, TColors.Green,
      TColors.Gold, TColors.Hotpink, TColors.Turquoise, TColors.Indigo,
      TColors.Brown, TColors.Violet);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
{ TForm1 }

procedure TForm1.BStartClick(Sender: TObject);
begin
  FLastFrames := 0;
  FFrames := 0;
  FC := TWebsocketLeapController.Create(Self);
  FC.EnableGestures := True;
  FC.OnFrame := DoNewFrame;
  FC.OnVersion := DoVersion;
  FC.ResolveFrames := True;
  FC.Enabled := True;
end;

procedure TForm1.BStopClick(Sender: TObject);
begin
  FC.Enabled := False;
  Application.ProcessMessages;
end;

procedure TForm1.DumpFrame(AFrame: TFrame; Dump: TStrings);

begin
  if AFrame = Nil then
  begin
    Dump.Add('Frame is nil');
    exit;
  end;
  With Dump do
  begin
    Add('Frame ' + IntToStr(AFrame.ID));
    Add('  Timestamp: ' + IntToStr(AFrame.Timestamp));
    Add('  Hands : ' + IntToStr(AFrame.Hands.Count));
    Add('  Pointables : ' + IntToStr(AFrame.Pointables.Count));
    Add('  Fingers: ' + IntToStr(AFrame.Fingers.Count));
    Add('  Tools: ' + IntToStr(AFrame.Tools.Count));
    Add('  Gestures: ' + IntToStr(AFrame.Gestures.Count));
  end;
end;

procedure TForm1.DoNewFrame(Sender: TObject; AFrame: TFrame);
begin
  Inc(FFrames);
  LFrameCount.Caption := IntToStr(AFrame.ID);
  Memo1.Lines.BeginUpdate;
  Memo1.Lines.Clear;
  DumpFrame(AFrame, Memo1.Lines);
  FLastFrame := AFrame;
  Memo1.Lines.EndUpdate;
  PB.Invalidate;
end;

procedure TForm1.DoVersion(Sender: TObject);
begin
  LVersion.Caption := IntToStr(FC.Version);
end;

procedure TForm1.PBPaint(Sender: TObject);
Var
  P: T3DVector;
  i, R: Integer;
  CX, CY: Integer;
begin
  if Assigned(FLastFrame) then
  begin
    // PB.Canvas.Clear;
    PB.Canvas.Brush.Color := clBlack;
    PB.Canvas.Brush.Style := bsSolid;
    For i := 0 to FLastFrame.Pointables.Count - 1 do
    begin
      if i < length(Rainbow) then
        PB.Canvas.Brush.Color := Rainbow[i]
      else
        PB.Canvas.Brush.Color := clBlack;
      P := FLastFrame.Pointables[i].TipPosition;
      CX := (PB.Width div 2) + Round(P.X);
      CY := (PB.Height) - Round(P.Y);
      if (P.Z = 0) then
        R := 10
      else
      begin
        R := Round(600 / Abs(P.Z));
        if R > 20 then
          R := 20;
      end;
      PB.Canvas.Ellipse(CX - R, CY - R, CX + R, CY + R);
    end;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
Var
  FPS: Integer;
begin
  FPS := FFrames - FLastFrames;
  LFPS.Caption := IntToStr(FPS);
  FLastFrames := FFrames;
end;

end.
