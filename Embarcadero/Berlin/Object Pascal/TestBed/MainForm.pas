//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Objects, FMX.ListBox,
  FMX.Layouts, FMX.Controls.Presentation,
  Box2D.Collision, Box2D.Common, Box2D.Dynamics, Box2D.Rope, Box2DTypes, Test;

type
  TTestBedForm = class(TForm)
    MainPanel: TPanel;
    RightPanel: TPanel;
    RightLayout: TLayout;
    TestLabel: TLabel;
    TestList: TComboBox;
    Line1: TLine;
    EnableLayout: TLayout;
    Sleep: TCheckBox;
    WarmStarting: TCheckBox;
    TimeOfImpact: TCheckBox;
    SubStepping: TCheckBox;
    DrawLayout: TLayout;
    ShapesChk: TCheckBox;
    AABBsChk: TCheckBox;
    ContactPointsChk: TCheckBox;
    ContactNormalsChk: TCheckBox;
    ContactImpulsesChk: TCheckBox;
    FrictionImpulsesChk: TCheckBox;
    CenterOfMassesChk: TCheckBox;
    ProfileChk: TCheckBox;
    StatisticsChk: TCheckBox;
    JointsChk: TCheckBox;
    ButtonsLayout: TLayout;
    QuiBtn: TButton;
    RestartBtn: TButton;
    SingleStepBtn: TButton;
    PauseBtn: TButton;
    PaintBox: TPaintBox;
    RightSplitter: TSplitter;
    Timer1: TTimer;
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject; Canvas: TCanvas);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TestListChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure PauseBtnClick(Sender: TObject);
    procedure SingleStepBtnClick(Sender: TObject);
    procedure RestartBtnClick(Sender: TObject);
    procedure QuiBtnClick(Sender: TObject);
    procedure DrawLayoutChanged(Sender: TObject);
  private
    { Private declarations }
    testIndex: Int32;
    testSelection: Int32;
    testCount: Int32;
    entry: PTestEntry;
    test: TTest;
    settings: Settings;
    rightMouseDown: Boolean;
    lastp: b2Vec2;
  public
    { Public declarations }
    procedure LoadTests;
    procedure TestChanged;
    procedure ResetView;
  end;

var
  TestBedForm: TTestBedForm;

implementation

{$R *.fmx}

uses
  System.Character,
  DebugDraw;

procedure TTestBedForm.DrawLayoutChanged(Sender: TObject);
begin
  settings.drawShapes := ShapesChk.IsChecked;
  settings.drawJoints := JointsChk.IsChecked;
  settings.drawAABBs := AABBsChk.IsChecked;
  settings.drawContactPoints := ContactPointsChk.IsChecked;
  settings.drawContactNormals := ContactNormalsChk.IsChecked;
  settings.drawContactImpulse := ContactImpulsesChk.IsChecked;
  settings.drawFrictionImpulse := FrictionImpulsesChk.IsChecked;
  settings.drawCOMs := CenterOfMassesChk.IsChecked;
  settings.drawStats := StatisticsChk.IsChecked;
  settings.drawProfile := ProfileChk.IsChecked;
end;

procedure TTestBedForm.FormCreate(Sender: TObject);
begin
  settings := Settings.Create;
  LoadTests;

end;

procedure TTestBedForm.FormDestroy(Sender: TObject);
begin
  Timer1.Enabled := False;
  test.Free;
  test := nil;
end;

procedure TTestBedForm.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
var
  modCtrl: Boolean;
  newOrigin: b2Vec2;
begin
  modCtrl := ssCtrl in Shift;
  case Key of
    vkEscape: Close;

    vkLeft:
    begin
      if modCtrl and (test <> nil) then
      begin
        newOrigin := b2vec2.Create(2.0, 0.0);
        test.ShiftOrigin(newOrigin);
      end
      else
      begin
        g_camera.m_center.x  :=  g_camera.m_center.x - 1.0;
      end;

    end;

    vkRight:
      if modCtrl and (test <> nil) then
      begin
        newOrigin := b2vec2.Create(-2.0, 0.0);
        test.ShiftOrigin(newOrigin);
      end
      else
      begin
        g_camera.m_center.x := g_camera.m_center.x + 1.0;
      end;

    vkDown:
      if modCtrl and (test <> nil) then
      begin
        newOrigin := b2vec2.Create(0.0, 2.0);
        test.ShiftOrigin(newOrigin);
      end
      else
      begin
        g_camera.m_center.y := g_camera.m_center.y - 1.0;
      end;

    vkUp:
      if modCtrl and (test <> nil) then
      begin
        newOrigin := b2vec2.Create(0.0, -2.0);
        test.ShiftOrigin(newOrigin);
      end
      else
      begin
        g_camera.m_center.y := g_camera.m_center.y + 1.0;
      end;

    vkHome:
    begin
      // Reset view
      g_camera.m_zoom := 1.0;
      g_camera.m_center.&Set(0.0, 20.0);
    end;

    vkSpace:
      if test <> nil then
      begin
        test.LaunchBomb;
      end;
  else
    case KeyChar of
      'P', 'p':
        settings.pause := not settings.pause;

      ' ':
        if test <> nil then
        begin
          test.LaunchBomb;
        end;

      'R', 'r':
        begin
          test.Free;
          test := entry^.createFcn;
        end;
    else
      if test <> nil then
      begin
        test.Keyboard(Ord(KeyChar.ToUpper));
      end;
    end;
  end;

end;

procedure TTestBedForm.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if test <> nil then
  begin
    test.KeyboardUp(Ord(KeyChar.ToUpper));
  end;
end;

procedure TTestBedForm.FormResize(Sender: TObject);
begin
  ResetView;
end;

procedure TTestBedForm.FormShow(Sender: TObject);
begin
  ResetView;
  DrawLayoutChanged(nil);
  TestChanged;
end;

procedure TTestBedForm.LoadTests;
var
  entry: PTestEntry;
begin
  testCount := 0;
  while (testCount < Length(g_testEntries)) do
  begin
    entry := @g_testEntries[testCount];
    TestList.Items.Add(entry^.name);
    Inc(testCount);
  end;

  TestList.ItemIndex := 0;
  testSelection := 0;
  testIndex := -1;
  test := nil;
end;

procedure TTestBedForm.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  pt: b2Vec2;
begin
  if (Button = TMouseButton.mbLeft) and (test <> nil) then
  begin
    pt := g_debugDraw.ScreenToWorld(X, Y);
    if ssShift in Shift then
    begin
      test.ShiftMouseDown(pt);
    end
    else
    begin
      test.MouseDown(pt);
    end;
  end;
end;

procedure TTestBedForm.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
var
  pt: b2Vec2;
begin
  if test <> nil then
  begin
    pt := g_debugDraw.ScreenToWorld(X, Y);
    test.MouseMove(pt);
  end;
end;

procedure TTestBedForm.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  pt: b2Vec2;
begin
  if test <> nil then
  begin
    pt := g_debugDraw.ScreenToWorld(X, Y);
    test.MouseUp(pt);
  end;

end;

procedure TTestBedForm.PaintBoxPaint(Sender: TObject; Canvas: TCanvas);
var
  rect: TRectF;
begin
  Canvas.BeginScene;
  try
    Canvas.Fill.Color := b2Color2TColor(b2Color.Create(0.3, 0.3, 0.3, 1));
    rect := PaintBox.BoundsRect;
    Canvas.FillRect(rect, 0, 0, [], DEFAULT_OPACITY);

    if test <> nil then

    begin

      g_debugDraw.Canvas := Canvas;

      g_debugDraw.Canvas.Fill.Color := TAlphaColorRec.Yellow;

      test.DrawTitle(entry.name);

      g_debugDraw.Canvas.Fill.Color := TAlphaColorRec.Aqua;

      test.Step(@settings);

    end;

  finally
    Canvas.EndScene;
  end;
end;

procedure TTestBedForm.PauseBtnClick(Sender: TObject);
begin
  settings.pause := not settings.pause;
end;

procedure TTestBedForm.QuiBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TTestBedForm.ResetView;
begin
  g_debugDraw.CanvasHeight := PaintBox.Height;
  g_camera.m_width := Round(PaintBox.Width);
  g_camera.m_height:= Round(PaintBox.Height);
  g_camera.m_center.&Set(PaintBox.Width/2, PaintBox.Height/2);
end;

procedure TTestBedForm.RestartBtnClick(Sender: TObject);
begin
  test.Free;
  test := entry.createFcn;
end;

procedure TTestBedForm.SingleStepBtnClick(Sender: TObject);
begin
  settings.singleStep := not settings.singleStep;
end;

procedure TTestBedForm.TestChanged;
begin
  if (testSelection <> testIndex) then
  begin
    testIndex := testSelection;
    test.Free;
    entry := @g_testEntries[testIndex];
    test := entry^.createFcn;
  end;

end;

procedure TTestBedForm.TestListChange(Sender: TObject);
begin
  if (TestList.ItemIndex <> testSelection) then
  begin
    testSelection := TestList.ItemIndex;
    TestChanged;
  end;
end;

procedure TTestBedForm.Timer1Timer(Sender: TObject);
begin
  if test <> nil then
    Invalidate;
end;


end.
