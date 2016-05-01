
//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit MainTestBed;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Objects, FMX.ListBox, FMX.Layouts,
  FMX.Controls.Presentation, Box2D.Collision, Box2D.Common, Box2D.Dynamics, Box2D.Rope, Box2DTypes, Test;

type
  TForm6 = class(TForm)
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
  private
    { Private declarations }
    testIndex: Int32;
    testSelection: Int32;
    testCount: Int32;
    entry: PTestEntry;
    test: PTest;
    settings: PSettings;
    rightMouseDown: Boolean;
    lastp: b2Vec2;

    procedure DrawLayoutChanged(Sender: TObject);
  public
    { Public declarations }
    procedure LoadTests;
    procedure TestChanged;
    procedure ResetView;
  end;

var
  Form6: TForm6;

implementation

{$R *.fmx}

procedure TForm6.DrawLayoutChanged(Sender: TObject);
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

procedure TForm6.FormCreate(Sender: TObject);
begin
  LoadTests;

end;

procedure TForm6.FormDestroy(Sender: TObject);
begin
  Timer1.Enabled := False;
//  Test* t = test;
//  test = 0;
//  delete t;

end;

procedure TForm6.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
//  bool modCtrl = Shift.Contains(ssCtrl);
//  switch(Key)
//  {
//  case vkEscape:
//    // Quit
//    Close();
//    break;
//
//  case vkLeft:
//    // Pan left
//    if (modCtrl && test)
//    {
//      b2Vec2 newOrigin(2.0f, 0.0f);
//      test->ShiftOrigin(newOrigin);
//    }
//    else
//    {
//      g_camera.m_center.x -= 0.5f;
//      g_debugDraw.OffsetX = g_debugDraw.OffsetX-1;
//    }
//    break;
//
//  case vkRight:
//    // Pan right
//    if (modCtrl && test)
//    {
//      b2Vec2 newOrigin(-2.0f, 0.0f);
//      test->ShiftOrigin(newOrigin);
//    }
//    else
//    {
//      g_camera.m_center.x += 0.5f;
//      g_debugDraw.OffsetX = g_debugDraw.OffsetX+1;
//    }
//    break;
//
//  case vkDown:
//    // Pan down
//    if (modCtrl && test)
//    {
//      b2Vec2 newOrigin(0.0f, 2.0f);
//      test->ShiftOrigin(newOrigin);
//    }
//    else
//    {
//      g_camera.m_center.y -= 0.5f;
//      g_debugDraw.OffsetY = g_debugDraw.OffsetY-1;
//    }
//    break;
//
//  case vkUp:
//    // Pan up
//    if (modCtrl && test)
//    {
//      b2Vec2 newOrigin(0.0f, -2.0f);
//      test->ShiftOrigin(newOrigin);
//    }
//    else
//    {
//      g_camera.m_center.y += 0.5f;
//      g_debugDraw.OffsetY = g_debugDraw.OffsetY+1;
//    }
//    break;
//
//  case vkHome:
//    // Reset view
//    g_camera.m_zoom = 1.0f;
//    g_camera.m_center.Set(0.0f, 20.0f);
//    break;
//
//  case vkSpace:
//    // Launch a bomb.
//    if (test)
//    {
//      test->LaunchBomb();
//    }
//    break;
//
//  default:
//    switch (KeyChar)
//    {
//    case 'P':
//    case 'p':
//      // Pause
//      settings.pause = !settings.pause;
//      break;
//
//    case ' ':
//      // Launch a bomb.
//      if (test)
//      {
//        test->LaunchBomb();
//      }
//      break;
//
//    case 'R':
//    case 'r':
//      // Reset test
//      delete test;
//      test = entry->createFcn();
//      break;
//
//    default:
//      if (test)
//      {
//        test->Keyboard(static_cast<int>(ToUpper(KeyChar)));
//      }
//    }
//  }

end;

procedure TForm6.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
//  if (test)
//  {
//    test->KeyboardUp(static_cast<int>(ToUpper(KeyChar)));
//  }

end;

procedure TForm6.FormResize(Sender: TObject);
begin
//  ResetView();

end;

procedure TForm6.FormShow(Sender: TObject);
begin
//  ResetView();
//  DrawLayoutChanged(0);
//  TestChanged();

end;

procedure TForm6.LoadTests;
begin
//  testCount = 0;
//  while (g_testEntries[testCount].createFcn != NULL)
//  {
//    TestEntry& entry = g_testEntries[testCount];
//    TestList->Items->Add(entry.name);
//    ++testCount;
//  }
//
//  TestList->ItemIndex = 0;
//  testSelection = 0;
//  testIndex = -1;
//  test = 0;

end;

procedure TForm6.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
//  if (Button == TMouseButton::mbLeft)
//  {
//    if (test)
//    {
//      b2Vec2 pt = g_debugDraw.ScreenToWorld(X, Y);
//      if (Shift.Contains(ssShift))
//        test->ShiftMouseDown(pt);
//      else
//        test->MouseDown(pt);
//    }
//  }

end;

procedure TForm6.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
//  if (test)
//  {
//    b2Vec2 pt = g_debugDraw.ScreenToWorld(X, Y);
//    test->MouseMove(pt);
//  }

end;

procedure TForm6.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
//  if (test)
//  {
//    b2Vec2 pt = g_debugDraw.ScreenToWorld(X, Y);
//    test->MouseUp(pt);
//  }

end;

procedure TForm6.PaintBoxPaint(Sender: TObject; Canvas: TCanvas);
begin
//  Canvas->BeginScene();
//  __try {
//    Canvas->Fill->Color = TAlphaColorRec::Black;
//    TRectF rect = PaintBox->BoundsRect;
//    Canvas->FillRect(rect, 0, 0, TCorners(), DEFAULT_OPACITY);
//    if (test)
//    {
//      g_debugDraw.Canvas = Canvas;
//      g_debugDraw.Canvas->Fill->Color = TAlphaColorRec::Yellow;
//      test->DrawTitle(entry->name);
//      g_debugDraw.Canvas->Fill->Color = TAlphaColorRec::Aqua;
//      test->Step(&settings);
//    }
//  }
//  __finally {
//    Canvas->EndScene();
//  }

end;

procedure TForm6.PauseBtnClick(Sender: TObject);
begin
//  settings.pause = !settings.pause;

end;

procedure TForm6.QuiBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TForm6.ResetView;
begin
//  g_debugDraw.OffsetX = PaintBox->Width/2;
//  g_debugDraw.OffsetY = PaintBox->Height/2;
//  g_debugDraw.ScaleX = 10;
//  g_debugDraw.ScaleY = 10;
//  g_debugDraw.CanvasHeight = PaintBox->Height;
//  g_camera.m_width = PaintBox->Width;
//  g_camera.m_height = PaintBox->Height;
//  g_camera.m_center.Set(g_debugDraw.OffsetX, g_debugDraw.OffsetY);

end;

procedure TForm6.RestartBtnClick(Sender: TObject);
begin
//  delete test;
//  test = entry->createFcn();

end;

procedure TForm6.SingleStepBtnClick(Sender: TObject);
begin
//  settings.singleStep = !settings.singleStep;

end;

procedure TForm6.TestChanged;
begin
//  if (testSelection != testIndex)
//  {
//    testIndex = testSelection;
//    delete test;
//    entry = g_testEntries + testIndex;
//    test = entry->createFcn();
//  }

end;

procedure TForm6.TestListChange(Sender: TObject);
begin
//  if (TestList->ItemIndex != testSelection)
//  {
//    testSelection = TestList->ItemIndex;
//    TestChanged();
//  }

end;

procedure TForm6.Timer1Timer(Sender: TObject);
begin
//  if (test)
//  {
//    Invalidate();
//  }

end;

end.
