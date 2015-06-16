unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Graphics,
  FMX.Types, FMX.Controls, FMX.Forms3D, FMX.Dialogs, FMX.Types3D, FMX.Objects3D,
  FMX.Layers3D, FMX.Edit, FMX.Layouts, FMX.Memo, FMX.Objects, FMX.Colors,
  FMX.ListBox, FMX.Ani, FMX.TabControl, FMX.Effects, FMX.ExtCtrls, FMX.Menus,
  FMX.Materials, FMX.MaterialSources, FMX.Controls3D, FMX.StdCtrls;

type
  TFrmMain = class(TForm3D)
    Text3D: TText3D;
    Light1: TLight;
    Rectangle: TRectangle3D;
    MemoText: TMemo;
    LayoutLeft: TLayout;
    FrameTextSet: TRectangle;
    HorizMargin: TTrackBar;
    Label4: TLabel;
    GroupFrameCorners: TRectangle;
    CheckTopLeft: TCheckBox;
    CheckTopRight: TCheckBox;
    CheckBottomRight: TCheckBox;
    CheckBottomLeft: TCheckBox;
    RectDepth: TTrackBar;
    Label6: TLabel;
    RectRadius: TTrackBar;
    Label7: TLabel;
    Text3DDepth: TTrackBar;
    VertMargin: TTrackBar;
    Label5: TLabel;
    ColorPanelFrame: TColorPanel;
    ColorAnimation1: TColorAnimation;
    ListFrame: TListBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    VertScrollBox1: TVertScrollBox;
    ExpanderFrame: TExpander;
    ExpanderText: TExpander;
    ColorPanelText: TColorPanel;
    ListText: TListBox;
    ListBoxItem4: TListBoxItem;
    ListBoxItem5: TListBoxItem;
    ExpanderExport: TExpander;
    LayoutRight: TLayout;
    Circle1: TCircle;
    CornerList: TListBox;
    ListBoxItem6: TListBoxItem;
    ListBoxItem7: TListBoxItem;
    ListBoxItem8: TListBoxItem;
    ListBoxItem9: TListBoxItem;
    Rectangle2: TRectangle;
    Layout3: TLayout;
    Layout4: TLayout;
    Rectangle3: TRectangle;
    Layout5: TLayout;
    Rectangle4: TRectangle;
    Layout6: TLayout;
    Rectangle5: TRectangle;
    ExpanderBack: TExpander;
    ColorPanelBackgroud: TColorPanel;
    GlowEffect1: TGlowEffect;
    ArrowUp: TPath;
    ArrowDown: TPath;
    ArrowLeft: TPath;
    ArrowRight: TPath;
    LayoutNavigator: TLayout;
    SBUp: TSpeedButton;
    SBDown: TSpeedButton;
    SBLeft: TSpeedButton;
    SBRight: TSpeedButton;
    TrackBarObjectDepth: TTrackBar;
    GlowEffect2: TGlowEffect;
    GlowEffect3: TGlowEffect;
    EdFileName: TEdit;
    Label1: TLabel;
    BtExport: TCornerButton;
    Layer3DLeft: TLayer3D;
    Layer3DRight: TLayer3D;
    StyleBook1: TStyleBook;
    Layer3DCenter: TLayer3D;
    FrameTexture: TImage;
    OpenDialogImages: TOpenDialog;
    Panel1: TPanel;
    Label2: TLabel;
    Panel2: TPanel;
    TextTexture: TImage;
    Label3: TLabel;
    CleanFrameTexture: TSpeedButton;
    Path1: TPath;
    CleanTextTexture: TSpeedButton;
    Path2: TPath;
    MainDummy: TDummy;
    DummyY: TDummy;
    DummyX: TDummy;
    Camera1: TCamera;
    SBFont: TSpeedButton;
    Path3: TPath;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    FloatAnimationCameraY: TFloatAnimation;
    FloatAnimationCameraX1: TFloatAnimation;
    FloatAnimationCameraX2: TFloatAnimation;
    FloatAnimationCameraX3: TFloatAnimation;
    RectangleMaterialShaftSource: TLightMaterialSource;
    RectangleMaterialBackSource: TLightMaterialSource;
    RectangleMaterialSource: TLightMaterialSource;
    Text3DMaterialSource: TLightMaterialSource;
    Text3DMaterialShaftSource: TLightMaterialSource;
    Text3DMaterialBackSource: TLightMaterialSource;
    procedure Viewport3D1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
    procedure Viewport3D1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Viewport3D1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Single);
    procedure MemoTextChange(Sender: TObject);
    procedure HorizMarginChange(Sender: TObject);
    procedure VertMarginChange(Sender: TObject);
    procedure RectDepthChange(Sender: TObject);
    procedure RectRadiusChange(Sender: TObject);
    procedure CheckTopRightChange(Sender: TObject);
    procedure CheckTopLeftChange(Sender: TObject);
    procedure CheckBottomLeftChange(Sender: TObject);
    procedure CheckBottomRightChange(Sender: TObject);
    procedure Text3DDepthChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ColorPanelFrameChange(Sender: TObject);
    procedure Text3DClick(Sender: TObject);
    procedure Viewport3D1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure ListFrameChange(Sender: TObject);
    procedure ListTextChange(Sender: TObject);
    procedure ColorPanelTextChange(Sender: TObject);
    procedure ColorPanelBackgroudChange(Sender: TObject);
    procedure CornerListChange(Sender: TObject);
    procedure SBUpClick(Sender: TObject);
    procedure SBDownClick(Sender: TObject);
    procedure SBLeftClick(Sender: TObject);
    procedure SBRightClick(Sender: TObject);
    procedure TrackBarObjectDepthChange(Sender: TObject);
    procedure ExpanderFrameCheckChange(Sender: TObject);
    procedure CornerButton1Click(Sender: TObject);
    procedure FrameTextureClick(Sender: TObject);
    procedure CleanFrameTextureClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure FloatAnimationCameraYProcess(Sender: TObject);
    procedure FloatAnimationCameraYFinish(Sender: TObject);
    procedure SBFontClick(Sender: TObject);
  private
    { Private declarations }
    Down: TPointF;
    FSelectObject: TShape3D;

    procedure SetFrameWidth(const Value: Single);
    function GetFrameWidth: Single;
    function GetFrameHeight: Single;
    procedure SetFrameHeight(const Value: Single);
    function GetFrameDepth: Single;
    procedure SetFrameDepth(const Value: Single);
    function GetFrameCorners: TCorners;
    procedure SetFrameCorners(const Value: TCorners);
    function GetFrameRadius: Single;
    procedure SetFrameRadius(const Value: Single);
    function GetTextDepth: Single;
    procedure SetTextDepth(const Value: Single);
    procedure SetFrameVisible(const Value: Boolean);
    function GetFrameVisible: Boolean;
    procedure InitalizeColorPanels;
    procedure IntializeMainControls;

  public
    { Public declarations }
    MouseUp, isAnimationRunning: Boolean;
    property FrameWidth: Single read GetFrameWidth write SetFrameWidth;
    property FrameHeight: Single read GetFrameHeight write SetFrameHeight;
    property FrameDepth: Single read GetFrameDepth write SetFrameDepth;
    property FrameCorners: TCorners read GetFrameCorners write SetFrameCorners;
    property FrameRadius: Single read GetFrameRadius write SetFrameRadius;
    property FrameVisible: Boolean read GetFrameVisible write SetFrameVisible;
    property SelectedObject: TShape3D read FSelectObject write FSelectObject;

    property TextDepth: Single read GetTextDepth write SetTextDepth;

    procedure InitalizeText;
    procedure InitializeControls;

    procedure ResetAll;
    function GetMaterialSource(AList: TListBox; AnObject: TObject): TLightMaterialSource;
  end;

var
  FrmMain: TFrmMain;

Const
  MFRONT = 1;
  MLEFT = 2;
  MBACK = 3;

implementation

uses
  System.Math.Vectors;
{$R *.fmx}

procedure TFrmMain.InitalizeText;
begin
  MemoText.Text := Text3D.Text;
end;

procedure TFrmMain.CheckBottomLeftChange(Sender: TObject);
begin
  if CheckBottomLeft.IsChecked then
    FrameCorners := FrameCorners + [TCorner.BottomLeft]
  else
    FrameCorners := FrameCorners - [TCorner.BottomLeft];
end;

procedure TFrmMain.CheckBottomRightChange(Sender: TObject);
begin
  if CheckBottomRight.IsChecked then
    FrameCorners := FrameCorners + [TCorner.BottomRight]
  else
    FrameCorners := FrameCorners - [TCorner.BottomRight];
end;

procedure TFrmMain.CheckTopLeftChange(Sender: TObject);
begin
  if CheckTopLeft.IsChecked then
    FrameCorners := FrameCorners + [TCorner.TopLeft]
  else
    FrameCorners := FrameCorners - [TCorner.TopLeft];
end;

procedure TFrmMain.CheckTopRightChange(Sender: TObject);
begin
  if CheckTopRight.IsChecked then
    FrameCorners := FrameCorners + [TCorner.TopRight]
  else
    FrameCorners := FrameCorners - [TCorner.TopRight];
end;

procedure TFrmMain.ColorPanelFrameChange(Sender: TObject);
begin
  case ListFrame.ItemByIndex(ListFrame.ItemIndex).Tag of
    MFRONT:
      RectangleMaterialSource.Diffuse := ColorPanelFrame.Color;
    MLEFT:
      RectangleMaterialShaftSource.Diffuse := ColorPanelFrame.Color;
    MBACK:
      RectangleMaterialBackSource.Diffuse := ColorPanelFrame.Color;
  end;

end;

procedure TFrmMain.ColorPanelTextChange(Sender: TObject);
begin
  case ListText.ItemByIndex(ListText.ItemIndex).Tag of
    MFRONT:
      Text3DMaterialSource.Diffuse := ColorPanelText.Color;
    MLEFT:
      Text3DMaterialShaftSource.Diffuse := ColorPanelText.Color;
    MBACK:
      Text3DMaterialBackSource.Diffuse := ColorPanelText.Color;
  end;

end;

procedure TFrmMain.ColorPanelBackgroudChange(Sender: TObject);
begin
  Color := ColorPanelBackgroud.Color;
end;

procedure TFrmMain.FloatAnimationCameraYFinish(Sender: TObject);
begin
  isAnimationRunning := False
end;

procedure TFrmMain.FloatAnimationCameraYProcess(Sender: TObject);
begin
  isAnimationRunning := True;
end;

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  InitializeControls;

end;

procedure TFrmMain.FrameTextureClick(Sender: TObject);
var
  FileName: String;
  Obj: TExtrudedShape3D;
  List: TListBox;
  TempLightMaterial: TLightMaterialSource;
begin
  Obj := nil;
  List := nil;
  if Sender = TextTexture then
  begin
    Obj := Text3D;
    List := ListText;
  end
  else
  begin
    if Sender = FrameTexture then
    begin
      Obj := Rectangle;
      List := ListFrame;
    end;
  end;
  TempLightMaterial := GetMaterialSource(List, Obj);
  if OpenDialogImages.Execute then
    FileName := OpenDialogImages.FileName;
  if Assigned(Obj) and Assigned(List) then
  begin
    TempLightMaterial.Texture.LoadFromFile(FileName);
    TImage(Sender).Bitmap.LoadFromFile(FileName);
  end;
end;

function TFrmMain.GetFrameCorners: TCorners;
begin
  Result := Rectangle.Corners;
end;

function TFrmMain.GetFrameDepth: Single;
begin
  Result := Rectangle.Depth;
end;

function TFrmMain.GetFrameHeight: Single;
begin
  Result := Rectangle.Height;
end;

function TFrmMain.GetFrameRadius: Single;
begin
  Result := Rectangle.XRadius
end;

function TFrmMain.GetFrameVisible: Boolean;
begin
  Result := (Rectangle.Sides = [TExtrudedShapeSide.Front,
    TExtrudedShapeSide.Back, TExtrudedShapeSide.Shaft])
end;

function TFrmMain.GetFrameWidth: Single;
begin
  Result := Rectangle.Width;
end;

function TFrmMain.GetMaterialSource(AList: TListBox; AnObject: TObject): TLightMaterialSource;
begin
  Result := nil;
  if AnObject = Rectangle then
  begin
    case AList.ItemByIndex(AList.ItemIndex).Tag of
      MFRONT:
        Result := RectangleMaterialSource;
      MLEFT:
        Result := RectangleMaterialShaftSource;
      MBACK:
        Result := RectangleMaterialBackSource;
    end;
  end;
  if AnObject = Text3d then
  begin
    case AList.ItemByIndex(AList.ItemIndex).Tag of
      MFRONT:
        Result := Text3dMaterialSource;
      MLEFT:
        Result := Text3dMaterialShaftSource;
      MBACK:
        Result := Text3dMaterialBackSource;
    end;
  end;
end;

function TFrmMain.GetTextDepth: Single;
begin
  Result := Text3D.Depth
end;

procedure TFrmMain.HorizMarginChange(Sender: TObject);
begin
  FrameWidth := HorizMargin.Value;
end;

procedure TFrmMain.InitializeControls;
Const
  SFilter : String = 'All Images (%s)|%s';
var
  ImageType : String;
begin

  ExpanderFrame.IsChecked := FrameVisible;

  ImageType := TBitmapCodecManager.GetFileTypes;
  OpenDialogImages.Filter := Format(SFilter, [ImageType, ImageType]);

  IntializeMainControls;
  InitalizeColorPanels;

  TrackBarObjectDepth.Value := -Camera.Position.Z;

  InitalizeText;

end;

procedure TFrmMain.CornerButton1Click(Sender: TObject);
var
  Image: TBitmap;
begin

  if Trim(EdFileName.Text) = EmptyStr then
  begin
    EdFileName.SetFocus;
    raise Exception.Create('Please, inform the image filename');
  end;

  Cursor := crHourGlass;
  Image := TBitmap.Create(0, 0);
  try
    MainDummy.CreateHighMultisampleSnapshot(Image,
      Round(MainDummy.ScreenBounds.Width), Round(MainDummy.ScreenBounds.Height),
      TAlphaColors.Null, 4);

    Image.SaveToFile(EdFileName.Text);

    if FileExists(EdFileName.Text) then
       MessageDlg('File generated at ' + EdFileName.Text, TMsgDlgType.mtWarning,
          [TMsgDlgBtn.MbOk], 0)
    else
       MessageDlg('File could not be saved on this folder', TMsgDlgType.mtError,
          [TMsgDlgBtn.MbOk], 0);
  finally
    Image.Free;
    Cursor := crDefault;
  end;

end;

procedure TFrmMain.CornerListChange(Sender: TObject);
begin
  Rectangle.CornerType := TCornerType(CornerList.ItemIndex);
end;

procedure TFrmMain.ExpanderFrameCheckChange(Sender: TObject);
begin
  FrameVisible := ExpanderFrame.IsChecked;
end;

procedure TFrmMain.ListFrameChange(Sender: TObject);
begin
  case ListFrame.ItemByIndex(ListFrame.ItemIndex).Tag of
    MFRONT:
      begin
        ColorPanelFrame.Color := RectangleMaterialSource.Diffuse;
        FrameTexture.Bitmap := RectangleMaterialSource.Texture;
      end;
    MLEFT:
      begin
        ColorPanelFrame.Color := RectangleMaterialShaftSource.Diffuse;
        FrameTexture.Bitmap := RectangleMaterialShaftSource.Texture;
      end;
    MBACK:
      begin
        ColorPanelFrame.Color := RectangleMaterialBackSource.Diffuse;
        FrameTexture.Bitmap := RectangleMaterialBackSource.Texture;
      end;
  end;

end;

procedure TFrmMain.ListTextChange(Sender: TObject);
begin
  case ListText.ItemByIndex(ListText.ItemIndex).Tag of
    MFRONT:
      begin
        ColorPanelText.Color := Text3DMaterialSource.Diffuse;
        TextTexture.Bitmap := Text3DMaterialSource.Texture;
      end;
    MLEFT:
      begin
        ColorPanelText.Color := Text3DMaterialShaftSource.Diffuse;
        TextTexture.Bitmap := Text3DMaterialShaftSource.Texture;
      end;
    MBACK:
      begin
        ColorPanelText.Color := Text3DMaterialBackSource.Diffuse;
        TextTexture.Bitmap := Text3DMaterialShaftSource.Texture;
      end;
  end;

end;

procedure TFrmMain.MemoTextChange(Sender: TObject);
var
  Str: String;
begin
  if MemoText.Text = EmptyStr then
    Str := ' '
  else
    Str := MemoText.Text;

  Text3D.Text := Str;

  FrameWidth  := Text3D.GetTextBounds.Width * 13;
  FrameHeight := Text3D.GetTextBounds.Height * 13;

//  MainDummy.Width  := Rectangle.Width;
//  MainDummy.Height := Rectangle.Height;
  MainDummy.Depth  := Rectangle.Depth + Text3D.Depth;
end;

procedure TFrmMain.RectDepthChange(Sender: TObject);
begin
  FrameDepth := RectDepth.Value;
end;

procedure TFrmMain.RectRadiusChange(Sender: TObject);
begin
  FrameRadius := RectRadius.Value;
end;

procedure TFrmMain.ResetAll;
begin
  FrameWidth := 19;
  FrameHeight := 5;
  FrameDepth := 3;
  FrameRadius := 2;

  FrameVisible := True;

  TextDepth := 3;

  MemoText.Lines.Text := '3D Text';

  FrameCorners := [TCorner.TopLeft, TCorner.BottomRight];
  CornerList.ItemIndex := 0;

  RectangleMaterialSource.Texture.SetSize(0, 0);
  RectangleMaterialShaftSource.Texture.SetSize(0, 0);
  RectangleMaterialBackSource.Texture.SetSize(0, 0);

  RectangleMaterialSource.Diffuse := TAlphaColorRec.Null;
  RectangleMaterialShaftSource.Diffuse := TAlphaColorRec.White;
  RectangleMaterialBackSource.Diffuse := TAlphaColorRec.White;

  Text3DMaterialSource.Texture.SetSize(0, 0);
  Text3DMaterialShaftSource.Texture.SetSize(0, 0);
  Text3DMaterialBackSource.Texture.SetSize(0, 0);

  Text3DMaterialSource.Diffuse := TAlphaColorRec.White;
  Text3DMaterialShaftSource.Diffuse := TAlphaColorRec.Darkgray;
  Text3DMaterialBackSource.Diffuse := TAlphaColorRec.White;

  DummyY.AnimateFloat('RotationAngle.Y', 0, 1.5, TAnimationType.InOut,
    TInterpolationType.Sinusoidal);
  DummyX.AnimateFloat('RotationAngle.X', 0, 1.5);

  Camera.AnimateFloat('Position.Z', -30, 1.5);
  Camera.Position.Vector := Vector3D(0, 0, 0);
  Camera.ResetRotationAngle;


end;

procedure TFrmMain.IntializeMainControls;
begin

//  Text3DDepth.Max := TextDepth * 2;
//  Text3DDepth.Value := TextDepth;

  //  SetTextDepth(Text3DDepth.Value);
  RectRadius.Value := FrameRadius;
  RectDepth.Value := FrameDepth;

  VertMargin.Value := FrameHeight;
  HorizMargin.Value := FrameWidth;

  CheckTopLeft.IsChecked := TCorner.TopLeft in FrameCorners;
  CheckTopRight.IsChecked := TCorner.TopRight in FrameCorners;
  CheckBottomLeft.IsChecked := TCorner.BottomLeft in FrameCorners;
  CheckBottomRight.IsChecked := TCorner.BottomRight in FrameCorners;

end;


procedure TFrmMain.InitalizeColorPanels;
begin
  ColorPanelFrame.Color := RectangleMaterialSource.Diffuse;
  ColorPanelText.Color := Text3DMaterialSource.Diffuse;
  ColorPanelBackgroud.Color := Self.Color;
end;

procedure TFrmMain.SetFrameCorners(const Value: TCorners);
begin
  Rectangle.Corners := Value;
end;

procedure TFrmMain.SetFrameDepth(const Value: Single);
begin
  Rectangle.Depth := Value;
  MainDummy.Depth := Value + TextDepth;
end;

procedure TFrmMain.SetFrameHeight(const Value: Single);
begin
  Rectangle.Height := Value;
  Text3D.Height := Value;
  MainDummy.Height := Rectangle.Height;
end;

procedure TFrmMain.SetFrameRadius(const Value: Single);
begin
  Rectangle.XRadius := Value;
  Rectangle.YRadius := Value;
end;

procedure TFrmMain.SetFrameVisible(const Value: Boolean);
begin
  if Value then
  begin
    Rectangle.Sides := [TExtrudedShapeSide.Front, TExtrudedShapeSide.Back,
      TExtrudedShapeSide.Shaft]
  end
  else
  begin
    Rectangle.Sides := []
  end;
end;

procedure TFrmMain.SetFrameWidth(const Value: Single);
begin
  Rectangle.Width := Value;
  Text3D.Width := Value;
  MainDummy.Width := Rectangle.Width;
end;

procedure TFrmMain.SetTextDepth(const Value: Single);
begin
  Text3D.Depth := Value;
  Text3D.Position.Z := (Value / 2) * -1;

  MainDummy.Depth := FrameDepth + TextDepth;

end;

procedure TFrmMain.SpeedButton1Click(Sender: TObject);
begin
  ResetAll;
  InitializeControls;
end;

procedure TFrmMain.SpeedButton2Click(Sender: TObject);
begin

  FloatAnimationCameraY.Start;

  FloatAnimationCameraX1.Start;
  FloatAnimationCameraX2.Start;
  FloatAnimationCameraX3.Start;


end;

procedure TFrmMain.CleanFrameTextureClick(Sender: TObject);
var
  Obj: TExtrudedShape3D;
  List: TListBox;
  Sample: TImage;
  TempMaterialSource: TLightMaterialSource;
begin

  Obj := nil;
  List := nil;
  Sample := nil;

  if Sender = CleanTextTexture then
  begin
    Obj := Text3D;
    List := ListText;
    Sample := TextTexture;
  end
  else
  begin
    if Sender = CleanFrameTexture then
    begin
      Obj := Rectangle;
      List := ListFrame;
      Sample := FrameTexture;
    end;
  end;

  if Assigned(Obj) and Assigned(List) then
  begin
    TempMaterialSource :=   GetMaterialSource(List, Obj);
    TempMaterialSource.Texture.SetSize(0, 0);
    Sample.Bitmap.SetSize(0, 0);
  end;

end;

procedure TFrmMain.SBDownClick(Sender: TObject);
begin
  MainDummy.RotationAngle.X := MainDummy.RotationAngle.X + 1;
end;

procedure TFrmMain.SBFontClick(Sender: TObject);
begin
  // not implemented yet
end;

procedure TFrmMain.SBLeftClick(Sender: TObject);
begin
  MainDummy.RotationAngle.Y := MainDummy.RotationAngle.Y + 1;

end;

procedure TFrmMain.SBRightClick(Sender: TObject);
begin
  MainDummy.RotationAngle.Y := MainDummy.RotationAngle.Y - 1;

end;

procedure TFrmMain.SBUpClick(Sender: TObject);
begin
  MainDummy.RotationAngle.X := MainDummy.RotationAngle.X - 1;
end;

procedure TFrmMain.Text3DClick(Sender: TObject);
begin
  SelectedObject := Sender as TShape3D;
end;

procedure TFrmMain.Text3DDepthChange(Sender: TObject);
begin
  TextDepth := Text3DDepth.Value;
end;

procedure TFrmMain.TrackBarObjectDepthChange(Sender: TObject);
begin
  Camera.Position.Z := - TrackBarObjectDepth.Value;
end;

procedure TFrmMain.VertMarginChange(Sender: TObject);
begin
  FrameHeight := VertMargin.Value;
end;

procedure TFrmMain.Viewport3D1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if not isAnimationRunning then
  begin
    Down := PointF(X, Y);
    MouseUp := True
  end;
end;

procedure TFrmMain.Viewport3D1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Single);
begin
  if (ssLeft in Shift) and (MouseUp) and (not isAnimationRunning) then
  begin
    DummyX.RotationAngle.X := DummyX.RotationAngle.X - ((Y - Down.Y) * 0.3);
    DummyY.RotationAngle.Y := DummyY.RotationAngle.Y + ((X - Down.X) * 0.3);

    Down := PointF(X, Y);
  end;
end;

procedure TFrmMain.Viewport3D1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  MouseUp := False;
end;

procedure TFrmMain.Viewport3D1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
var
  AVector: TVector3D;
begin
  if not isAnimationRunning then
  begin
    AVector := Vector3D(0, 0, 1);
    Camera.Position.Vector := Camera.Position.Vector + AVector * (WheelDelta / 120) * 0.3;
    TrackBarObjectDepth.Value := -Camera.Position.Z;
  end;
end;

end.
