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
  System.SysUtils, System.Types, System.Classes, System.Variants, FMX.Types,
  FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Ani, FMX.Layers3D, FMX.Types3D,
  System.UITypes, FMX.Objects, FMX.Layouts, FMX.Filter.Effects, FMX.Effects, System.IOUtils,
  FMX.Colors, System.UIConsts, FMX.Edit, FMX.Platform, FMX.StdCtrls,
  FMX.Controls3D, FMX.Viewport3D, FMX.Graphics, System.Math.Vectors;

type
  TFrmMain = class(TForm)
    Viewport3D1: TViewport3D;
    OpenDialog1: TOpenDialog;
    ResourcesDark: TStyleBook;
    Rectangle1: TRectangle;
    RoundRect2: TRoundRect;
    TrackBar1: TTrackBar;
    AniIndicator1: TAniIndicator;
    Button1: TButton;
    CoverFlow: TLayout3D;
    procedure CoverScrollChange(Sender: TObject);
    procedure CoverflowMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
    procedure TrackBar1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure OpenDialog1Show(Sender: TObject);
  private
    CoverIndex: integer;

    procedure AddFolder;
    procedure SetCoverIndex(AIndex: integer);
    procedure DoCoverMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);

  Const
//    Factor = 0.8;
    Factor = 0.5;
    DivFac = 3;
    RotationYAngle = 70;
    Duration = 0.5;
  public
    { Public declarations }
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.fmx}
{$R *.Windows.fmx MSWINDOWS}

type

  TImageThread = class(TThread)
  private
    FImage: TImage;
    FTempBitmap: TBitmap;
    FFileName: string;
  protected
    procedure Execute; override;
    procedure Finished;
  public
    constructor Create(const AImage: TImage; const AFileName: string);
    destructor Destroy; override;
  end;

{ TImageThread }

constructor TImageThread.Create(const AImage: TImage; const AFileName: string);
begin
  inherited Create(true);
  FFileName := AFileName;
  FImage := AImage;
//  Priority := tpIdle;
  FreeOnTerminate := True;
end;

destructor TImageThread.Destroy;
begin
  inherited;
end;

procedure TImageThread.Execute;
begin
//  Sleep(random(300));
  FTempBitmap := TBitmap.Create(0, 0);
  FTempBitmap.LoadThumbnailFromFile(FFileName, FImage.Width, FImage.Height, false);
  Synchronize(Finished);

end;

procedure TImageThread.Finished;
begin
  FImage.Bitmap.Assign(FTempBitmap);
  FTempBitmap.Free;
end;

procedure TFrmMain.AddFolder;
var
  Dir, FileName: String;
  Cover: TLayer3D;
  Layout: TLayout;
  Image: TImage;
  Effect: TReflectionEffect;
  I: integer;
  L : TRectangle;
  FilterPredicate : TDirectory.TFilterPredicate;
begin
  OpenDialog1.Filter := 'All Images|' + TBitmapCodecManager.GetFileTypes;
//    OpenDialog1.FileName := '/Users/andreano/Pictures/Demo Photos/Picture1.JPEG';
  if OpenDialog1.Execute then
  begin
    CoverFlow.DeleteChildren;
    AniIndicator1.Visible := true;
    Dir := ExtractFilePath(OpenDialog1.FileName);
    I := 0;

    FilterPredicate := function(const Path: string; const SearchRec: TSearchRec): Boolean
                       begin
                         Result := (TPath.MatchesPattern(SearchRec.Name, '*.*', False)); // and
//                                 (SearchRec.Attr = faAnyFile);

                         if Result then // Check if it is a image file
                         begin
                           Result := Pos(LowerCase(ExtractFileExt(SearchRec.Name)),
                                     TBitmapCodecManager.GetFileTypes) > 0;
                         end;
                       end;

    for FileName in TDirectory.GetFiles(Dir, FilterPredicate) do
    begin
      // Create Cover
      Cover := TLayer3D.Create(Self);
      Cover.Parent := CoverFlow;

      Cover.Projection := TProjection.Screen;
      Cover.Width := Round(CoverFlow.Height * Factor);
      Cover.Height := Round(Round(CoverFlow.Height * Factor) * 1.5);
      Cover.ZWrite := True;
      Cover.Fill.Color := Viewport3D1.Color;
      Cover.Fill.Kind  := TBrushKind.Solid;
      Cover.Transparency := True;
      Cover.OnLayerMouseDown := DoCoverMouseDown;
      Cover.Tag := I;
      Cover.Padding.Rect := TRectF.Create(0, 0, 0, 0);
      Cover.Position.Y := Trunc((CoverFlow.Height + Round(CoverFlow.Height * Factor)) / 2);
      Cover.Cursor := crHandPoint;

      if I = 0 then
      begin
        Cover.Position.X := CoverFlow.Width/2;
      end
      else
      begin
        Cover.Position.X := (I + 1) * (Round(CoverFlow.Height * Factor) / DivFac) + CoverFlow.Width/2;
        Cover.Position.Z := Round(CoverFlow.Height * Factor) * 2;
        Cover.RotationAngle.Y := RotationYAngle;
      end;

      // Child
      Layout := TLayout.Create(Self);
      Layout.Parent := Cover;
      Layout.Align := TAlignLayout.Top;
      Layout.Height := Trunc(Cover.Height / 2); // original = 2
      Layout.Padding.Rect := TRectF.Create(0, 0, 0, 0);
      Layout.Cursor := crHandPoint;

      // This rectangle is necessary to avoid blank lines on the image
      L := TRectangle.Create(Self);
      L.Parent := Layout;
      L.Align := TAlignLayout.Top;
      L.Height := Trunc(Cover.Height / 2);
      L.Fill.Kind := TBrushKind.None;
      L.Stroke.Color := Viewport3D1.Color;
      L.Stroke.Kind := TBrushKind.None;

      Image := TImage.Create(Self);
      Image.Parent := Layout;
      Image.Padding.Rect := TRectF.Create(0, 0, 0, 0);
      Image.TagString := FileName;

      with TBitmapCodecManager.GetImageSize(Image.TagString) do
      begin
        Image.Width := X;
        Image.Height := Y;
      end;

      Image.WrapMode := TImageWrapMode.Stretch;
      Image.Align := TAlignLayout.Fit;
      Image.HitTest := true;
      Image.TagString := FileName;
      Image.Cursor := crHandPoint;
      TImageThread.Create(Image, Image.TagString).Start;
      Image.OnMouseDown := DoCoverMouseDown;
//      Image.OnMouseWheel := CoverflowMouseWheel;

      Image.Tag := I;

      Effect := TReflectionEffect.Create(Self);
      Effect.Parent := Image;
      Effect.Opacity := 0.6;

      // Opacity animation
      Cover.Opacity := 0.01;
      Cover.AnimateFloat('Opacity', 1, Duration);

      // Load thumb
      Cover.TagObject := Image;

      Inc(I);

      Application.ProcessMessages;
    end;

    CoverIndex := 0;
    AniIndicator1.Visible := False;
    TrackBar1.Max := Coverflow.ChildrenCount-1;
    TrackBar1.Value := 0;
    TrackBar1.Visible := True;
    TrackBar1.SetFocus;
  end;
end;

procedure TFrmMain.Button1Click(Sender: TObject);
begin
  AddFolder;
end;

procedure TFrmMain.CoverflowMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin

  TrackBar1.Value := CoverIndex - (WheelDelta div 120);
  Handled := true;

end;

procedure TFrmMain.CoverScrollChange(Sender: TObject);
begin
  SetCoverIndex(Round(TrackBar1.Value));
end;

procedure TFrmMain.SetCoverIndex(AIndex: integer);
var
  i : integer;
  Cover: TLayer3D;
  PercCoeff, Coeff: single;
begin
  if AniIndicator1.Visible or (CoverFlow.ChildrenCount = 0) then
  begin
     TrackBar1.Value := CoverIndex;
     Abort;
  end;

  PercCoeff := 0.6;


  if AIndex < 0 then AIndex := 0;
  if AIndex >= CoverFlow.ChildrenCount then AIndex := CoverFlow.ChildrenCount - 1;
  if AIndex <> CoverIndex then
  begin
    { translate all }  //move the pictures in the back
    for i := 0 to CoverFlow.ChildrenCount - 1 do
    begin

      Cover := TLayer3D(CoverFlow.Children[i]);
      Cover.StopPropertyAnimation('Position.X');
      Cover.AnimateFloat('Position.X', Cover.Position.X + ((CoverIndex - AIndex) * (Round(CoverFlow.Height * Factor) div DivFac)), Duration);
    end;

    { transform between old an new value }
    i := CoverIndex;
    while i <> AIndex do
    begin
//      Coeff := (0.1 + (Abs(AIndex - i) / Abs(AIndex - CoverIndex))) * (PercCoeff + 0.1);
      Coeff := (0.1 + (Abs(AIndex - i) / Abs(AIndex - CoverIndex))) * (PercCoeff + 0.1);

      Cover := TLayer3D(CoverFlow.Children[i]);
      Cover.StopPropertyAnimation('Position.X');
      Cover.StopPropertyAnimation('RotationAngle.Y');

      if CoverIndex > AIndex then
      begin
        Cover.AnimateFloat('RotationAngle.Y', RotationYAngle, Duration );
        if i = CoverIndex then
          Cover.AnimateFloat('Position.X', Cover.Position.X + (1 * (Round(CoverFlow.Height * Factor) div DivFac)), Duration * Coeff)
        else
          Cover.AnimateFloat('Position.X', Cover.Position.X + (2 * (Round(CoverFlow.Height * Factor) div DivFac)), Duration * Coeff);
      end
      else
      begin
        Cover.AnimateFloat('RotationAngle.Y', RotationYAngle*-1, Duration );
        if i = CoverIndex then
          Cover.AnimateFloat('Position.X', Cover.Position.X - (1 * (Round(CoverFlow.Height * Factor) div DivFac)), Duration * Coeff)
        else
          Cover.AnimateFloat('Position.X', Cover.Position.X - (2 * (Round(CoverFlow.Height * Factor) div DivFac)), Duration * Coeff);
      end;
      Cover.AnimateFloat('Position.Z', Round(CoverFlow.Height * Factor) * 2, Duration);
      if AIndex > CoverIndex then
        Inc(i)
      else
        Dec(i);
    end;

    Cover := TLayer3D(CoverFlow.Children[AIndex]);

    Cover.StopPropertyAnimation('Position.X');
    Cover.StopPropertyAnimation('Position.Z');

    Cover.AnimateFloat('RotationAngle.Y', 0, Duration);
    Cover.AnimateFloat('Position.Z', 0, Duration);
    if CoverIndex > AIndex then
       Cover.AnimateFloat('Position.X', Cover.Position.X + (1 * (Round(CoverFlow.Height * Factor) div DivFac)), Duration)
    else
      Cover.AnimateFloat('Position.X', Cover.Position.X - (1 * (Round(CoverFlow.Height * Factor) div DivFac)), Duration);

    CoverIndex := AIndex;

  end;
end;

procedure TFrmMain.TrackBar1Change(Sender: TObject);
begin
  SetCoverIndex(Round(TrackBar1.Value));
end;

procedure TFrmMain.DoCoverMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  TrackBar1.Value := Round(StrToFloat(IntToStr(TImage(Sender).Tag)));
end;

procedure TFrmMain.OpenDialog1Show(Sender: TObject);
begin
{$ifdef MACOS}
  TOpenDialog(Sender).InitialDir := System.SysUtils.ExpandFileName( '../../..');
{$else}
  TOpenDialog(Sender).InitialDir := ExtractFilePath( ParamStr(0)) ;
{$endif}
  TOpenDialog(Sender).FileName := '*.jpg';
end;

end.
