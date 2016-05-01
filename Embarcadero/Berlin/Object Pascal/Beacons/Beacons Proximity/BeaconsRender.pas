//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit BeaconsRender;

interface

uses
  System.Types, System.UITypes, System.Generics.Collections, System.Math,
  FMX.Types, FMX.Graphics, FMX.Objects, FMX.TextLayout;

type
  TBeaconGraphicInfo = record
    FName: string;
    FColor: TAlphaColor;
    FOriginalColor: TAlphaColor;
    FDistance: Double;
  end;

  TRenderer = class
  private
    FFillBrush: TBrush;
    FStrokeBrush: TStrokeBrush;
    FReferencePathData, FPathData: TPathData;
    FList: TList<TBeaconGraphicInfo>;
    FMaxDistance: Double;
    FTextLayout: TTextLayout;

    procedure GetExtrems(const ARect: TRectF; var AUnit: Single; var AOrigin, ADestination: TPointF);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Render(MaxDistance: Double; const AList: TList<TBeaconGraphicInfo>; Sender: TRectangle;
      Canvas: TCanvas; const ARect: TRectF);
    function GetObjectUnderMouse(const ARectangle: TRectangle; const APoint: TPointF): string;
  end;

implementation

uses
  System.SysUtils, System.Math.Vectors;

const
  MarkScaleX = 5;
  MarkScaleY = 13;

{ TRenderer }

constructor TRenderer.Create;
begin
  inherited;
  FFillBrush := TBrush.Create(TBrushKind.Solid, TAlphaColorRec.Red);
  FStrokeBrush := TStrokeBrush.Create(TBrushKind.Solid, TAlphaColorRec.Red);

  FReferencePathData := TPathData.Create;
  FReferencePathData.MoveTo(TPointF.Create(0, 0));
  FReferencePathData.LineTo(TPointF.Create(1, 1));
  FReferencePathData.LineTo(TPointF.Create(1, 2));
  FReferencePathData.LineTo(TPointF.Create(-1, 2));
  FReferencePathData.LineTo(TPointF.Create(-1, 1));
  FReferencePathData.ClosePath;

  FPathData := TPathData.Create;

  FList := TList<TBeaconGraphicInfo>.Create;
  FMaxDistance := 100;

  FTextLayout := TTextLayoutManager.DefaultTextLayout.Create;;
end;

destructor TRenderer.Destroy;
begin
  FFillBrush.Free;
  FStrokeBrush.Free;
  FTextLayout.Free;
  FReferencePathData.Free;
  FPathData.Free;
  FList.Free;
  inherited;
end;

procedure TRenderer.GetExtrems(const ARect: TRectF; var AUnit: Single; var AOrigin, ADestination: TPointF);
begin
//  AUnit := Min(ARect.Width, ARect.Height) * 0.01;
  AUnit := ARect.Width * 0.003;

  AOrigin.X := 10 * AUnit;
  AOrigin.Y := (ARect.Top + ARect.Bottom) * 0.5;

  ADestination.X := ARect.Right - AOrigin.X;
  ADestination.Y := AOrigin.Y;
end;

function TRenderer.GetObjectUnderMouse(const ARectangle: TRectangle; const APoint: TPointF): string;
var
  LUnit: Single;
  LOrigin, LDestination: TPointF;
  LVector, LAux: TPointF;
  LTouchRect: TRectF;
  I: Integer;
begin
  Result := '';

  GetExtrems(ARectangle.LocalRect, LUnit, LOrigin, LDestination);

  LVector := LDestination - LOrigin;

  for I := 0 to FList.Count - 1 do
  begin
    LAux := LOrigin + LVector * (FList.List[I].FDistance / FMaxDistance);
    LTouchRect.Left := LAux.X - (LUnit * MarkScaleX);
    LTouchRect.Right := LAux.X + (LUnit * MarkScaleX);
    LTouchRect.Top := LAux.Y;
    LTouchRect.Bottom := LAux.Y + (LUnit * MarkScaleY * 2);

    if LTouchRect.Contains(APoint) then
      Exit(FList.List[I].FName);
  end;

end;


procedure TRenderer.Render(MaxDistance: Double; const AList: TList<TBeaconGraphicInfo>; Sender: TRectangle;
  Canvas: TCanvas; const ARect: TRectF);
var
  LOrigin, LDestination: TPointF;
  LCircleOffset, LVector, LAux: TPointF;
  LUnit: Single;
  LIterationCount: Integer;
  I: Integer;
  LState: TCanvasSaveState;
  LMatrix: TMatrix;
begin
  LState := Canvas.SaveState;

  FMaxDistance := MaxDistance;

  GetExtrems(ARect, LUnit, LOrigin, LDestination);

  LVector := LDestination - LOrigin;

  LCircleOffset := TPointF.Create(LUnit * 5, LUnit * 5);

  FStrokeBrush.Thickness := LUnit * 2;
  Canvas.DrawLine(LOrigin, LDestination, 1.0, FStrokeBrush);

  FStrokeBrush.Thickness := 1;
  LIterationCount := 10;
  for I := 0 to LIterationCount - 1 do
  begin
    LAux := LOrigin + LVector * ((I + 1) / LIterationCount);
    Canvas.DrawLine(LAux + TPointF.Create(0, LUnit * 4), LAux - TPointF.Create(0, LUnit * 4), 1.0, FStrokeBrush);
  end;

  FFillBrush.Color := TAlphaColorRec.Red;
  Canvas.FillEllipse(TRectF.Create(LOrigin - LCircleOffset, LOrigin + LCircleOffset), 1.0, FFillBrush);
  Canvas.FillEllipse(TRectF.Create(LDestination - LCircleOffset, LDestination + LCircleOffset), 1.0, FFillBrush);

  if AList <> nil then
  begin
    FList.Count := AList.Count;
    for I := 0 to FList.Count - 1 do
      FList[I] := AList.List[I];
  end;

  for I := 0 to FList.Count - 1 do
  begin
    LAux := LOrigin + LVector * (FList.List[I].FDistance / FMaxDistance);
    FPathData.Assign(FReferencePathData);
    FPathData.Scale(LUnit * MarkScaleX, LUnit * MarkScaleY);
    FPathData.Translate(LAux);
    FFillBrush.Color := FList.List[I].FColor;
    Canvas.FillPath(FPathData, 0.8, FFillBrush);
  end;

  FTextLayout.Text := FMaxDistance.ToString + ' m.';
  FTextLayout.Font.Size := 20;

  LMatrix := Canvas.Matrix * TMatrix.CreateTranslation(ARect.Right - LUnit * 50, 0);
  Canvas.SetMatrix(LMatrix);
  FTextLayout.RenderLayout(Canvas);

  Canvas.RestoreState(LState);
end;

end.
