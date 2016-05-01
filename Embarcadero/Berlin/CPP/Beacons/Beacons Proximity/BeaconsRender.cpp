//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#pragma hdrstop

#include "BeaconsRender.h"
#include <System.SysUtils.hpp>
#include <System.Math.Vectors.hpp>
//---------------------------------------------------------------------------

#define MarkScaleX 5
#define MarkScaleY 13

void __fastcall TRenderer::GetExtrems(const TRectF ARect, Single &AUnit, TPointF &AOrigin, TPointF &ADestination)
{
  AUnit = ARect.Width() * 0.003f;

  AOrigin.X = 10 * AUnit;
  AOrigin.Y = (ARect.Top + ARect.Bottom) * 0.5;

  ADestination.X = ARect.Right - AOrigin.X;
  ADestination.Y = AOrigin.Y;
};


TPointF __fastcall TRenderer::CreatePoint(Single AX, Single AY)
{
  TPointF Result;
  Result.X = AX;
  Result.Y = AY;
  return (Result);
};

TRectF __fastcall TRenderer::CreateRectF(Single Left, Single Top, Single Right, Single Bottom)
{
  TRectF Result;
  Result.Left = Left;
  Result.Top = Top;
  Result.Right = Right;
  Result.Bottom = Bottom;
  return(Result);
}

TMatrix __fastcall TRenderer::MultiplyMatrix(TMatrix AMatrix1, TMatrix AMatrix2)
{
  TMatrix Result;
  Result.m11 = AMatrix1.m11 * AMatrix2.m11 + AMatrix1.m12 * AMatrix2.m21 + AMatrix1.m13 * AMatrix2.m31;
  Result.m12 = AMatrix1.m11 * AMatrix2.m12 + AMatrix1.m12 * AMatrix2.m22 + AMatrix1.m13 * AMatrix2.m32;
  Result.m13 = AMatrix1.m11 * AMatrix2.m13 + AMatrix1.m12 * AMatrix2.m23 + AMatrix1.m13 * AMatrix2.m33;
  Result.m21 = AMatrix1.m21 * AMatrix2.m11 + AMatrix1.m22 * AMatrix2.m21 + AMatrix1.m23 * AMatrix2.m31;
  Result.m22 = AMatrix1.m21 * AMatrix2.m12 + AMatrix1.m22 * AMatrix2.m22 + AMatrix1.m23 * AMatrix2.m32;
  Result.m23 = AMatrix1.m21 * AMatrix2.m13 + AMatrix1.m22 * AMatrix2.m23 + AMatrix1.m23 * AMatrix2.m33;
  Result.m31 = AMatrix1.m31 * AMatrix2.m11 + AMatrix1.m32 * AMatrix2.m21 + AMatrix1.m33 * AMatrix2.m31;
  Result.m32 = AMatrix1.m31 * AMatrix2.m12 + AMatrix1.m32 * AMatrix2.m22 + AMatrix1.m33 * AMatrix2.m32;
  Result.m33 = AMatrix1.m31 * AMatrix2.m13 + AMatrix1.m32 * AMatrix2.m23 + AMatrix1.m33 * AMatrix2.m33;
  return(Result);
}


__fastcall TRenderer::TRenderer()
{
  FFillBrush = new TBrush(TBrushKind::Solid, (TAlphaColor)TAlphaColorRec::Red);
  FStrokeBrush =  new TStrokeBrush(TBrushKind::Solid, (TAlphaColor)TAlphaColorRec::Red);

  FReferencePathData = new TPathData;

  FReferencePathData->MoveTo(CreatePoint(0, 0));
  FReferencePathData->LineTo(CreatePoint(1, 1));
  FReferencePathData->LineTo(CreatePoint(1, 2));
  FReferencePathData->LineTo(CreatePoint(-1, 2));
  FReferencePathData->LineTo(CreatePoint(-1, 1));
  FReferencePathData->ClosePath();

  FPathData = new TPathData;

  FMaxDistance = 100;
};

__fastcall TRenderer::~TRenderer()
{
  FFillBrush->Free();
  FStrokeBrush->Free();
  FReferencePathData->Free();
  FPathData->Free();
  FList.Length = 0;
};

void __fastcall TRenderer::Render(Double MaxDistance, TBeaconGraphicList AList,TRectangle *Sender, TCanvas *Canvas, const TRectF ARect)
{
  TPointF LOrigin;
  TPointF LDestination;
  TPointF LCircleOffset;
  TPointF LVector;
  TPointF LAux;
  Single LUnit;
  Integer LIterationCount;
  int I;
  TCanvasSaveState *LState;
  TMatrix LMatrix;

  LState = Canvas->SaveState();

  FMaxDistance = MaxDistance;

  GetExtrems(ARect, LUnit, LOrigin, LDestination);

  LVector = LDestination - LOrigin;

  LCircleOffset = CreatePoint(LUnit * 5, LUnit * 5);

  FStrokeBrush->Thickness = LUnit * 2;
  Canvas->DrawLine(LOrigin, LDestination, 1.0, FStrokeBrush);

  FStrokeBrush->Thickness = 1;
  LIterationCount = 10;
  for (I = 0; I < LIterationCount; I++)
  {
	LAux.X = LOrigin.X + LVector.X * ((I + 1) / LIterationCount);
	LAux.Y = LOrigin.Y + LVector.Y * ((I + 1) / LIterationCount);
	Canvas->DrawLine(LAux + CreatePoint(0, LUnit * 4), LAux - CreatePoint(0, LUnit * 4), 1.0, FStrokeBrush);
  };

  FFillBrush->Color = (TAlphaColor)TAlphaColorRec::Red;
  Canvas->FillEllipse(CreateRectF(LOrigin.X - LCircleOffset.X, LOrigin.Y - LCircleOffset.Y,
                     LOrigin.X + LCircleOffset.X, LOrigin.Y + LCircleOffset.Y), 1.0, FFillBrush);
  Canvas->FillEllipse(CreateRectF(LDestination.X - LCircleOffset.X, LDestination.Y - LCircleOffset.Y,
					 LDestination.X + LCircleOffset.X, LDestination.Y + LCircleOffset.Y), 1.0, FFillBrush);

  if (AList.Length > 0)
  {
	FList.Length = AList.Length;
	for (I = 0; I < FList.Length; I++)
	{
	  FList[I] = AList[I];
	}
  };

  for (I = 0; I < FList.Length; I++)
  {
	LAux.X = LOrigin.X + LVector.X * (FList[I].FDistance / FMaxDistance);
	LAux.Y = LOrigin.Y + LVector.Y * (FList[I].FDistance / FMaxDistance);
	FPathData->Assign(FReferencePathData);
	FPathData->Scale(LUnit * MarkScaleX, LUnit * MarkScaleY);
	FPathData->Translate(LAux);
	FFillBrush->Color = (TAlphaColor)FList[I].FColor;
	Canvas->FillPath(FPathData, 0.8, FFillBrush);
  };

  LMatrix = MultiplyMatrix(Canvas->Matrix, TMatrix::CreateTranslation(ARect.Right - LUnit * 50, 0));
  Canvas->SetMatrix(LMatrix);
  Canvas->RestoreState(LState);
};

String __fastcall TRenderer::GetObjectUnderMouse(TRectangle *ARectangle, const TPointF APoint)
{
  Single LUnit;
  TPointF LOrigin;
  TPointF LDestination;
  TPointF LVector;
  TPointF LAux;
  TRectF LTouchRect;
  int I;
  String Result;

  Result = "";

  GetExtrems(ARectangle->LocalRect, LUnit, LOrigin, LDestination);

  LVector = LDestination - LOrigin;

  for (I = 0; I < FList.Length; I++)
  {
	LAux.X = LOrigin.X + LVector.X * (FList[I].FDistance / FMaxDistance);
	LAux.Y = LOrigin.Y + LVector.Y * (FList[I].FDistance / FMaxDistance);
	LTouchRect.Left = LAux.X - (LUnit * MarkScaleX);
	LTouchRect.Right = LAux.X + (LUnit * MarkScaleX);
	LTouchRect.Top = LAux.Y;
	LTouchRect.Bottom = LAux.Y + (LUnit * MarkScaleY * 2);

	if (LTouchRect.Contains(APoint))
	{
	  Result = FList[I].FName;
	  break;
	}
  };
  return(Result);
};

#pragma package(smart_init)
