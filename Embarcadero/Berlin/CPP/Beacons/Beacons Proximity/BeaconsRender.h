//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef BeaconsRenderH
#define BeaconsRenderH
//---------------------------------------------------------------------------

#include<System.Types.hpp>
#include<System.UITypes.hpp>
#include<System.Generics.Collections.hpp>
#include<System.Math.hpp>
#include<FMX.Types.hpp>
#include<FMX.Graphics.hpp>
#include<FMX.Objects.hpp>
#include<FMX.TextLayout.hpp>

typedef struct
{
	String FName;
	unsigned FColor;
	unsigned FOriginalColor;
	Double FDistance;
} TBeaconGraphicInfo;

#define TBeaconGraphicList System::DynamicArray<TBeaconGraphicInfo>

class TRenderer
{
  private:
	TBrush *FFillBrush;
	TStrokeBrush *FStrokeBrush;
	TPathData *FReferencePathData;
	TPathData *FPathData;
	TBeaconGraphicList FList;
	Double FMaxDistance;
	TTextLayout *FTextLayout;
	void __fastcall GetExtrems(const TRectF ARect, Single &AUnit, TPointF &AOrigin, TPointF &ADestination);
	TPointF __fastcall CreatePoint(Single AX, Single AY);
	TRectF __fastcall CreateRectF(Single Left, Single Top, Single Right, Single Bottom);
	TMatrix __fastcall MultiplyMatrix(TMatrix AMatrix1, TMatrix AMatrix2);
  public:
	__fastcall TRenderer();
	__fastcall ~TRenderer();
	void __fastcall Render(Double MaxDistance, TBeaconGraphicList AList,TRectangle *Sender, TCanvas *Canvas, const TRectF ARect);
	String __fastcall GetObjectUnderMouse(TRectangle *ARectangle, const TPointF APoint);
};

#endif
