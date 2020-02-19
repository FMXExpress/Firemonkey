//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef aniformH
#define aniformH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Forms3D.hpp>
#include <FMX.Ani.hpp>
#include <FMX.Objects.hpp>
#include <FMX.Types.hpp>
#include <FMX.StdCtrls.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TAniIndicator *AniIndicator1;
	TAniIndicator *AniIndicator2;
	TAniIndicator *AniIndicator3;
	TEllipse *Ellipse1;
	TFloatAnimation *FloatAnimation1;
	TText *Text1;
	TImage *Image1;
	TBitmapAnimation *BitmapAnimation1;
	TText *Text4;
	TLabel *Label1;
	TPath *Path1;
	TRectangle *Rectangle1;
	TRectangle *Rectangle2;
	TRectangle *Rectangle3;
	TColorAnimation *ColorAnimation1;
	TText *Text2;
	TFloatAnimation *FloatAnimation2;
	TText *Text3;
	TText *Text5;
	TRoundRect *RoundRect1;
	TPathAnimation *PathAnimation1;
	TText *Text6;
	TText *Text7;
	TPathAnimation *PathAnimation2;
private:	// User declarations
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
