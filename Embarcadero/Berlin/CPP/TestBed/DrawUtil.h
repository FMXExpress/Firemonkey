//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef DrawUtilH
#define DrawUtilH

#include <Box2D/Box2D.h>
#include <FMX.Graphics.hpp>


inline TColor b2Color2TColor(const b2Color& color)
{
  TAlphaColorRec res;
  res.R = 255 * color.r;
  res.G = 255 * color.g;
  res.B = 255 * color.b;
  res.A = 255 * color.a;
  return *(reinterpret_cast<TColor*>(&res.Color));
}

#endif
