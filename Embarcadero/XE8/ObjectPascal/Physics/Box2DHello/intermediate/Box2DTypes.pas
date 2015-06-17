
//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit Box2DTypes;

interface

uses System.Types;

type

{$IF DEFINED(ANDROID)}
  tdCharPtr = MarshaledAString;
{$ELSEIF DEFINED(IOS)}
  tdCharPtr = MarshaledAString;
{$ELSE}
  tdCharPtr = PAnsiChar;
{$ENDIF}


implementation

end.
