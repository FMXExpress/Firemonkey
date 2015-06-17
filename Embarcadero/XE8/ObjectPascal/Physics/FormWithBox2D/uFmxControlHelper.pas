
//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit uFmxControlHelper;

interface

uses
  FMX.Controls;

type
  TFmxControlHelper = class helper for TControl
  public
    procedure SetRotation(const Value: Single);
  end;

implementation

{ TFmxControlHelper }

procedure TFmxControlHelper.SetRotation(const Value: Single);
begin
  self.RotationAngle := Value;
end;

end.
