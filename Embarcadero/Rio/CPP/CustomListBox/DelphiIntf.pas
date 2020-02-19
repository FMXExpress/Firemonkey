//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit DelphiIntf;

interface

uses Fmx.Types, System.Classes, FMX.Graphics, System.Rtti;

function GetFmxCodecInstance: TBitmapCodecManager;
function NotifyEventAsTValue(Ev: TNotifyEvent): TValue;

implementation

function GetFmxCodecInstance: TBitmapCodecManager;
begin
  Result := TBitmapCodecManager.Create  ;
end;

function NotifyEventAsTValue(Ev: TNotifyEvent): TValue;
begin
  Result := TValue.From<TNotifyEvent>(Ev);
end;

end.
