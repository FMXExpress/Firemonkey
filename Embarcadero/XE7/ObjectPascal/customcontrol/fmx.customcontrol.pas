
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit fmx.customcontrol;

interface

uses
  System.Types, System.SysUtils, System.Classes, FMX.Types, FMX.Styles, FMX.Controls;

type
  TMyControl = class(TStyledControl)
  protected
    function GetStyleObject: TFmxObject; override;
  public
  end;

procedure Register;

implementation

{$IFDEF MACOS}
{$R *.mac.res}
{$ENDIF}
{$IFDEF MSWINDOWS}
{$R *.win.res}
{$ENDIF}

procedure Register;
begin
  RegisterComponents('Samples', [TMyControl]);
end;

{ TMyControl }

function TMyControl.GetStyleObject: TFmxObject;
begin
  if (StyleLookup = '') then
    Result := TStyleStreaming.LoadFromResource(HInstance, 'mycontrolstyle', RT_RCDATA)
  else
    Result := inherited GetStyleObject;
end;

end.
