
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit uOSVersionUtils;

interface

uses
  System.SysUtils;

function OSArchitectureToStr(const a: TOSVersion.TArchitecture): string;
function OSPlatformToStr(const p: TOSVersion.TPlatform): string;

implementation

function OSArchitectureToStr(const a: TOSVersion.TArchitecture): string;
begin
  case a of
    arIntelX86:
      Result := 'IntelX86';

    arIntelX64:
      Result := 'IntelX64';

    else
      Result := 'UNKNOWN OS architecture';
  end;
end;

function OSPlatformToStr(const p: TOSVersion.TPlatform): string;
begin
  case p of
    pfWindows:
      Result := 'Windows';

    pfMacOS:
      Result := 'MacOS';

    else
      Result := 'UNKNOWN OS Platform';
  end;
end;

end.
