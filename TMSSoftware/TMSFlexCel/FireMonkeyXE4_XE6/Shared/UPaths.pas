unit UPaths;

interface

function DataFolder: string;
implementation

uses
  System.SysUtils;

function DataFolder: string;
begin
{$IFDEF MSWINDOWS}
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + '..\..\';
{$ELSE}
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
{$ENDIF}
end;
end.
