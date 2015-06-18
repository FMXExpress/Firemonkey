unit BSysUtils;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$H+}

interface

uses
  Classes, SysUtils;

{
function StringToHexString(aData: string): string;

function HexStringToString(aData: string): string;
}

implementation

{
function StringToHexString(aData: string): string;
var tmp: string;
    i: integer;
begin
  tmp := '';
  for i := 1 to Length(aData) do tmp := tmp + IntToHex(ord(aData[i]), 2);
  result := tmp;
end;

function HexStringToString(aData: string): string;
var tmp: string;
    i: integer;
begin
  tmp := '';
  for i := 1 to (Length(aData)+1) div 2 do tmp := tmp + Chr(StrToInt('$' + Copy(aData, i * 2 - 1, 2)));
  result := tmp;
end;
}
end.

