unit uPublic;

interface

//{$DEFINE FMX}

uses
//  Windows,
  SysUtils,

  uFileCommon,
  uFuncCommon,
//  {$IFDEF VCL}
//  Windows,
//  Forms,
//  {$ENDIF}
//
//  {$IFDEF FMX}
//  IOUtils,
//  FMX.Forms,
//  {$ENDIF}

  Classes;


//function StandardStrToDateTime(Const Str:String):TDateTime;
//function StandardDateTimeToStr(Const ADateTime:TDateTime):String;

//function UrlEncode(Url: string): string;
//function FuncUrlEncode(const S : String) : String;
//function UrlEncodeUTF8(Url: UTF8String): string;
//function FuncUrlEncodeUTF8(const S : UTF8String) : String;

function GetResponseTempDir:String;




implementation


//var
//  GlobalStandardFmtSettings: TFormatSettings;






//function GetApplicationPath:String;
//begin
//
//  {$IFDEF VCL}
//  Result:=ExtractFilePath(Application.ExeName);
//  {$ENDIF}
//
//  {$IFDEF MSWINDOWS}
//  Result:=TPath.GetLibraryPath;//+PathDelim;
//  {$ENDIF}
//
//  {$IFDEF IOS}
//  Result:=GetHomePath+PathDelim+'Documents'+PathDelim;
//  {$ENDIF}
//
//  {$IFDEF Android}
//  Result:=GetHomePath+PathDelim;
//  {$ENDIF}
//
//end;









function GetResponseTempDir:String;
begin
  Result:=GetApplicationPath+'Response'+PathDelim;
  if Not DirectoryExists(Result) then
  begin
    SysUtils.ForceDirectories(Result);
  end;
end;

//function StandardDateTimeToStr(Const ADateTime:TDateTime):String;
//begin
//  Result:=DateTimeToStr(ADateTime,GlobalStandardFmtSettings);
//end;
//
//function StandardStrToDateTime(Const Str:String):TDateTime;
//begin
//  if Str='' then
//  begin
//    Result:=0;
//  end
//  else
//  begin
//    Result:=StrToDateTime(Str,GlobalStandardFmtSettings);
//  end;
//end;
//
//function _IntToHex(Value: Integer; Digits: Integer): String;
//begin
//  Result := SysUtils.IntToHex(Value, Digits);
//end;
//
//function XDigit(Ch : Char) : Integer;
//begin
//  if (Ch >= '0') and (Ch <= '9') then
//      Result := Ord(Ch) - Ord('0')
//  else
//      Result := (Ord(Ch) and 15) + 9;
//end;
//
//
//function IsXDigit(Ch : Char) : Boolean;
//begin
//  Result := ((Ch >= '0') and (Ch <= '9')) or
//            ((Ch >= 'a') and (Ch <= 'f')) or
//            ((Ch >= 'A') and (Ch <= 'F'));
//end;
//
//function htoin(Value : PChar; Len : Integer) : Integer;
//var
//  I : Integer;
//begin
//  Result := 0;
//  I      := 0;
//  while (I < Len) and (Value[I] = ' ') do
//      I := I + 1;
//  while (I < len) and (IsXDigit(Value[I])) do begin
//      Result := Result * 16 + XDigit(Value[I]);
//      I := I + 1;
//  end;
//end;
//
//function htoi2(Value : PChar) : Integer;
//begin
//  Result := htoin(Value, 2);
//end;
//
//function FuncUrlEncode(const S : String) : String;
//var
//  I : Integer;
//  Ch : Char;
//begin
//  Result := '';
//  for I := 1 to Length(S) do begin
//      Ch := S[I];
//      if ((Ch >= '0') and (Ch <= '9')) or
//         ((Ch >= 'a') and (Ch <= 'z')) or
//         ((Ch >= 'A') and (Ch <= 'Z')) or
//         (Ch = '.') or (Ch = '-') or (Ch = '_') or (Ch = '~')then
//          Result := Result + Ch
//      else
//          Result := Result + '%' + _IntToHex(Ord(Ch), 2);
//  end;
//end;
//
//function FuncUrlEncodeUTF8(const S : UTF8String) : String;
//var
//  I : Integer;
//  Ch : AnsiChar;
//begin
//  Result := '';
//  for I := 1 to Length(S) do begin
//      Ch := S[I];
//      if ((Ch >= '0') and (Ch <= '9')) or
//         ((Ch >= 'a') and (Ch <= 'z')) or
//         ((Ch >= 'A') and (Ch <= 'Z')) or
//         (Ch = '.') or (Ch = '-') or (Ch = '_') or (Ch = '~')then
//          Result := Result + Ch
//      else
//          Result := Result + '%' + _IntToHex(Ord(Ch), 2);
//  end;
//end;
//
//function UrlEncode(Url: string): string;
//var
//  Url1: string;
//begin
//  Url1 := FuncUrlEncode(Url);
//  //Url1 := StringReplace(Url1, '+', ' ', [rfReplaceAll, rfIgnoreCase]);
//  result := Url1;
//end;
//
//function UrlEncodeUTF8(Url: UTF8String): string;
//var
//  Url1: string;
//begin
//  Url1 := FuncUrlEncodeUTF8(Url);
//  //Url1 := StringReplace(Url1, '+', ' ', [rfReplaceAll, rfIgnoreCase]);
//  result := Url1;
//end;
//
//
//
//
//initialization
//  GlobalStandardFmtSettings.ShortDateFormat:='yyyy-MM-dd';
//  GlobalStandardFmtSettings.DateSeparator:='-';
//  GlobalStandardFmtSettings.LongTimeFormat:='hh:mm:ss';
//  GlobalStandardFmtSettings.TimeSeparator:=':';



end.
