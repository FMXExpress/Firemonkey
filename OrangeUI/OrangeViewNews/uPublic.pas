unit uPublic;

interface

uses
  SysUtils,
  uFuncCommon,
  uFileCommon;


function StandardStrToDateTime(Const Str:String):TDateTime;
function StandardDateTimeToStr(Const ADateTime:TDateTime):String;

function GetResponseTempDir:String;


implementation


var
  GlobalStandardFmtSettings: TFormatSettings;

function GetResponseTempDir:String;
begin
  Result:=GetApplicationPath+'Response'+PathDelim;
  if Not DirectoryExists(Result) then
  begin
    SysUtils.ForceDirectories(Result);
  end;
end;

function StandardDateTimeToStr(Const ADateTime:TDateTime):String;
begin
  Result:=DateTimeToStr(ADateTime,GlobalStandardFmtSettings);
end;

function StandardStrToDateTime(Const Str:String):TDateTime;
begin
  if Str='' then
  begin
    Result:=0;
  end
  else
  begin
    Result:=StrToDateTime(Str,GlobalStandardFmtSettings);
  end;
end;



initialization
  GlobalStandardFmtSettings.ShortDateFormat:='yyyy-MM-dd';
  GlobalStandardFmtSettings.DateSeparator:='-';
  GlobalStandardFmtSettings.LongTimeFormat:='hh:mm:ss';
  GlobalStandardFmtSettings.TimeSeparator:=':';



end.
