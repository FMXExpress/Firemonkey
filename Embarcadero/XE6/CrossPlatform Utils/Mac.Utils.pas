unit Mac.Utils;

interface

uses
  Macapi.Foundation, Macapi.CocoaTypes;

function NSStringToString(APtr: Pointer): string; overload;
function NSStringToString(AStr: NSString): string; overload;

function NSDateToDateTime(APtr: Pointer): TDateTime; overload;
function NSDateToDateTime(ADate: NSDate): TDateTime; overload;

function NSNumberToInt(ANumber: NSNumber): Integer; overload;
function NSNumberToInt(APtr: Pointer): Integer; overload;
function NSNumberToLongInt(ANumber: NSNumber): LongInt; overload;
function NSNumberToLongInt(APtr: Pointer): LongInt; overload;
function NSNumberToDouble(ANumber: NSNumber): Double; overload;
function NSNumberToDouble(APtr: Pointer): Double; overload;

function NSObjectToString(AObject: NSObject): string; overload;
function NSObjectToString(APtr: Pointer): string; overload;

function DateTimeToNSDate(ADateTime: TDateTime): NSDate;
function NSStrPtr(AString: string): Pointer;
function PtrForObject(AObject: NSObject): Pointer;
{$ENDIF}

implementation

{$IFDEF MACOS}
uses
  Macapi.ObjectiveC, System.SysUtils;

function NSStringToString(APtr: Pointer): string;
begin
  //TODO: Rewrite using routines from
  //http://delphihaven.wordpress.com/2011/09/26/converting-from-a-cocoa-string-to-a-delphi-string/
  Result := NSStringToString(TNSString.Wrap(APtr));
end;

function NSStringToString(AStr: NSString): string;
begin
  //TODO: Rewrite using routines from
  //http://delphihaven.wordpress.com/2011/09/26/converting-from-a-cocoa-string-to-a-delphi-string/
  Result := string(AStr.UTF8String);
end;

function NSStrPtr(AString: string): Pointer;
begin
  Result := TNSString.OCClass.stringWithUTF8String(PAnsiChar(UTF8String(AString)));
end;

function PtrForObject(AObject: NSObject): Pointer;
begin
  Result := (AObject as ILocalObject).GetObjectID;
end;

function DateTimeToNSDate(ADateTime: TDateTime): NSDate;
const
  cnDateFmt = '%.4d-%.2d-%.2d %.2d:%.2d:%.2d +0000';
var
  Day, Month, Year, Hour, Min, Sec, MSec: Word;
  DateStr: string;
  Tmp: NSDate;
begin
  DecodeDate(ADateTime, Year, Month, Day);
  DecodeTime(ADateTime, Hour, Min, Sec, MSec);
  DateStr := Format(cnDateFmt, [Year, Month, Day, Hour, Min, Sec, 0]);
  Tmp := TNSDate.Create;
  try
    Tmp.initWithString(NSStr(DateStr));
    Result := TNSDate.Wrap(Tmp.addTimeInterval(MSec / 1000));
    Result.retain;
  finally
    Tmp.release;
  end;
end;

function NSDateToDateTime(APtr: Pointer): TDateTime;
begin
  Result := NSDateToDateTime(TNSDate.Wrap(APtr));
end;

function NSDateToDateTime(ADate: NSDate): TDateTime;
var
  lCalendar: NSCalendar;
  lComps: NSDateComponents;
  lUnits: Cardinal;
begin
  lCalendar := TNSCalendar.Wrap(TNSCalendar.OCClass.currentCalendar);
  lUnits := NSYearCalendarUnit or NSMonthCalendarUnit or NSDayCalendarUnit or
    NSHourCalendarUnit or NSMinuteCalendarUnit or NSSecondCalendarUnit;
  lComps := lCalendar.components(lUnits, ADate);

  Result := EncodeDate(lComps.year, lComps.month, lComps.day) +
    EncodeTime(lComps.hour, lComps.minute, lComps.second, 0);
end;

function NSObjectToString(AObject: NSObject): string;
var
  lNum: NSNumber;
  lStr: NSString;
  lDate: NSDate;
begin
  if True then
    Result := NSStringToString(NSString(AObject))
  else if Supports(AObject, NSString, lStr) then
    Result := NSStringToString(lStr)
  else if Supports(AObject, NSNumber, lNum) then
    Result := IntToStr(NSNumberToInt(lNum))
  else if Supports(AObject, NSDate, lDate) then
    Result := DateTimeToStr(NSDateToDateTime(lDate))
  else
    Result := '';
end;

function NSObjectToString(APtr: Pointer): string;
begin
  Result := NSObjectToString(TNSObject.Wrap(APtr));
end;

function NSNumberToInt(ANumber: NSNumber): Integer;
begin
  Result := ANumber.integerValue;
end;

function NSNumberToInt(APtr: Pointer): Integer;
begin
  Result := NSNumberToInt(TNSNumber.Wrap(APtr));
end;

function NSNumberToLongInt(ANumber: NSNumber): LongInt;
begin
  Result := ANumber.longLongValue;
end;

function NSNumberToLongInt(APtr: Pointer): LongInt;
begin
  Result := NSNumberToLongInt(TNSNumber.Wrap(APtr));
end;

function NSNumberToDouble(ANumber: NSNumber): Double;
begin
  Result := ANumber.doubleValue;
end;

function NSNumberToDouble(APtr: Pointer): Double;
begin
  Result := NSNumberToDouble(TNSNumber.Wrap(APtr));
end;



end.
