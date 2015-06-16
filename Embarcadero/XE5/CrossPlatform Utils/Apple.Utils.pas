unit Apple.Utils;

interface

uses
  Macapi.ObjcRuntime,
{$IFDEF IOS}
  iOSapi.Foundation, iOSapi.CocoaTypes, iOSapi.UIKit;
{$ELSE}
{$IFDEF MACOS}
  Macapi.Foundation, Macapi.CocoaTypes, Macapi.AppKit;
{$ENDIF MACOS}
{$ENDIF IOS}

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
function NSNumberToBool(ANumber: NSNumber): Boolean; overload;
function NSNumberToBool(APtr: Pointer): Boolean; overload;

function NSObjectToString(AObject: NSObject): string; overload;
function NSObjectToString(APtr: Pointer): string; overload;

function DateTimeToNSDate(ADateTime: TDateTime): NSDate;
function NSStrPtr(AString: string): Pointer;
function IntToNSNumber(ANumber: Integer): NSNumber;

function NSNumberPtr(ANumber: Integer): Pointer; overload;
function NSNumberPtr(ANumber: Double): Pointer; overload;
function NSNumberPtr(ANumber: Single): Pointer; overload;
function NSNumberPtr(ANumber: Int64): Pointer; overload;
function NSNumberPtr(ANumber: SmallInt): Pointer; overload;
function NSNumberPtr(ANumber: Cardinal): Pointer; overload;
function NSNumberPtr(ANumber: Word): Pointer; overload;
function NSNumberPtr(ANumber: UInt64): Pointer; overload;
function NSNumberPtr(ANumber: Boolean): Pointer; overload;

function PtrForObject(AObject: NSObject): Pointer;
function ObjcClassName(APtr: Pointer): string;
function StringToNSUrl(AString: string): NSUrl;
procedure OpenURL(AUrl: string);

{$IFDEF IOS}
function ActiveView: UIView;
function SharedApplication: UIApplication;
{$ELSE}
function ActiveView: NSView;
function SharedApplication: NSApplication;
function SharedWorkspace: NSWorkspace;
{$ENDIF}

implementation

uses
  Macapi.ObjectiveC, System.SysUtils, System.StrUtils, FMX.Platform, FMX.Forms,
  {$IFDEF IOS}
  FMX.Platform.iOS
  {$ELSE}
  FMX.Platform.Mac
  {$ENDIF}
  ;

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
  {$IFDEF IOS}
  Result := TNSString.OCClass.stringWithUTF8String(MarshaledAString(UTF8Encode(AString)));
  {$ELSE}
  Result := TNSString.OCClass.stringWithUTF8String(PAnsiChar(UTF8Encode(AString)));
  {$ENDIF}
end;

function PtrForObject(AObject: NSObject): Pointer;
begin
  Result := (AObject as ILocalObject).GetObjectID;
end;

function DateTimeToNSDate(ADateTime: TDateTime): NSDate;
const
  cnDateTemplate = 'YYYY-MM-dd HH:mm:ss';
  cnDateFmt = '%.4d-%.2d-%.2d %.2d:%.2d:%.2d';
var
  Day, Month, Year, Hour, Min, Sec, MSec: Word;
  DateStr: string;
  Tmp: NSDate;
  Formatter: NSDateFormatter;
begin
  DecodeDate(ADateTime, Year, Month, Day);
  DecodeTime(ADateTime, Hour, Min, Sec, MSec);
  DateStr := Format(cnDateFmt, [Year, Month, Day, Hour, Min, Sec, 0]);

  Formatter := TNSDateFormatter.Create;
  try
    Formatter.setDateFormat(NSStr(cnDateTemplate));
    Tmp := formatter.dateFromString(NSStr(DateStr));
    Result := TNSDate.Wrap(Tmp.addTimeInterval(MSec / 1000));
    Result.retain;
  finally
    Formatter.release;
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
begin
  Result := NSObjectToString(PtrForObject(AObject));
end;

function NSObjectToString(APtr: Pointer): string;
var
  lClass: string;
begin
  //This is NOT the ideal way to do this, but it will have to do until I can
  //figure out how to determine the actual Objective-C type behind a specified
  //pointer.
  lClass := ObjcClassName(APtr);
  if EndsStr('Date', lClass) then
    //This will format it a bit nicer than using NSDate.description, which
    //would use yyyy-mm-dd hh:mm:ss +0000
    Result := DateTimeToStr(NSDateToDateTime(APtr))
  else
    //The reason this works is because the NSObject class has a description
    //selector. While this isn't exposed in the Delphi wrapping of NSObject, we
    //can hijack the one in NSString, and the Objective-C runtime will happily
    //send the message to the object instance, which will respond accordingly.
    Result := NSStringToString(TNSString.Wrap(APtr).description);
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

function NSNumberToBool(ANumber: NSNumber): Boolean; overload;
begin
  Result := ANumber.boolValue;
end;

function NSNumberToBool(APtr: Pointer): Boolean; overload;
begin
  Result := NSNumberToBool(TNSNumber.Wrap(APtr));
end;

function NSNumberPtr(ANumber: Integer): Pointer;
begin
  Result := TNSNumber.OCClass.numberWithInt(ANumber);
end;

function NSNumberPtr(ANumber: Double): Pointer;
begin
  Result := TNSNumber.OCClass.numberWithDouble(ANumber);
end;

function NSNumberPtr(ANumber: Single): Pointer;
begin
  Result := TNSNumber.OCClass.numberWithFloat(ANumber);
end;

function NSNumberPtr(ANumber: Int64): Pointer;
begin
  Result := TNSNumber.OCClass.numberWithLongLong(ANumber);
end;

function NSNumberPtr(ANumber: SmallInt): Pointer;
begin
  Result := TNSNumber.OCClass.numberWithShort(ANumber);
end;

function NSNumberPtr(ANumber: Cardinal): Pointer;
begin
  Result := TNSNumber.OCClass.numberWithUnsignedInt(ANumber);
end;

function NSNumberPtr(ANumber: Word): Pointer;
begin
  Result := TNSNumber.OCClass.numberWithUnsignedShort(ANumber);
end;

function NSNumberPtr(ANumber: UInt64): Pointer;
begin
  Result := TNSNumber.OCClass.numberWithUnsignedLongLong(ANumber);
end;

function NSNumberPtr(ANumber: Boolean): Pointer; overload;
begin
  Result := TNSNumber.OCClass.numberWithBool(ANumber);
end;

function IntToNSNumber(ANumber: Integer): NSNumber;
begin
  Result := TNSNumber.Wrap(NSNumberPtr(ANumber));
end;

function ObjcClassName(APtr: Pointer): string;
begin
  Result := string(class_getname(object_getclass(APtr)));
end;

function StringToNSUrl(AString: string): NSUrl;
begin
  Result := TNSUrl.Wrap(TNSUrl.OCClass.URLWithString(NSStr(AString)))
end;

procedure OpenURL(AUrl: string);
begin
  {$IFDEF IOS}
  SharedApplication.openURL(StringToNSUrl(AUrl));
  {$ELSE}
  SharedWorkspace.openURL(StringToNSUrl(AUrl));
  {$ENDIF}
end;

{$IFDEF IOS}
function ActiveView: UIView;
{$ELSE}
function ActiveView: NSView;
{$ENDIF}
begin
  Result := WindowHandleToPlatform(Screen.ActiveForm.Handle).View;
end;

{$IFDEF IOS}
function SharedApplication: UIApplication;
begin
  Result := TUIApplication.wrap(TUIApplication.OCClass.SharedApplication);
end;
{$ELSE}
function SharedApplication: NSApplication;
begin
  Result := TNSApplication.wrap(TNSApplication.OCClass.SharedApplication);
end;
{$ENDIF}

{$IFNDEF IOS}
function SharedWorkspace: NSWorkspace;
begin
Result := TNSWorkspace.wrap(TNSWorkspace.OCClass.sharedWorkspace);
end;
{$ENDIF}

end.
