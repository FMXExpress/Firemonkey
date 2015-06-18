unit NetworkState;

interface

uses
{$IFDEF IOS}
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.Dispatch,
  iOSApi.CocoaTypes, iOSApi.Foundation, Posix.SysSocket;
{$ENDIF}
{$IFDEF ANDROID}
System.SysUtils, FMX.Helpers.Android, Androidapi.JNIBridge,
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes;
{$ENDIF}

{$IFDEF IOS}
const
  libSystemConfiguration =
    '/System/Library/Frameworks/SystemConfiguration.framework/SystemConfiguration';

  kSCNetworkFlagsConnectionAutomatic = 8;
  kSCNetworkFlagsConnectionRequired = 4;
  kSCNetworkFlagsInterventionRequired = 16;
  kSCNetworkFlagsIsDirect = 131072;
  kSCNetworkFlagsIsLocalAddress = 65536;
  kSCNetworkFlagsReachable = 2;
  kSCNetworkFlagsTransientConnection = 1;
  kSCNetworkReachabilityFlagsConnectionAutomatic = 8;
  kSCNetworkReachabilityFlagsConnectionOnDemand = 32;
  kSCNetworkReachabilityFlagsConnectionOnTraffic = 8;
  kSCNetworkReachabilityFlagsConnectionRequired = 4;
  kSCNetworkReachabilityFlagsInterventionRequired = 16;
  kSCNetworkReachabilityFlagsIsDirect = 131072;
  kSCNetworkReachabilityFlagsIsLocalAddress = 65536;
  kSCNetworkReachabilityFlagsReachable = 2;
  kSCNetworkReachabilityFlagsTransientConnection = 1;
  kSCNetworkReachabilityFlagsIsWWAN = $40000;
{$ENDIF}

type
{$IFDEF IOS}
  SCNetworkReachabilityFlags = UInt32;

  SCNetworkReachabilityRef = ^__SCNetworkReachability;

  __SCNetworkReachability = record
  end;

  SCNetworkReachabilityContext = record
    version: CFIndex;
    info: Pointer;
    retain: function(info: Pointer): Pointer;
    release: procedure(info: Pointer);
    copyDescription: function(info: Pointer): CFStringRef;
  end;

  SCNetworkReachabilityContextPtr = ^SCNetworkReachabilityContext;

  SCNetworkReachabilityCallback = procedure(target: SCNetworkReachabilityRef;
    flags: SCNetworkReachabilityFlags; info: Pointer);

  TReachability = class;

  Reachability = interface(NSObject)
    ['{B405394F-57B1-4FF1-83D9-8FBFA38FFD7B}']
    function startNotifier: LongBool; cdecl;
    procedure stopNotifier; cdecl;
    function isReachable: LongBool; cdecl;
    function isReachableViaWWAN: LongBool; cdecl;
    function isReachableViaWiFi: LongBool; cdecl;
    function isConnectionRequired: LongBool; cdecl;
    function connectionRequired: LongBool; cdecl;
    function isConnectionOnDemand: LongBool; cdecl;
    function isInterventionRequired: LongBool; cdecl;
    function currentReachabilityStatus: NSInteger; cdecl;
    function reachabilityFlags: SCNetworkReachabilityFlags; cdecl;
    function currentReachabilityString: NSString; cdecl;
    function currentReachabilityFlags: NSString; cdecl;
  end;

  ReachabilityClass = interface(NSObjectClass)
    ['{39EC0490-2787-4BB9-95EA-77BB885BFD01}']
    function reachabilityWithHostname(hostname: NSString): Pointer; cdecl;
    function reachabilityForInternetConnection: Pointer; cdecl;
    function reachabilityWithAddress: Pointer; cdecl;
    function reachabilityForLocalWiFi: Pointer; cdecl;
  end;

  TReachability = class(TOCGenericImport<ReachabilityClass, Reachability>)
  end;

function SCNetworkReachabilityCreateWithAddress(allocator: CFAllocatorRef;
  address: psockaddr): SCNetworkReachabilityRef; cdecl;
  external libSystemConfiguration name _PU +
  'SCNetworkReachabilityCreateWithAddress';
function SCNetworkReachabilityCreateWithAddressPair(allocator: CFAllocatorRef;
  localAddress: psockaddr; remoteAddress: psockaddr): SCNetworkReachabilityRef;
  cdecl; external libSystemConfiguration name _PU +
  'SCNetworkReachabilityCreateWithAddressPair';
function SCNetworkReachabilityCreateWithName(allocator: CFAllocatorRef;
  nodename: PChar): SCNetworkReachabilityRef; cdecl;
  external libSystemConfiguration name _PU +
  'SCNetworkReachabilityCreateWithName';
function SCNetworkReachabilityGetTypeID: CFTypeID; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkReachabilityGetTypeID';
function SCNetworkReachabilityGetFlags(target: SCNetworkReachabilityRef;
  var flags: SCNetworkReachabilityFlags): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkReachabilityGetFlags';
function SCNetworkReachabilitySetCallback(target: SCNetworkReachabilityRef;
  callout: SCNetworkReachabilityCallback;
  var context: SCNetworkReachabilityContext): Boolean; cdecl;
  external libSystemConfiguration name _PU + 'SCNetworkReachabilitySetCallback';
function SCNetworkReachabilityScheduleWithRunLoop
  (target: SCNetworkReachabilityRef; runLoop: CFRunLoopRef;
  runLoopMode: CFStringRef): Boolean; cdecl;
  external libSystemConfiguration name _PU +
  'SCNetworkReachabilityScheduleWithRunLoop';
function SCNetworkReachabilityUnscheduleFromRunLoop
  (target: SCNetworkReachabilityRef; runLoop: CFRunLoopRef;
  runLoopMode: CFStringRef): Boolean; cdecl;
  external libSystemConfiguration name _PU +
  'SCNetworkReachabilityUnscheduleFromRunLoop';
function SCNetworkReachabilitySetDispatchQueue(target: SCNetworkReachabilityRef;
  queue: dispatch_queue_t): Boolean; cdecl;
  external libSystemConfiguration name _PU +
  'SCNetworkReachabilitySetDispatchQueue';

{$IFDEF CPUARM}
function FakeLoader: Reachability; cdecl;
  external 'libReachability.a' name 'OBJC_CLASS_$_Reachability';
{$ENDIF}

{$ENDIF}

{$IFDEF ANDROID}
JConnectivityManager = interface;
JNetworkInfo = interface;

JNetworkInfoClass = interface(JObjectClass)
  ['{E92E86E8-0BDE-4D5F-B44E-3148BD63A14C}']
end;

[JavaSignature('android/net/NetworkInfo')]
JNetworkInfo = interface(JObject)['{6DF61A40-8D17-4E51-8EF2-32CDC81AC372}']
{ Methods }
function isAvailable: Boolean;
cdecl;

function isConnected: Boolean; cdecl;
  function isConnectedOrConnecting: Boolean; cdecl;
  end;
TJNetworkInfo = class(TJavaGenericImport<JNetworkInfoClass, JNetworkInfo>)
end;

JConnectivityManagerClass = interface(JObjectClass)
  ['{E03A261F-59A4-4236-8CDF-0068FC6C5FA1}']
{ Property methods }
function _GetTYPE_WIFI: Integer;
cdecl;

function _GetTYPE_WIMAX: Integer; cdecl;
  function _GetTYPE_MOBILE: Integer; cdecl;
  { Properties }
  property TYPE_WIFI: Integer read _GetTYPE_WIFI;
  property TYPE_WIMAX: Integer read _GetTYPE_WIMAX;
  property TYPE_MOBILE: Integer read _GetTYPE_MOBILE;
  end;

[JavaSignature('android/net/ConnectivityManager')]
JConnectivityManager = interface(JObject)
  ['{1C4C1873-65AE-4722-8EEF-36BBF423C9C5}']
{ Methods }
function getActiveNetworkInfo: JNetworkInfo;
cdecl;

function getNetworkInfo(networkType: Integer): JNetworkInfo; cdecl;
end;
TJConnectivityManager = class(TJavaGenericImport<JConnectivityManagerClass,
  JConnectivityManager>)
end;
{$ENDIF}

type
  TMobileNetworkStatus = class(TObject)
  public
    constructor Create;
    destructor Destroy; override;
    function isConnected: Boolean;
    function IsWiFiConnected: Boolean;
    function IsMobileConnected: Boolean;
  end;

implementation

{$IFDEF ANDROID}

function GetConnectivityManager: JConnectivityManager;
var
  ConnectivityServiceNative: JObject;
begin
  ConnectivityServiceNative := SharedActivityContext.getSystemService
    (TJContext.JavaClass.CONNECTIVITY_SERVICE);
  if not Assigned(ConnectivityServiceNative) then
    raise Exception.Create('Could not locate Connectivity Service');
  Result := TJConnectivityManager.Wrap
    ((ConnectivityServiceNative as ILocalObject).GetObjectID);
  if not Assigned(Result) then
    raise Exception.Create('Could not access Connectivity Manager');
end;
{$ENDIF}

{$IFDEF IOS}

function GetInternetReachability: Reachability;
begin
  Result := TReachability.Wrap
    (TReachability.OCClass.reachabilityForInternetConnection);
end;
{$ENDIF}

constructor TMobileNetworkStatus.Create;
begin
end;

destructor TMobileNetworkStatus.Destroy;
begin
  inherited;
end;

function TMobileNetworkStatus.isConnected: Boolean;
{$IFDEF ANDROID}
var
  ConnectivityManager: JConnectivityManager;
  ActiveNetwork: JNetworkInfo;
{$ENDIF}
begin
{$IFDEF IOS}
  Result := GetInternetReachability.isReachable;
{$ENDIF}
{$IFDEF ANDROID}
  ConnectivityManager := GetConnectivityManager;
  ActiveNetwork := ConnectivityManager.getActiveNetworkInfo;
  Result := Assigned(ActiveNetwork) and ActiveNetwork.isConnected;
{$ENDIF}
end;

function TMobileNetworkStatus.IsMobileConnected: Boolean;
{$IFDEF ANDROID}
var
  ConnectivityManager: JConnectivityManager;
  MobileNetwork: JNetworkInfo;
{$ENDIF}
begin
{$IFDEF IOS}
  Result := GetInternetReachability.isReachableViaWWAN;
{$ENDIF}
{$IFDEF ANDROID}
  ConnectivityManager := GetConnectivityManager;
  MobileNetwork := ConnectivityManager.getNetworkInfo
    (TJConnectivityManager.JavaClass.TYPE_MOBILE);
  Result := MobileNetwork.isConnected;
{$ENDIF}
end;

function TMobileNetworkStatus.IsWiFiConnected: Boolean;
{$IFDEF ANDROID}
var
  ConnectivityManager: JConnectivityManager;
  WiFiNetwork: JNetworkInfo;
{$ENDIF}
begin
{$IFDEF IOS}
  Result := GetInternetReachability.isReachableViaWiFi;
{$ENDIF}
{$IFDEF ANDROID}
  ConnectivityManager := GetConnectivityManager;
  WiFiNetwork := ConnectivityManager.getNetworkInfo
    (TJConnectivityManager.JavaClass.TYPE_WIFI);
  Result := WiFiNetwork.isConnected;
{$ENDIF}
end;

initialization

{$IFDEF IOS}
{$IFDEF CPUARM}
if False then
  FakeLoader;
{$ENDIF}
{$ENDIF}

end.
