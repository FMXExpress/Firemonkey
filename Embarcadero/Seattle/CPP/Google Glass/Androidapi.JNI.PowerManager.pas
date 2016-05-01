// This software is Copyright (c) 2014 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of one of Embarcadero's developer tools products.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

{**********************************************************}
{                                                          }
{           CodeGear Delphi Redistributable                }
{                                                          }
{ Delphi-JNI Bridge                                        }
{ Interfaces for Android OS PowerManager & WakeLock        }
{                                                          }
{ Original is part of the Android Open Source Project      }
{ Used under Apache 2.0 license                            }
{                                                          }
{ Translator: Embarcadero Technologies, Inc.               }
{   Copyright(c) 2014 Embarcadero Technologies, Inc.       }
{                                                          }
{**********************************************************}

unit Androidapi.JNI.PowerManager;

interface

{$IFDEF Android}
uses
  System.SysUtils,
  Androidapi.JNIBridge,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes,
  Androidapi.Helpers,
  FMX.Helpers.Android;

type
  { Class forward declarations }
  JWakeLock = interface; // android.os.PowerManager.WakeLock
  JPowerManager = interface; // android.os.PowerManager

  JPowerManager_WakeLockClass = interface(JObjectClass)
    ['{DA204013-460E-4CE5-B77E-772870E53853}']
  end;

  // Any application using a WakeLock must request the
    //    android.permission.WAKE_LOCK permission
  // http://developer.android.com/reference/android/os/PowerManager.WakeLock.html
  [JavaSignature('android/os/PowerManager$WakeLock')]
  JWakeLock = interface(JObject)
    ['{944B58EB-1BDA-403B-AF6F-D37E07CFE914}']
    { Methods }
    procedure setReferenceCounted(referenceCount: Boolean); cdecl;
    procedure acquire; cdecl; overload;
    procedure acquire(timeout: Int64); cdecl; overload;
    procedure release; cdecl;
    function isHeld: Boolean; cdecl;
    // procedure setWorkSource(ws: JWorkSource); cdecl;
    function toString: JString; cdecl;
  end;

  TJWakeLock = class
    (TJavaGenericImport<JPowerManager_WakeLockClass, JWakeLock>)
  end;

  // http://developer.android.com/reference/android/os/PowerManager.html
  JPowerManagerClass = interface(JObjectClass)
    ['{E4AC8BDC-18D1-42AA-84A3-A1ACEAAEDAEF}']
    { Property Methods }
    function _GetPARTIAL_WAKE_LOCK: Integer;
    function _GetSCREEN_DIM_WAKE_LOCK: Integer;
    function _GetSCREEN_BRIGHT_WAKE_LOCK: Integer;
    function _GetFULL_WAKE_LOCK: Integer;
    function _GetACQUIRE_CAUSES_WAKEUP: Integer;
    function _GetON_AFTER_RELEASE: Integer;
    { Properties }
    // Wake lock level: Ensures that the CPU is running; the screen and keyboard
    //    backlight will be allowed to go off.
    // If the user presses the power button, then the screen will be turned off
    //    but the CPU will be kept on until all partial wake locks have been
    //    released.
    property PARTIAL_WAKE_LOCK: Integer read _GetPARTIAL_WAKE_LOCK;
    // Wake lock level: Ensures that the screen is on (but may be dimmed); the
    //    keyboard backlight will be allowed to go off.
    // If the user presses the power button, then the SCREEN_DIM_WAKE_LOCK will
    //    be implicitly released by the system, causing both the screen and the
    //    CPU to be turned off. Contrast with PARTIAL_WAKE_LOCK.
    property SCREEN_DIM_WAKE_LOCK: Integer read _GetSCREEN_DIM_WAKE_LOCK;
    // Wake lock level: Ensures that the screen is on at full brightness; the
    //    keyboard backlight will be allowed to go off.
    // If the user presses the power button, then the SCREEN_BRIGHT_WAKE_LOCK
    //    will be implicitly released by the system, causing both the screen and
    //    the CPU to be turned off. Contrast with PARTIAL_WAKE_LOCK.
    property SCREEN_BRIGHT_WAKE_LOCK: Integer read _GetSCREEN_BRIGHT_WAKE_LOCK;
    // Wake lock level: Ensures that the screen and keyboard backlight are on at
    //    full brightness.
    // If the user presses the power button, then the FULL_WAKE_LOCK will be
    //    implicitly released by the system, causing both the screen and the CPU
    //    to be turned off. Contrast with PARTIAL_WAKE_LOCK.
    property FULL_WAKE_LOCK: Integer read _GetFULL_WAKE_LOCK;
    // Normally wake locks don't actually wake the device, they just cause the
    //    screen to remain on once it's already on. Think of the video player
    //    application as the normal behavior. Notifications that pop up and want
    //    the device to be on are the exception; use this flag to be like them.
    // Cannot be used with PARTIAL_WAKE_LOCK.
    property ACQUIRE_CAUSES_WAKEUP: Integer read _GetACQUIRE_CAUSES_WAKEUP;
    // Wake lock flag: When this wake lock is released, poke the user activity
    //    timer so the screen stays on for a little longer.
    // Will not turn the screen on if it is not already on.
    // Cannot be used with PARTIAL_WAKE_LOCK.
    property ON_AFTER_RELEASE: Integer read _GetON_AFTER_RELEASE;
  end;

  [JavaSignature('android/os/PowerManager')]
  JPowerManager = interface(JObject)
    ['{DEAED658-4353-4D17-B0A3-8179E48BE87F}']
    { Methods }
    // Creates a new wake lock with the specified level and flags.
    function newWakeLock(levelAndFlags: Integer; tag: JString)
      : JWakeLock; cdecl;
    // Notifies the power manager that user activity happened.
    // Resets the auto-off timer and brightens the screen if the device is not
    //    asleep. This is what happens normally when a key or the touch screen
    //    is pressed or when some other user activity occurs. This method does
    //    not wake up the device if it has been put to sleep.
    procedure userActivity(when: Int64; noChangeLights: Boolean); cdecl;
    // Forces the device to go to sleep.
    // Overrides all the wake locks that are held. This is what happens when the
    //    power key is pressed to turn off the screen.
    // Requires the DEVICE_POWER permission.
    procedure goToSleep(time: Int64); cdecl;
    // Forces the device to wake up from sleep.
    // If the device is currently asleep, wakes it up, otherwise does nothing.
    //    This is what happens when the power key is pressed to turn on the
    //    screen.
    // Requires the DEVICE_POWER permission.
    procedure wakeUp(long: Int64); cdecl;
    // Returns whether the screen is currently on.
    // Only indicates whether the screen is on. The screen could be either
    //    bright or dim.
    function isScreenOn: Boolean; cdecl;
    // Reboot the device. Will not return if the reboot is successful.
    // Requires the REBOOT permission.
    procedure reboot(reason: JString); cdecl;
  end;

  TJPowerManager = class(TJavaGenericImport<JPowerManagerClass, JPowerManager>)
  end;

function GetPowerManager: JPowerManager;
function AcquireWakeLock: Boolean;
procedure ReleaseWakeLock;

{$ENDIF}

implementation

{$IFDEF Android}

var
  AWakeLock: JWakeLock;

function GetPowerManager: JPowerManager;
var
  PowerServiceNative: JObject;
begin
  PowerServiceNative := SharedActivityContext.getSystemService
    (TJContext.JavaClass.POWER_SERVICE);
  if not Assigned(PowerServiceNative) then
    raise Exception.Create('Could not locate Power Service.');
  Result := TJPowerManager.Wrap((PowerServiceNative as ILocalObject)
    .GetObjectID);
  if not Assigned(Result) then
    raise Exception.Create('Could not access Power Manager.');
end;

function AcquireWakeLock: Boolean;
var
  PowerManager: JPowerManager;
begin
  Result := Assigned(AWakeLock);
  if not Result then
  begin
    PowerManager := GetPowerManager;
    AWakeLock := PowerManager.newWakeLock
      (TJPowerManager.JavaClass.SCREEN_BRIGHT_WAKE_LOCK
        or TJPowerManager.JavaClass.ACQUIRE_CAUSES_WAKEUP,
      StringToJString('{4D9BA098-BA3B-4054-A70F-BAB085B5B8A1}'));
    Result := Assigned(AWakeLock);
  end;
  if Result then
  begin
    if not AWakeLock.isHeld then
    begin
      AWakeLock.acquire;
      Result := AWakeLock.isHeld
    end;
  end;
end;

procedure ReleaseWakeLock;
begin
  if Assigned(AWakeLock) then
  begin
    AWakeLock.release;
    AWakeLock := nil
  end;
end;

{$ENDIF}

end.
