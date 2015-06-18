unit UPatchMissingOpenURLEvent;

//This unit is a patch for a bug in XE4, XE6 unpatched: The OpenURL event doesn't get called.
// QC 115594
//The bug was fixed in XE6 SP1, so this doesn't apply for newer versions.

//This patch is applied automatically in the initialization section,
//so just including this unit in your project is enough to apply the patch.


//This unit is designed to work in Delphi XE4/XE5/XE6 (not sp1) to workaround a very specific bug:
//http://qc.embarcadero.com/wc/qcmain.aspx?d=115594

//So from XE6 SP1 or newer, we'll do nothing. You can remove the unit from your projects too.


interface
implementation
{$if CompilerVersion <= 26.0}
uses MacAPI.ObjCRuntime,
  iOSapi.Foundation, SysUtils,
  FMX.Platform.iOS, //This must be used, so the DelphiAppDelegate class is initialized before ours.
  FMX.Platform, iOSapi.UIKit, System.Rtti;

function CallFMXOpenURLEvent(const EventContext: TiOSOpenApplicationContext): boolean;
var
  AppService: IFMXApplicationService;
  PlatformCocoa: TObject; //TPlatformCocoaTouch is defined in the implementation section.
  c : TRttiContext;
  platc: TRttiType;
  HandleAppEventMethod: TRttiMethod;
  DelegateResult: TValue;
begin
  Result := false;
  if not TPlatformServices.Current.SupportsPlatformService(IFMXApplicationService, IInterface(AppService)) then exit;
  PlatformCocoa := AppService as TObject;
  if PlatformCocoa = nil then exit;


  c := TRttiContext.Create;
  platc := c.GetType(PlatformCocoa.ClassInfo);

  //We could instead get the AppDelegateProperty, and call the "application" method on it, but since application is overloaded, it is more difficult to find the right method to call.
  HandleAppEventMethod := platc.GetMethod('HandleApplicationEvent');
  if HandleAppEventMethod = nil then exit;
{$if CompilerVersion >= 27.0}
  DelegateResult := HandleAppEventMethod.Invoke(PlatformCocoa, [TValue.From(TApplicationEvent.OpenURL), EventContext]);
{$else}
  DelegateResult := HandleAppEventMethod.Invoke(PlatformCocoa, [TValue.From(TApplicationEvent.aeOpenURL), EventContext]);
{$endif}
  Result := DelegateResult.AsBoolean;
end;

function applicationOpenURL(self: pointer; _cmd: pointer;
  application: pointer; openURL: pointer; sourceApplication: pointer; annotation: Pointer): Boolean; cdecl;
var
  sourceApp: string;
  url: string;
begin
  sourceApp := Utf8ToString(TNSString.Wrap(sourceApplication).UTF8String);
  url:= UTF8ToString(TNSURL.Wrap(openURL).absoluteString.UTF8String);

  Result := CallFMXOpenURLEvent(TiOSOpenApplicationContext.Create(sourceApp, url, annotation));
end;

//This method adds the OpenURL delegate to the DelphiAppDelegate class
//which is defined by Delphi. In XE4, they have forgotten to add this delegate,
//so we need to add it ourselves. If in a future method the delegated is
//added to Delphi, then this method will do nothing. (as it checks the method isn't already added)
procedure AddOpenURLEvent;
var
  AppDelegateRef: pointer;
  uid: pointer;
begin
  AppDelegateRef := objc_getClass('DelphiAppDelegate');
  if AppDelegateRef <> nil then
  begin
    uid := sel_getUid('application:openURL:sourceApplication:annotation:');
    if (class_getInstanceMethod(AppDelegateRef, uid) = nil) then
    begin
      class_addMethod(appDelegateRef, uid, @applicationOpenURL, 'B@:@@@@');
    end;
  end;
end;


initialization
  AddOpenURLEvent;
{$endif}
end.
