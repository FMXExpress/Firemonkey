unit OpenViewUrl;
 
interface
 
// URLEncode is performed on the URL
// so you need to format it   protocol://path
function OpenURL(const URL: string; const DisplayError: Boolean = False): Boolean;
 
implementation
 
uses
  IdURI, SysUtils, Classes, FMX.Dialogs,
{$IFDEF ANDROID}
  FMX.Helpers.Android, Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Net, Androidapi.JNI.JavaTypes;
{$ELSE}
{$IFDEF IOS}
  iOSapi.Foundation, FMX.Helpers.iOS;
{$ENDIF IOS}
{$ENDIF ANDROID}
 
function OpenURL(const URL: string; const DisplayError: Boolean = False): Boolean;
{$IFDEF ANDROID}
var
  Intent: JIntent;
begin
// There may be an issue with the geo: prefix and URLEncode.
// will need to research
  Intent := TJIntent.JavaClass.init(TJIntent.JavaClass.ACTION_VIEW,
    TJnet_Uri.JavaClass.parse(StringToJString(TIdURI.URLEncode(URL))));
  try
    SharedActivity.startActivity(Intent);
    exit(true);
  except
    on e: Exception do
    begin
      if DisplayError then ShowMessage('Error: ' + e.Message);
      exit(false);
    end;
  end;
end;
{$ELSE}
{$IFDEF IOS}
var
  NSU: NSUrl;
begin
  // iOS doesn't like spaces, so URL encode is important.
  NSU := StrToNSUrl(TIdURI.URLEncode(URL));
  if SharedApplication.canOpenURL(NSU) then
    exit(SharedApplication.openUrl(NSU))
  else
  begin
    if DisplayError then
      ShowMessage('Error: Opening "' + URL + '" not supported.');
    exit(false);
  end;
end;
{$ELSE}
begin
  raise Exception.Create('Not supported!');
end;
{$ENDIF IOS}
{$ENDIF ANDROID}
 
end.