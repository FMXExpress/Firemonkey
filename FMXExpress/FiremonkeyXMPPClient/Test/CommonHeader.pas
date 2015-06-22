unit CommonHeader;

interface

uses fmx.Graphics, fmxJabberClient, sysutils, system.IOUtils, FMX.Dialogs;

var
  GJabberClient : TfmxJabberClient;

  function GetImageFilename(AName : string) : string;

implementation


{-------------------------------------------------------------------------------
  Procedure: GetImageFilename : Return the fullname(path+name) of an image depending on the target OS
  Arguments: AName : string
  Result:    string

  // TODO  : Add copatibility with OSX and IOS
-------------------------------------------------------------------------------}
function GetImageFilename(AName : string) : string;
begin
  {$IFDEF ANDROID}
    Result := TPath.Combine(TPath.GetHomePath(),AName)
  {$ELSE}
    Result := ExtractFilepath(ParamStr(0))+AName;
  {$ENDIF}
end;



initialization
  GJabberClient := TfmxJabberClient.Create(nil);    // Create Jaber client

finalization

  GJabberClient.Free;
end.
