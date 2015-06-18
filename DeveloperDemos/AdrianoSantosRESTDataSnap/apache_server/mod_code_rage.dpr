library mod_code_rage;

uses
  Winapi.ActiveX,
  System.Win.ComObj,
  Web.WebBroker,
  Web.ApacheApp,
  Web.HTTPD22Impl,
  Data.DBXCommon,
  Datasnap.DSSession,
  WebModuleUnit1 in 'WebModuleUnit1.pas' {WebModule1: TWebModule},
  ServerMethodsUnit1 in '..\server\ServerMethodsUnit1.pas' {ServerMethods1: TDSServerModule};

{$R *.res}

// httpd.conf entries:
//
(*
 LoadModule code_rage_module modules/mod_code_rage.dll

 <Location /xyz>
    SetHandler mod_code_rage-handler
 </Location>
*)
//
// These entries assume that the output directory for this project is the apache/modules directory.
//
// httpd.conf entries should be different if the project is changed in these ways:
//   1. The TApacheModuleData variable name is changed
//   2. The project is renamed.
//   3. The output directory is not the apache/modules directory
//

// Declare exported variable so that Apache can access this module.
var
  GModuleData: TApacheModuleData;
exports
  GModuleData name 'code_rage_module';

procedure TerminateThreads;
begin
  TDSSessionManager.Instance.Free;
  Data.DBXCommon.TDBXScheduler.Instance.Free;
end;

begin
  CoInitFlags := COINIT_MULTITHREADED;
  Web.ApacheApp.InitApplication(@GModuleData);
  Application.Initialize;
  Application.WebModuleClass := WebModuleClass;
  TApacheApplication(Application).OnTerminate := TerminateThreads;
  Application.Run;
end.
