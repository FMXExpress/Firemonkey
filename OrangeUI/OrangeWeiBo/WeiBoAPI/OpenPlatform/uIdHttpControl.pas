unit uIdHttpControl;

interface

//{$DEFINE USE_OPENSSL}
//{$DEFINE STATICLOAD_OPENSSL}
//{$DEFINE HAS_DIRECTIVE_HPPEMIT_LINKUNIT}

uses
//  Windows,
  Classes,
  SysUtils,
  IdBaseComponent,
  uFuncCommon,
  IdComponent,
  IdIOHandler,
  IdIOHandlerSocket,
  IdIOHandlerStack,
//  REST.OpenSSL,
  IdHttp,
  IdSSLOpenSSL,
//  IdSSL,
//  IdSSLOpenSSLHeaders_Static,
  uOpenPlatform;


type
  TIdHttpControl=class(THttpControl)
  private
    FIdHttp:TIdHttp;
    FIdSSLIOHandlerSocketOpenSSL:TIdSSLIOHandlerSocketOpenSSL;
  public
    constructor Create;override;
    destructor Destroy;override;
    function Get(const HttpUrl:String;AResponseStream:TStream):Boolean;overload;override;
    function Get(const HttpUrl:String):String;overload;override;
    function Post(const HttpUrl:String;ARequestStream:TStream;AResponseStream:TStream):Boolean;overload;override;
    function Post(const HttpUrl:String;ARequestStream:TStream):String;overload;override;
  end;

implementation

{ TIdHttpControl }

constructor TIdHttpControl.Create;
begin
  Inherited Create;

  FIdHttp:=TIdHttp.Create(nil);
//  FIdHttp.ReadTimeout:=0;
//  FIdHttp.AllowCookies:=True;
//  FIdHttp.ProxyParams.BasicAuthentication:=False;
//  FIdHttp.ProxyParams.ProxyPort:=0;
//  FIdHttp.Request.ContentLength:=-1;
//  FIdHttp.Request.ContentRangeEnd:=0;
//  FIdHttp.Request.ContentRangeStart:=0;
//  FIdHttp.Request.ContentType:='application/x-www-form-urlencoded';
//  FIdHttp.Request.Accept:='text/html, */*';
//  FIdHttp.Request.BasicAuthentication:=False;
//  FIdHttp.Request.UserAgent:='Mozilla/3.0 (compatible; Indy Library)';
//  FIdHttp.HTTPOptions:=[hoForceEncodeParams];

  FIdSSLIOHandlerSocketOpenSSL:=TIdSSLIOHandlerSocketOpenSSL.Create(FIdHttp);
  FIdHttp.IOHandler:=FIdSSLIOHandlerSocketOpenSSL;
  FIdSSLIOHandlerSocketOpenSSL.SSLOptions.Method:=sslvTLSv1;
//  FIdSSLIOHandlerSocketOpenSSL.SSLOptions.VerifyMode := [TIdSSLVerifyMode.sslvrfClientOnce];


  //  FIdSSLIOHandlerSocketOpenSSL.SSLOptions.SSLVersions
end;

destructor TIdHttpControl.Destroy;
begin
  FreeAndNil(FIdSSLIOHandlerSocketOpenSSL);
  FreeAndNil(FIdHttp);
  inherited;
end;

function TIdHttpControl.Get(const HttpUrl: String): String;
begin
  Result:='';
  Result:=FIdHttp.Get(HttpUrl);
end;

function TIdHttpControl.Get(const HttpUrl: String;AResponseStream: TStream): Boolean;
begin
  Result:=False;
  FIdHttp.Get(HttpUrl,AResponseStream);
  Result:=True;
end;

//function TIdHttpControl.Post(const HttpUrl: String;AResponseStream: TStream): Boolean;
//var
//  AStringList:TStringList;
//begin
//  Result:=False;
//  AStringList:=TStringList.Create;
//  try
//    FIdHttp.Post(HttpUrl,AStringList,AResponseStream);
//  finally
//    AStringList.Free;
//  end;
//  Result:=True;
//end;

function TIdHttpControl.Post(const HttpUrl: String; ARequestStream,AResponseStream: TStream): Boolean;
begin
  Result:=False;
  FIdHttp.Post(HttpUrl,ARequestStream,AResponseStream);
  Result:=True;
end;

function TIdHttpControl.Post(const HttpUrl: String;ARequestStream: TStream): String;
begin
  Result:='';
  Result:=FIdHttp.Post(HttpUrl,ARequestStream);
end;




end.
