//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit CustomLoginResourceU;

// EMS Resource Unit

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  EMS.Services, EMS.ResourceAPI,
  EMS.ResourceTypes;

type
  {$METHODINFO ON}
  [ResourceName('CustomLogin')]
  TCustomLogonResource = class
  private
    procedure ValidateCredentials(const ADomainName, AUserName,
      APassword: string);
  public
    [EndpointName('CustomSignupUser')]
    // Declare endpoint that matches signature of Users.SignupUser
    [ResourceSuffix('signup')]
    procedure PostSignup(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);

    [EndpointName('CustomLoginUser')]
    // Declare endpoint that matches signature of Users.LoginUser
    [ResourceSuffix('login')]
    procedure PostLogin(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
  end;
  {$METHODINFO OFF}

procedure Register;

implementation

uses Winapi.Windows;

procedure Register;
begin
  RegisterResource(TypeInfo(TCustomLogonResource));
end;

{ TCustomLogonResource }

// Validate credentials using windows logon API.
procedure TCustomLogonResource.ValidateCredentials(const ADomainName, AUserName, APassword: string);
const
  LOGON32_LOGON_NETWORK_CLEARTEXT = 8;
var
  LLoginType: Cardinal;
  LLoginProvider: Cardinal;
  LToken: THandle;
  LError: Cardinal;
begin
  LLoginType := LOGON32_LOGON_NETWORK_CLEARTEXT;
  LLoginProvider := LOGON32_PROVIDER_DEFAULT;
  if LogonUser(PChar(AUserName), PChar(ADomainName), PChar(APassword), LLoginType, LLoginProvider, LToken) then
    CloseHandle(LToken)
  else
  begin
    LError := GetLastError;
    EEMSHTTPError.RaiseUnauthorized('', SysErrorMessage(LError));
  end;
end;

// Custom EMS login
procedure TCustomLogonResource.PostLogin(const AContext: TEndpointContext;
  const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  LEMSAPI: TEMSInternalAPI;
  LResponse: IEMSResourceResponseContent;
  LValue: TJSONValue;
  LUserName: string;
  LPassword: string;
begin
  // Create in-process EMS API
  LEMSAPI := TEMSInternalAPI.Create(AContext);
  try
    // Extract credentials from request
    if not (ARequest.Body.TryGetValue(LValue) and
      LValue.TryGetValue<string>(TEMSInternalAPI.TJSONNames.UserName, LUserName) and
      LValue.TryGetValue<string>(TEMSInternalAPI.TJSONNames.Password, LPassword)) then
      AResponse.RaiseBadRequest('', 'Missing credentials');
    ValidateCredentials('', LUserName, LPassword);
// Uncomment to automatically create a user if the credentials are valid.
// Otherwise, EMS will raise a not-found exception if an EMS user has not already been signed up.
//    if not LEMSAPI.QueryUserName(LUserName) then
//      // Add user when there is no user for these credentials
//      // in-process call to actual Users/Signup endpoint
//      LResponse := LEMSAPI.SignupUser(LUserName, LPassword, nil)
//    else
      // in-process call to actual Users/Login endpoint
      LResponse := LEMSAPI.LoginUser(LUserName, LPassword);
    if LResponse.TryGetValue(LValue) then
      AResponse.Body.SetValue(LValue, False);
  finally
    LEMSAPI.Free;
  end;
end;

// Custom EMS signup
procedure TCustomLogonResource.PostSignup(const AContext: TEndpointContext;
  const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  LEMSAPI: TEMSInternalAPI;
  LResponse: IEMSResourceResponseContent;
  LValue: TJSONObject;
  LUserName: string;
  LPassword: string;
  LUserFields: TJSONObject;
begin
  // Create in-process EMS API
  LEMSAPI := TEMSInternalAPI.Create(AContext);
  try
    // Extract credentials from request
    if not (ARequest.Body.TryGetObject(LValue) and
      LValue.TryGetValue<string>(TEMSInternalAPI.TJSONNames.UserName, LUserName) and
      LValue.TryGetValue<string>(TEMSInternalAPI.TJSONNames.Password, LPassword)) then
      AResponse.RaiseBadRequest('', 'Missing credentials');
    ValidateCredentials('', LUserName, LPassword);
    LUserFields := LValue.Clone as TJSONObject;
    try
      // Remove meta data
      LUserFields.RemovePair(TEMSInternalAPI.TJSONNames.UserName);
      LUserFields.RemovePair(TEMSInternalAPI.TJSONNames.Password);
      // Add another field, for example
      LUserFields.AddPair('comment', 'This user added by CustomResource.CustomSignupUser');
      // in-process call to actual Users/Signup endpoint
      LResponse := LEMSAPI.SignupUser(LUserName, LPassword, LUserFields)
    finally
      LUserFields.Free;
    end;
    if LResponse.TryGetObject(LValue) then
      AResponse.Body.SetValue(LValue, False);
  finally
    LEMSAPI.Free;
  end;
end;


end.


