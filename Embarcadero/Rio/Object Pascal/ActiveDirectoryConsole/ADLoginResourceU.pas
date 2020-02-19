//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit ADLoginResourceU;

// EMS Resource Unit

interface

uses
  System.SysUtils, System.Classes, System.JSON, EMS.Services, EMS.ResourceAPI,
  EMS.ResourceTypes;

type
{$METHODINFO ON}
  /// <summary>
  ///   This class is a custom resource for the EMS server. It provides the
  ///   connectivity to the active directory (AD).
  /// </summary>
  [ResourceName('ADLogin')]
  TCustomLogonResource = class(TObject)
  strict private const
    cCommentField = 'comment';
    cLocalServer = 'localhost';
    cMasterPassword = 'AdEmsPwd#01';
  strict private
    /// <summary>
    ///   This method validates the username and the password. It checks if it
    ///   is possible to connect to the AD with this credentials. If there is
    ///   no username or password, or if it is not possible to connect to the AD
    ///   it raises an Unauthorized (401) error.
    /// </summary>
    procedure ValidateCredentials(const ARequest: TEndpointRequest; const AResponse: TEndpointResponse); overload;
    /// <summary>
    ///   This method validates the username and the password. It checks if it
    ///   is possible to connect to the AD with this credentials. If there is
    ///   no username or password, or if it is not possible to connect to the AD
    ///   it raises an Unauthorized (401) error.
    ///   The username and password are returned.
    /// </summary>
    procedure ValidateCredentials(const ARequest: TEndpointRequest; const AResponse: TEndpointResponse; out AUserName, APassword: string); overload;
    /// <summary>
    ///   This method returns internal password, which will be provided
    ///   to EMS instead of real AD end user password. This is required for
    ///   importing AD users when an AD user password is unknown, to avoid
    ///   need to sync AD and EMS password, and to avoid EMS users login
    ///   without AD autorization.
    ///   The internal password is returned.
    /// </summary>
    function GetIntPassword(const AUserName: string): string;
  public
    /// <summary>
    ///   This method deletes all EMS users that are not AD users. It is also
    ///   possible to add an optional group. This means that all EMS users are
    ///   deleted that are not member of the group. Be careful to call this.
    ///   Before the deletion happens the credential are checked with
    ///   ValidateCredentials. This means that the user and password are tested
    ///   towards the AD.
    /// </summary>
    [EndpointName('CustomDeleteUsers')]
    [ResourceSuffix('deleteusers')]
    procedure PostDeleteUsers(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
    /// <summary>
    ///   This method enumerates all AD groups. Before the enumeration happens
    ///   the credential are checked with ValidateCredentials. This means that
    ///   the user and password are tested towards the AD.
    /// </summary>
    [EndpointName('CustomEnumGroups')]
    [ResourceSuffix('enumgroups')]
    procedure PostEnumGroups(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
    /// <summary>
    ///   This method enumerates all AD users. Optionally you can add a group
    ///   so that you will get all AD users of the AD group. Before the
    ///   enumeration happens the credential are checked with ValidateCredentials.
    ///   This means that the user and password are tested towards the AD.
    /// </summary>
    [EndpointName('CustomEnumUsers')]
    [ResourceSuffix('enumusers')]
    procedure PostEnumUsers(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
    /// <summary>
    ///   This method imports all AD users into the EMS. If an EMS user already
    ///   exists nothing happens. That's why you can call this method several
    ///   times. It is also possible to add an optional group. Then only the AD
    ///   users of the AD group are imported. Before the deletion happens the
    ///   credential are checked with ValidateCredentials. This means that the
    ///   user and password are tested towards the AD.
    /// </summary>
    [EndpointName('CustomImport')]
    [ResourceSuffix('import')]
    procedure PostImport(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
    /// <summary>
    ///   This method replaces the normal login towards the EMS systems with an
    ///   login towards the AD.
    /// </summary>
    [EndpointName('CustomLoginUser')]
    // Declare endpoint that matches signature of Users.LoginUser
    [ResourceSuffix('login')]
    procedure PostLogin(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
    /// <summary>
    ///   This method checks the credentials versus the AD and then signs the
    ///   user into the EMS system. It replaces the normal signup towards the
    ///   EMS system.
    /// </summary>
    [EndpointName('CustomSignupUser')]
    // Declare endpoint that matches signature of Users.SignupUser
    [ResourceSuffix('signup')]
    procedure PostSignup(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
  end;
{$METHODINFO OFF}

procedure Register;

implementation

uses
  System.Generics.Collections, System.Hash, EMS.AD.Classes;

resourcestring
  SMissingCredentials = 'Missing credentials';
  SCustomAdded = 'This user added by CustomResource.CustomSignupUser';

procedure Register;
begin
  RegisterResource(TypeInfo(TCustomLogonResource));
end;

{ TCustomLogonResource }

procedure TCustomLogonResource.ValidateCredentials(const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  lUserName: string;
  lPassword: string;
begin
  ValidateCredentials(ARequest, AResponse, lUserName, lPassword);
end;

procedure TCustomLogonResource.ValidateCredentials(const ARequest: TEndpointRequest; const AResponse: TEndpointResponse; out AUserName, APassword: string);
var
  bCredentials: Boolean;
  lValue: TJSONValue;
  lError: string;
begin
  AUserName := '';
  APassword := '';
  bCredentials := ARequest.Body.TryGetValue(lValue) and
    lValue.TryGetValue<string>(TEMSInternalAPI.TJSONNames.UserName, AUserName) and
    lValue.TryGetValue<string>(TEMSInternalAPI.TJSONNames.Password, APassword);

  if not bCredentials then
    AResponse.RaiseUnauthorized('', SMissingCredentials)
  else if not TEnumAD.CredentialsValid('', AUserName, APassword, lError) then
    AResponse.RaiseUnauthorized('', lError);

  APassword := GetIntPassword(AUserName);
end;

function TCustomLogonResource.GetIntPassword(const AUserName: string): string;
begin
  Result := THashMD5.GetHashString(cMasterPassword + AUserName);
end;

procedure TCustomLogonResource.PostDeleteUsers(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  lEMSAPI: TEMSInternalAPI;
  lValue: TJSONValue;
  lResponse: TJSONObject;
  lGroup: string;
  lContent: IEMSResourceResponseContent;
  lParams: TEMSResourceRequestQueryParams;
  lADUsers: TDictionary<string, string>;
  lUserId: string;
  lUser: string;
  lBuffer: string;
  lArray: TJSONArray;
begin
  ValidateCredentials(ARequest, AResponse);

  lResponse := nil;
  lADUsers := nil;
  lEMSAPI := TEMSInternalAPI.Create(AContext);
  try
    lValue := ARequest.Body.GetValue;

    lADUsers := TDictionary<string, string>.Create;
    if not lValue.TryGetValue<string>(TEMSInternalAPI.TJSONNames.GroupName, lGroup) then
      TEnumAD.EnumUsers(cLocalServer,
        procedure(AUserName: string)
        begin
          lADUsers.Add(AUserName, AUserName);
        end)
    else
      TEnumAD.EnumUsersOfAGroup(cLocalServer, lGroup,
        procedure(AUserName: string)
        begin
          lADUsers.Add(AUserName, AUserName);
        end);

    lResponse := TJSONObject.Create;
    lContent := lEMSAPI.QueryUsers(lParams);
    if lContent.TryGetValue(lValue) then
    begin
      lArray := lValue as TJSONArray;
      for lValue in lArray do
      begin
        lUserId := lValue.GetValue<string>(TEMSInternalAPI.TJSONNames.UserID);
        lUser := lValue.GetValue<string>(TEMSInternalAPI.TJSONNames.UserName);
        if not lADUsers.TryGetValue(lUser, lBuffer) then
        begin
          lEMSAPI.DeleteUser(lUserId);
          lResponse.AddPair(lUser, lUser);
        end;
      end;
    end;

    AResponse.Body.SetValue(lResponse, False);
  finally
    lResponse.Free;
    lADUsers.Free;
    lEMSAPI.Free;
  end;
end;

procedure TCustomLogonResource.PostEnumGroups(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  lGroupName: string;
  lGroups: TStrings;
  lResponse: TJSONObject;
begin
  ValidateCredentials(ARequest, AResponse);

  lResponse := nil;
  lGroups := TStringList.Create;
  try
    TEnumAD.EnumGroups(cLocalServer, lGroups);
    lResponse := TJSONObject.Create;
    for lGroupName in lGroups do
      lResponse.AddPair(lGroupName, lGroupName);
    AResponse.Body.SetValue(lResponse, False);
  finally
    lGroups.Free;
    lResponse.Free;
  end;
end;

procedure TCustomLogonResource.PostEnumUsers(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  lUserName: string;
  lUsers: TStrings;
  lResponse: TJSONObject;
  lValue: TJSONValue;
  lGroup: string;
begin
  ValidateCredentials(ARequest, AResponse);

  lResponse := nil;
  lUsers := TStringList.Create;
  try
    lValue := ARequest.Body.GetValue;
    if not lValue.TryGetValue<string>(TEMSInternalAPI.TJSONNames.GroupName, lGroup) then
      TEnumAD.EnumUsers(cLocalServer, lUsers)
    else
      TEnumAD.EnumUsersOfAGroup(cLocalServer, lGroup, lUsers);
    lResponse := TJSONObject.Create;
    for lUserName in lUsers do
      lResponse.AddPair(lUserName, lUserName);
    AResponse.Body.SetValue(lResponse, False);
  finally
    lUsers.Free;
    lResponse.Free;
  end;
end;

procedure TCustomLogonResource.PostImport(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  lEMSAPI: TEMSInternalAPI;
  lUserName: string;
  lUserFields: TJSONObject;
  lUsers: TStrings;
  lValue: TJSONValue;
  lResponse: TJSONObject;
  lGroup: string;
begin
  ValidateCredentials(ARequest, AResponse);

  lResponse := nil;
  lUserFields := nil;
  lUsers := nil;
  lEMSAPI := TEMSInternalAPI.Create(AContext);
  try
    lValue := ARequest.Body.GetValue;

    lUsers := TStringList.Create;
    if not lValue.TryGetValue<string>(TEMSInternalAPI.TJSONNames.GroupName, lGroup) then
      TEnumAD.EnumUsers(cLocalServer, lUsers)
    else
      TEnumAD.EnumUsersOfAGroup(cLocalServer, lGroup, lUsers);

    lUserFields := TJSONObject.Create;
    lUserFields.AddPair(cCommentField, SCustomAdded);

    lResponse := TJSONObject.Create;
    for lUserName in lUsers do
    begin
      if not lEMSAPI.QueryUserName(lUserName) then
      begin
        lEMSAPI.SignupUser(lUserName, GetIntPassword(lUserName), lUserFields);
        lResponse.AddPair(lUserName, lUserName);
      end;
    end;

    AResponse.Body.SetValue(lResponse, False);
  finally
    lResponse.Free;
    lUsers.Free;
    lUserFields.Free;
    lEMSAPI.Free;
  end;
end;

procedure TCustomLogonResource.PostLogin(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  lEMSAPI: TEMSInternalAPI;
  lResponse: IEMSResourceResponseContent;
  lValue: TJSONValue;
  lUserName: string;
  lPassword: string;
begin
  ValidateCredentials(ARequest, AResponse, lUserName, lPassword);

  lEMSAPI := TEMSInternalAPI.Create(AContext);
  try
    // Uncomment to automatically create an EMS user if the AD credentials are valid,
    // but EMS user does not exist. Otherwise, EMS will raise a not-found exception
    // if an EMS user has not already been signed up.
    {
    if not lEMSAPI.QueryUserName(lUserName) then
      PostSignup(AContext, ARequest, AResponse);
    }

    lResponse := lEMSAPI.LoginUser(lUserName, lPassword);
    if lResponse.TryGetValue(lValue) then
      AResponse.Body.SetValue(lValue, False);
  finally
    lEMSAPI.Free;
  end;
end;

procedure TCustomLogonResource.PostSignup(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  lEMSAPI: TEMSInternalAPI;
  lResponse: IEMSResourceResponseContent;
  lValue: TJSONObject;
  lUserName: string;
  lPassword: string;
  lUserFields: TJSONObject;
begin
  ValidateCredentials(ARequest, AResponse, lUserName, lPassword);

  lEMSAPI := TEMSInternalAPI.Create(AContext);
  try
    lValue := ARequest.Body.GetObject;
    lUserFields := lValue.Clone as TJSONObject;
    try
      // Remove meta data
      lUserFields.RemovePair(TEMSInternalAPI.TJSONNames.UserName).Free;
      lUserFields.RemovePair(TEMSInternalAPI.TJSONNames.Password).Free;
      // Add another field, for example
      lUserFields.AddPair(cCommentField, SCustomAdded);
      // in-process call to actual Users/Signup endpoint
      lResponse := lEMSAPI.SignupUser(lUserName, lPassword, lUserFields)
    finally
      lUserFields.Free;
    end;
    if lResponse.TryGetObject(lValue) then
      AResponse.Body.SetValue(lValue, False);
  finally
    lEMSAPI.Free;
  end;
end;

end.


