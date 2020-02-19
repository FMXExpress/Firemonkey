unit EMS.AD.Classes;

interface

uses
  System.SysUtils, System.Classes, Winapi.Windows, EMS.AD.Api;

type
  /// <summary>
  ///   Exception class for TEnumUserResult.
  /// </summary>
  EEnumUserResult = class(Exception);

  /// <summary>
  ///   Enumeration for the result of the EnumUsers function.
  /// </summary>
  TEnumUserResult = (
    /// <summary>
    ///   The operation completed successfully.
    /// </summary>
    NERR_Success,
    /// <summary>
    ///   The user does not have access to the requested information.
    /// </summary>
    ERROR_ACCESS_DENIED,
    /// <summary>
    ///   The system call level is not correct. This error is returned if the
    ///   level parameter is set to a value not supported.
    /// </summary>
    ERROR_INVALID_LEVEL,
    /// <summary>
    ///   The buffer is too small to contain an entry. No information has been
    ///   written to the buffer.
    /// </summary>
    NERR_BufTooSmall,
    /// <summary>
    ///   The computer name is invalid.
    /// </summary>
    NERR_InvalidComputer,
    /// <summary>
    ///   More entries are available. Specify a large enough buffer to receive
    ///   all entries.
    /// </summary>
    ERROR_MORE_DATA);


  /// <summary>
  ///   An enum type for the NetUserEnum filter value.
  /// </summary>
  TADFilter = (
    /// <summary>
    ///   Indicates that all normal user, trust data, and machine account data
    ///   should be included.
    /// </summary>
    FILTER_ALL,
    /// <summary>
    ///   Enumerates account data for users whose primary account is in another
    ///   domain. This account type provides user access to this domain, but
    ///   not to any domain that trusts this domain. The User Manager refers to
    ///   this account type as a local user account.
    /// </summary>
    FILTER_TEMP_DUPLICATE_ACCOUNT,
    /// <summary>
    ///   Enumerates normal user account data. This account type is associated
    ///   with a typical user.
    /// </summary>
    FILTER_NORMAL_ACCOUNT,
    /// <summary>
    ///   Enumerates interdomain trust account data. This account type is
    ///   associated with a trust account for a domain that trusts other
    ///   domains.
    /// </summary>
    FILTER_INTERDOMAIN_TRUST_ACCOUNT,
    /// <summary>

    ///   Enumerates workstation or member server trust account data. This
    ///   account type is associated with a machine account for a computer that
    ///   is a member of the domain.
    /// </summary>
    FILTER_WORKSTATION_TRUST_ACCOUNT,

    /// <summary>

    ///   Enumerates member server machine account data. This account type is
    ///   associated with a computer account for a backup domain controller
    ///   that is a member of the domain.
    /// </summary>
    FILTER_SERVER_TRUST_ACCOUNT);


   TEnumUserResultHelper = record helper for TEnumUserResult
   strict private const
     cIntegerValues: array[TEnumUserResult] of Integer =
     (TEMSADReturnConsts.NERR_Success,
      Winapi.Windows.ERROR_ACCESS_DENIED,
      Winapi.Windows.ERROR_INVALID_LEVEL,
      TEMSADReturnConsts.NERR_BufTooSmall,
      TEMSADReturnConsts.NERR_InvalidComputer,
      Winapi.Windows.ERROR_MORE_DATA);
   public
     /// <summary>
     ///   Returns the high value of the enum type.
     /// </summary>
     class function High: TEnumUserResult; static; inline;
     /// <summary>
     ///   Returns the low value of the enum type.
     /// </summary>
     class function Low: TEnumUserResult; static; inline;
     /// <summary>

     ///   Converts an Integer to the enumeration type. Throws an EEnumUserResult
     ///   if this is not possible.
     /// </summary>
     class function Parse(AValue: Integer): TEnumUserResult; overload; inline; static;
     /// <summary>

     ///   Converts the enumeration type into the fitting integer const.
     /// </summary>
     function ToInteger: Integer; inline;
   end;

   /// <summary>
   ///   Record helper for TADFilter.
   /// </summary>
   TADFilterHelper = record helper for TADFilter
   strict private const
     cIntegerValues: array[TADFilter] of Integer =
      (TEMSADFilterConsts.FILTER_ALL,
       TEMSADFilterConsts.FILTER_TEMP_DUPLICATE_ACCOUNT,
       TEMSADFilterConsts.FILTER_NORMAL_ACCOUNT,
       TEMSADFilterConsts.FILTER_INTERDOMAIN_TRUST_ACCOUNT,
       TEMSADFilterConsts.FILTER_WORKSTATION_TRUST_ACCOUNT,
       TEMSADFilterConsts.FILTER_SERVER_TRUST_ACCOUNT);
   public
     /// <summary>
     ///   Converts the enumeration type into the fitting integer const.
     /// </summary>
     function ToInteger: Integer; overload; inline;
     /// <summary>
     ///   Converts an array of the enumeration type into the fitting integer const.
     /// </summary>
     class function ToInteger(const AValues: TArray<TADFilter>): Integer; overload; static; inline;
   end;

  /// <summary>
  /// This class provides the ability to enumerate users and groups in a
  /// active directory.
  /// </summary>
  TEnumAD = record
  strict private
    /// <summary>
    ///   Returns all groups for the Enum proc.
    /// </summary>
    class function InternalEnumGroups(const AServer: string; AEnumProc: TProc<string>): TEnumUserResult; static;
    /// <summary>
    ///   Returns all groups for the Enum proc.
    /// </summary>
    class function InternalEnumGroupsOfAUser(const AServer, AUser: string; AEnumProc: TProc<string>): TEnumUserResult; static;
    /// <summary>
    ///   Returns all filtered users and calls the anonymous method.
    /// </summary>
    class function InternalEnumUsers(const AServer: string; AFilter: Integer; AEnumProc: TProc<string>): TEnumUserResult; static;
    /// <summary>
    ///   Returns all users of a group for the Enum proc.
    /// </summary>
    class function InternalEnumUsersOfAGroup(const AServer, AGroup: string; AEnumProc: TProc<string>): TEnumUserResult; static;
    /// <summary>
    ///   Removes the Domain before a user: domain\user -> user
    /// </summary>
    class function RemoveDomain(const AUserName: string): string; static;
  public
    /// <summary>
    ///   Finds out if the credentials domain, user and password are valid.
    /// </summary>
    class function CredentialsValid(const ADomain, AUser, APassword: string; out AError: string): Boolean; static;
    /// <summary>
    ///   Returns all groups
    /// </summary>
    class function EnumGroups(const AServer: string; AGroups: TStrings): TEnumUserResult; static;
    /// <summary>
    ///   Returns all groups of a user.
    /// </summary>
    class function EnumGroupsOfAUser(const AServer, AUser: string; AUsers: TStrings): TEnumUserResult; static;
    /// <summary>
    ///   Returns all filtered users and calls the anonymous method.
    /// </summary>
    class function EnumUsers(const AServer: string; AFilter: TArray<TADFilter>; AEnumProc: TProc<string>): TEnumUserResult; overload; static;
    /// <summary>
    ///   Returns all filtered users and calls the anonymous method.
    /// </summary>
    class function EnumUsers(const AServer: string; AEnumProc: TProc<string>): TEnumUserResult; overload; static;
    /// <summary>
    ///   Returns all normal users
    /// </summary>
    class function EnumUsers(const AServer: string; AUsers: TStrings): TEnumUserResult; overload; static;
    /// <summary>
    ///   Returns all users of a group.
    /// </summary>
    class function EnumUsersOfAGroup(const AServer, AGroup: string; AEnumProc: TProc<string>): TEnumUserResult; overload; static;
    /// <summary>
    ///   Returns all users of a group.
    /// </summary>
    class function EnumUsersOfAGroup(const AServer, AGroup: string; AUsers: TStrings): TEnumUserResult; overload; static;
  end;

resourcestring
  SInvalidEnumUserResult = 'Invalid EnumUserResult [%d]!';

implementation

{ TEnumUserResultHelper }

class function TEnumUserResultHelper.High: TEnumUserResult;
begin
  Result := System.High(Result);
end;

class function TEnumUserResultHelper.Low: TEnumUserResult;
begin
  Result := System.Low(Result);
end;

class function TEnumUserResultHelper.Parse(AValue: Integer): TEnumUserResult;
begin
  for Result := Low to High do
  begin
    if Result.ToInteger = AValue then
      Exit;
  end;
  raise EEnumUserResult.CreateFmt(SInvalidEnumUserResult, [AValue]);
end;

function TEnumUserResultHelper.ToInteger: Integer;
begin
  Result := cIntegerValues[Self];
end;

{ TADFilterHelper }

function TADFilterHelper.ToInteger: Integer;
begin
  Result := cIntegerValues[Self];
end;

class function TADFilterHelper.ToInteger(const AValues: TArray<TADFilter>): Integer;
var
  iValue: TADFilter;
begin
  Result := 0;
  for iValue in AValues do
    Result := Result + iValue.ToInteger;
end;

{ TEnumAD }

class function TEnumAD.CredentialsValid(const ADomain, AUser, APassword: string; out AError: string): Boolean;
var
  iToken: THandle;
begin
  Result := LogonUser(PChar(AUser), PChar(ADomain), PChar(APassword), LOGON32_LOGON_BATCH,
    LOGON32_PROVIDER_DEFAULT, iToken);
  if Result then
  begin
    CloseHandle(iToken);
    AError := '';
  end
  else
    AError := SysErrorMessage(GetLastError);
end;

class function TEnumAD.EnumGroups(const AServer: string; AGroups: TStrings): TEnumUserResult;
begin
  Result := InternalEnumGroups(AServer,
    procedure(AGroupName: string)
    begin
      AGroups.Add(AGroupName);
    end);
end;

class function TEnumAD.EnumGroupsOfAUser(const AServer, AUser: string; AUsers: TStrings): TEnumUserResult;
begin
  Result := InternalEnumGroupsOfAUser(AServer, AUser,
    procedure(AGroupName: string)
    begin
      AUsers.Add(AGroupName);
    end);
end;

class function TEnumAD.EnumUsers(const AServer: string; AFilter: TArray<TADFilter>; AEnumProc: TProc<string>): TEnumUserResult;
begin
  Result := InternalEnumUsers(AServer, TADFilter.ToInteger(AFilter), AEnumProc);
end;

class function TEnumAD.EnumUsers(const AServer: string;  AEnumProc: TProc<string>): TEnumUserResult;
begin
  Result := InternalEnumUsers(AServer, 0, AEnumProc);
end;

class function TEnumAD.EnumUsers(const AServer: string; AUsers: TStrings): TEnumUserResult;
begin
  Result := InternalEnumUsers(AServer, 0,
    procedure(AUserName: string)
    begin
      AUsers.Add(AUserName);
    end);
end;

class function TEnumAD.EnumUsersOfAGroup(const AServer, AGroup: string; AEnumProc: TProc<string>): TEnumUserResult;
begin
  Result := InternalEnumUsersOfAGroup(AServer, AGroup, AEnumProc);
end;

class function TEnumAD.EnumUsersOfAGroup(const AServer, AGroup: string; AUsers: TStrings): TEnumUserResult;
begin
  Result := InternalEnumUsersOfAGroup(AServer, AGroup,
    procedure(AUserName: string)
    begin
      AUsers.Add(AUserName)
    end);
end;

class function TEnumAD.InternalEnumGroups(const AServer: string; AEnumProc: TProc<string>): TEnumUserResult;
var
  pGroupInfo: Pointer;
  pWork: Pointer;
  iEntriesRead: DWORD;
  iEntriesTotal: DWORD;
  iNetResult: DWORD;
  iCount: Integer;
begin
  pGroupInfo := nil;
  pWork := nil;
  try
    iNetResult := NetLocalGroupEnum(PChar(AServer), 0, pGroupInfo, TEMSADConsts.MAX_PREFERRED_LENGTH,
      iEntriesRead, iEntriesTotal, nil);
    if (iNetResult = TEMSADReturnConsts.NERR_SUCCESS) and (iEntriesRead > 0) then
    begin
      pWork := pGroupInfo;
      if Assigned(AEnumProc) then
      begin
        for iCount := 0 to iEntriesRead - 1 do
        begin
          AEnumProc(PLocalGroupInfo0(pGroupInfo)^.lgrpi0_name);
          Inc(NativeInt(pGroupInfo), SizeOf(Pointer));
        end;
      end;
    end;
  finally
    NetApiBufferFree(pWork)
  end;
  Result := TEnumUserResult.Parse(iNetResult);
end;

class function TEnumAD.InternalEnumGroupsOfAUser(const AServer, AUser: string; AEnumProc: TProc<string>): TEnumUserResult;
var
  pGroupInfo: Pointer;
  pWork: Pointer;
  iNetResult: DWORD;
  iEntriesRead: DWORD;
  iEntriesTotal: DWORD;
  iCount: Integer;
begin
  pGroupInfo := nil;
  pWork := nil;
  try
    iNetResult := NetUserGetLocalGroups(PChar(AServer), PChar(AUser),
      0, 0, pGroupInfo, TEMSADConsts.MAX_PREFERRED_LENGTH, iEntriesRead, iEntriesTotal);
    if (iNetResult = TEMSADReturnConsts.NERR_SUCCESS) and (iEntriesRead > 0) then
    begin
      pwork := pGroupInfo;
      if Assigned(AEnumProc) then
      begin
        for iCount := 0 to iEntriesRead - 1 do
        begin
          AEnumProc(PGroupInfo0(pGroupInfo)^.grpi0_name);
          Inc(NativeInt(pGroupInfo), SizeOf(Pointer));
        end;
      end;
    end;
  finally
    NetApiBufferFree(pWork);
  end;
  Result := TEnumUserResult.Parse(iNetResult);
end;

class function TEnumAD.InternalEnumUsers(const AServer: string; AFilter: Integer; AEnumProc: TProc<string>): TEnumUserResult;
var
  pUserInfo: Pointer;
  pWork: Pointer;
  iEntriesRead: DWORD;
  iEntriesTotal: DWORD;
  iNetResult: DWORD;
  iCount: Integer;
begin
  pUserInfo := nil;
  pWork := nil;
  try
    iNetResult := NetUserEnum(PChar(AServer), 0, AFilter, pUserInfo,
      TEMSADConsts.MAX_PREFERRED_LENGTH, iEntriesRead, iEntriesTotal, nil);
    if (iNetResult = TEMSADReturnConsts.NERR_SUCCESS) and (iEntriesRead > 0) then
    begin
      pWork := pUserInfo;
      if Assigned(AEnumProc) then
      begin
        for iCount := 0 to iEntriesRead - 1 do
        begin
          AEnumProc(PUserInfo1(pUserInfo)^.usri1_name);
          Inc(NativeInt(pUserInfo), Sizeof(Pointer));
        end;
      end;
    end;
  finally
    NetApiBufferFree(pWork);
  end;
  Result := TEnumUserResult.Parse(iNetResult);
end;

class function TEnumAD.InternalEnumUsersOfAGroup(const AServer, AGroup: string; AEnumProc: TProc<string>): TEnumUserResult;
var
  pInfo3: Pointer;
  pWork: Pointer;
  iEntriesRead: DWORD;
  iEntriesTotal: DWORD;
  iNetResult: NET_API_STATUS;
  iCount: Integer;
begin
  pInfo3 := nil;
  pWork := nil;
  try
    iNetResult := NetLocalGroupGetMembers(PChar(AServer), PChar(AGroup), 3, pInfo3, TEMSADConsts.MAX_PREFERRED_LENGTH,
      iEntriesRead, iEntriesTotal, nil);
    if (iNetResult = TEMSADReturnConsts.NERR_SUCCESS) and (iEntriesRead > 0) then
    begin
      pWork := pInfo3;
      if Assigned(AEnumProc) then
      begin
        for iCount := 0 to iEntriesRead - 1 do
        begin
          AEnumProc(RemoveDomain(PLocalGroupMembersInfo3(pInfo3)^.lgrmi3_domainandname));
          Inc(NativeInt(pInfo3), SizeOf(Pointer));
        end;
      end;
    end;
  finally
    NetApiBufferFree(pWork);
  end;
  Result := TEnumUserResult.Parse(iNetResult);
end;

class function TEnumAD.RemoveDomain(const AUserName: string): string;
const
  cDomainDeli = '\';
var
  iIndex: Integer;
begin
  iIndex := AUserName.IndexOf(cDomainDeli);
  if iIndex > -1 then
    Result := AUserName.Remove(0, iIndex + 1)
  else
    Result := AUserName;
end;

end.
