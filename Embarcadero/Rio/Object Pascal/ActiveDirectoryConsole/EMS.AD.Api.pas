/// <summary>
///   This unit provides the necessary functions from the Windows API to
///   enumerate usera and groups.
/// </summary>
unit EMS.AD.Api;

interface

uses
  Winapi.Windows;

type
  NET_API_STATUS = DWORD;

  /// <summary>
  ///   Constants for the net api functions
  /// </summary>
  TEMSADConsts = record
  public const
    /// <summary>
    ///   A metavalue used in NetrWkstaUserEnum (section 3.2.4.3) and
    ///   NetrWkstaTransportEnum (section 3.2.4.4) method parameters to indicate
    ///   that the server MUST allocate the amount of memory required to return
    ///   all the requested data.
    /// </summary>
    MAX_PREFERRED_LENGTH = DWORD(-1);
  end;

  /// <summary>
  ///   Constants for the return values of the net api functions
  /// </summary>
  TEMSADReturnConsts = record
  public const
    /// <summary>
    ///   The operation completed successfully.
    /// </summary>
    NERR_Success = 0;
    /// <summary>
    /// The buffer is too small.
    /// </summary>
    NERR_BufTooSmall = 2132;
    /// <summary>
    /// This computer name is invalid.
    /// </summary>
    NERR_InvalidComputer = 2351;
  end;

  /// <summary>
  ///   Constants for the return values of the NetUserEnum function
  /// </summary>
  TEMSADFilterConsts = record
  public const
    /// <summary>
    ///   A value of zero indicates that all normal user, trust data, and
    ///   machine account data should be included.
    /// </summary>
    FILTER_ALL = 0;
    /// <summary>
    ///   Enumerates account data for users whose primary account is in another
    ///   domain. This account type provides user access to this domain, but not
    ///   to any domain that trusts this domain. The User Manager refers to this
    ///   account type as a local user account.
    /// </summary>
    FILTER_TEMP_DUPLICATE_ACCOUNT = $0001;
    /// <summary>
    ///   Enumerates normal user account data. This account type is associated
    ///   with a typical user.
    /// </summary>
    FILTER_NORMAL_ACCOUNT = $0002;
    /// <summary>
    ///   Enumerates interdomain trust account data. This account type is
    ///   associated with a trust account for a domain that trusts other domains.
    /// </summary>
    FILTER_INTERDOMAIN_TRUST_ACCOUNT = $0008;
    /// <summary>
    ///   Enumerates workstation or member server trust account data. This
    ///   account type is associated with a machine account for a computer that
    ///   is a member of the domain.
    /// </summary>
    FILTER_WORKSTATION_TRUST_ACCOUNT = $0010;
    /// <summary>
    ///   Enumerates member server machine account data. This account type is
    ///   associated with a computer account for a backup domain controller that
    ///   is a member of the domain.
    /// </summary>
    FILTER_SERVER_TRUST_ACCOUNT = $0020;
  end;

  /// <summary>
  ///   The LOCALGROUP_INFO_0 structure contains a local group name.
  /// </summary>
  LocalGroupInfo0 = record
    lgrpi0_name: LPWSTR;
  end;
  PLocalGroupInfo0 = ^LocalGroupInfo0;

  /// <summary>
  ///   The LOCALGROUP_MEMBERS_INFO_3 structure contains the account name and
  ///   domain name associated with a local group member.
  /// </summary>
  TLocalGroupMembersInfo3 = record
    lgrmi3_domainandname: LPWSTR;
  end;
  PLocalGroupMembersInfo3 = ^TLocalGroupMembersInfo3;

  /// <remarks>
  ///   The USER_INFO_1 structure contains information about a user account,
  ///   including account name, password data, privilege level, and the path to
  ///   the user's home directory.
  /// </remarks>
  TUserInfo1 = record
    usri1_name: LPWSTR;
    usri1_password: LPWSTR;
    usri1_password_age: DWORD;
    usri1_priv: DWORD;
    usri1_home_dir: LPWSTR;
    usri1_comment: LPWSTR;
    usri1_flags: DWORD;
    usri1_script_path: LPWSTR;
  end;
  PUserInfo1 = ^TUserInfo1;

  /// <summary>
  ///   The LOCALGROUP_USERS_INFO_0 structure contains local group member
  ///   information.
  /// </summary>
  TGroupInfo0 = record
    grpi0_name: LPWSTR;
  end;
  PGroupInfo0 = ^TGroupInfo0;

/// <summary>
///   The NetUserEnum function retrieves information about all user accounts on
///   a server.
/// </summary>
function NetUserEnum(servername: LPCWSTR; level: DWORD; filter: DWORD; var bufptr: Pointer;
  prefmaxlen: DWORD; var entriesred: DWORD; var totalentries: DWORD; resumehandle: PDWORD): NET_API_STATUS; stdcall;

/// <summary>
///   The NetApiBufferFree function frees the memory that the
///   NetApiBufferAllocate function allocates. Applications should also call
///   NetApiBufferFree to free the memory that other network management
///   functions use internally to return information.
/// </summary>
function NetApiBufferFree(Buffer: Pointer): NET_API_STATUS; stdcall;

/// <summary>
///   The NetLocalGroupEnum function returns information about each local group
///   account on the specified server.
/// </summary>
function NetLocalGroupEnum(servername: LPCWSTR; level: DWORD; var bufptr: Pointer;
  prefmaxlen: DWORD; var entriesread: DWORD; var totalentries: DWORD; resumehandle: PDWORD): NET_API_STATUS; stdcall;

/// <summary>
///   The NetLocalGroupGetMembers function retrieves a list of the members of a
///   particular local group in the security database, which is the security
///   accounts manager (SAM) database or, in the case of domain controllers,
///   the Active Directory. Local group members can be users or global groups.
/// </summary>
function NetLocalGroupGetMembers(servername: LPCWSTR; localgroupname: LPCWSTR; level: DWORD;
  var bufptr: Pointer; prefmaxlen: DWORD; var entriesread: DWORD; var totalentries: DWORD;
  resumehandle: PDWORD): NET_API_STATUS; stdcall;

/// <summary>
///   The NetUserGetLocalGroups function retrieves a list of local groups to
///   which a specified user belongs.
/// </summary>
function NetUserGetLocalGroups(servername: LPCWSTR; username: LPCWSTR; level: DWORD; flags: DWORD;
  var bufptr: Pointer; prefmaxlen: DWORD; var entriesread: DWORD; var totalentries: DWORD): NET_API_STATUS; stdcall;

implementation

const
  netapi32 = 'netapi32.dll';

function NetApiBufferFree; external netapi32 name 'NetApiBufferFree';
function NetLocalGroupEnum; external netapi32 name 'NetLocalGroupEnum';
function NetLocalGroupGetMembers; external netapi32 name 'NetLocalGroupGetMembers';
function NetUserEnum; external netapi32 name 'NetUserEnum';
function NetUserGetLocalGroups; external netapi32 name 'NetUserGetLocalGroups';

end.
