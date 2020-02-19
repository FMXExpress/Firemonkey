EMS Active Directory Console / Login Example

This example demonstrates how to implement custom Login and Signup endpoints 
using Active Directory Services (AD).

Projects
========

ADLoginPackage.dpk

This project registers and implements a custom EMS resource that implements 
custom Login and Signup endpoints using AD. The name of the custom resource 
is "ADLogin".

ADConsole.dpr

This project implements an EMS client to import / view AD users, view AD 
groups. And login, signup and delete a user using AD.

Using these projects
====================

Open ADLoginPackage.dpk. Run. This will start EMSDevServer and load 
ADLoginPackage.bpl.

Open ADConsole.dproj. Run. When the checkbox is checked the custom AD Login 
and Signup endpoints will be called instead of the standard endpoints in 
the Users resource.

The custom AD Login and Signup endpoints validate the user name and password 
against Windows users by calling WinApi.Windows.LogonUser. So, you will need 
to signup with valid Windows credentials. 

Custom Login
============

There are two ways to cause EMS to execute custom login and signup methods.

1.  The TEMSProvider.LoginResource property may be set to the name of a custom 
resource.  This custom resource must have a methods that matches the signatures
of the Users.LoginUser and Users.SignupUser endpoints.

By checking the box in CustomLoginClient, the TEMSProvider.LoginResource 
property is set to "ADLogin". 

2.  The emsserver.ini may include settings to redirect Users.LoginUser and 
Users.SignupUser to a custom resource.  Add settings to the [Server.Redirect] 
section as follows:

[Server.Redirect]
Users.SignupUser={"destination":"ADLogin"}
Users.LoginUser={"destination":"ADLogin"}

Connection to the Active Directory
===================================
This custom resource allows to connect the user management of the EMS server
to the Active Directory (AD). You have the possibility to logon into the AD 
directly. Furthermore you import new AD users and delete EMS users that don't
exist in the AD any longer. These methods can be filtered by AD groups. The 
head of the method declarations gives more details:

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

Adding packages to the EMS server
===================================
The Embarcadero wiki has many examples how to do this:
http://docwiki.embarcadero.com/RADStudio/en/Tutorial:_Implementing_Your_First_EMS_Resource
http://docwiki.embarcadero.com/RADStudio/en/EMS_Resource_Overview
http://docwiki.embarcadero.com/RADStudio/en/EMS_Server
http://docwiki.embarcadero.com/RADStudio/en/Running_the_EMS_Server_or_EMS_Console_Server_on_a_Developer_Environment
http://docwiki.embarcadero.com/RADStudio/en/Extending_the_EMS_Server

netapi32.dll
============
The connection to the AD runs through the netap32.dll. It uses the following functions:

function NetApiBufferFree; external netapi32 name 'NetApiBufferFree';
function NetLocalGroupEnum; external netapi32 name 'NetLocalGroupEnum';
function NetLocalGroupGetMembers; external netapi32 name 'NetLocalGroupGetMembers';
function NetUserEnum; external netapi32 name 'NetUserEnum';
function NetUserGetLocalGroups; external netapi32 name 'NetUserGetLocalGroups';

They run under Win32 and Win64.
  