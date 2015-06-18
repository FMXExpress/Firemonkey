{File generated on 10.11.2014 23:17:18 by JavaImport for Android}

unit android.net.wifi;

interface

uses
  Androidapi.JNIBridge,
  Androidapi.JNI.Analytics,
  Androidapi.JNI.ApkExpansion,
  Androidapi.JNI.App,
  Androidapi.JNI.Dalvik,
  Androidapi.JNI.Embarcadero,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Hardware,
  Androidapi.JNI.InputMethodService,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Licensing,
  Androidapi.JNI.Location,
  Androidapi.JNI.Media,
  Androidapi.JNI.Net,
  Androidapi.JNI.OpenGL,
  Androidapi.JNI.Os,
  Androidapi.JNI.PlayServices,
  Androidapi.JNI.Provider,
  Androidapi.JNI.Support,
  Androidapi.JNI.Telephony,
  Androidapi.JNI.Util,
  Androidapi.JNI.VideoView,
  Androidapi.JNI.Webkit,
  Androidapi.JNI.Widget;

type
  {Class forward declarations}
  JWifiP2pDnsSdServiceInfo = interface; {android/net/wifi/p2p/nsd/WifiP2pDnsSdServiceInfo}
  JWifiP2pDnsSdServiceRequest = interface; {android/net/wifi/p2p/nsd/WifiP2pDnsSdServiceRequest}
  JWifiP2pServiceInfo = interface; {android/net/wifi/p2p/nsd/WifiP2pServiceInfo}
  JWifiP2pServiceRequest = interface; {android/net/wifi/p2p/nsd/WifiP2pServiceRequest}
  JWifiP2pUpnpServiceInfo = interface; {android/net/wifi/p2p/nsd/WifiP2pUpnpServiceInfo}
  JWifiP2pUpnpServiceRequest = interface; {android/net/wifi/p2p/nsd/WifiP2pUpnpServiceRequest}
  JWifiP2pConfig = interface; {android/net/wifi/p2p/WifiP2pConfig}
  JWifiP2pDevice = interface; {android/net/wifi/p2p/WifiP2pDevice}
  JWifiP2pDeviceList = interface; {android/net/wifi/p2p/WifiP2pDeviceList}
  JWifiP2pGroup = interface; {android/net/wifi/p2p/WifiP2pGroup}
  JWifiP2pInfo = interface; {android/net/wifi/p2p/WifiP2pInfo}
  JWifiP2pManager_ActionListener = interface; {android/net/wifi/p2p/WifiP2pManager$ActionListener}
  JWifiP2pManager_Channel = interface; {android/net/wifi/p2p/WifiP2pManager$Channel}
  JWifiP2pManager_ChannelListener = interface; {android/net/wifi/p2p/WifiP2pManager$ChannelListener}
  JWifiP2pManager_ConnectionInfoListener = interface; {android/net/wifi/p2p/WifiP2pManager$ConnectionInfoListener}
  JWifiP2pManager_DnsSdServiceResponseListener = interface; {android/net/wifi/p2p/WifiP2pManager$DnsSdServiceResponseListener}
  JWifiP2pManager_DnsSdTxtRecordListener = interface; {android/net/wifi/p2p/WifiP2pManager$DnsSdTxtRecordListener}
  JWifiP2pManager_GroupInfoListener = interface; {android/net/wifi/p2p/WifiP2pManager$GroupInfoListener}
  JWifiP2pManager_PeerListListener = interface; {android/net/wifi/p2p/WifiP2pManager$PeerListListener}
  JWifiP2pManager_ServiceResponseListener = interface; {android/net/wifi/p2p/WifiP2pManager$ServiceResponseListener}
  JWifiP2pManager_UpnpServiceResponseListener = interface; {android/net/wifi/p2p/WifiP2pManager$UpnpServiceResponseListener}
  JWifiP2pManager = interface; {android/net/wifi/p2p/WifiP2pManager}
  JScanResult = interface; {android/net/wifi/ScanResult}
  JSupplicantState = interface; {android/net/wifi/SupplicantState}
  JWifiConfiguration_AuthAlgorithm = interface; {android/net/wifi/WifiConfiguration$AuthAlgorithm}
  JWifiConfiguration_GroupCipher = interface; {android/net/wifi/WifiConfiguration$GroupCipher}
  JWifiConfiguration_KeyMgmt = interface; {android/net/wifi/WifiConfiguration$KeyMgmt}
  JWifiConfiguration_PairwiseCipher = interface; {android/net/wifi/WifiConfiguration$PairwiseCipher}
  JWifiConfiguration_Protocol = interface; {android/net/wifi/WifiConfiguration$Protocol}
  JWifiConfiguration_Status = interface; {android/net/wifi/WifiConfiguration$Status}
  JWifiConfiguration = interface; {android/net/wifi/WifiConfiguration}
  JWifiEnterpriseConfig_Eap = interface; {android/net/wifi/WifiEnterpriseConfig$Eap}
  JWifiEnterpriseConfig_Phase2 = interface; {android/net/wifi/WifiEnterpriseConfig$Phase2}
  JWifiEnterpriseConfig = interface; {android/net/wifi/WifiEnterpriseConfig}
  JWifiInfo = interface; {android/net/wifi/WifiInfo}
  JWifiManager_MulticastLock = interface; {android/net/wifi/WifiManager$MulticastLock}
  JWifiManager_WifiLock = interface; {android/net/wifi/WifiManager$WifiLock}
  JWifiManager_WpsCallback = interface; {android/net/wifi/WifiManager$WpsCallback}
  JWifiManager = interface; {android/net/wifi/WifiManager}
  JWpsInfo = interface; {android/net/wifi/WpsInfo}

  JWifiP2pDnsSdServiceInfoClass = interface(JObjectClass)
    ['{E576C7D7-51DA-4740-885C-A270C3D22CE4}']
    {Methods}
    function newInstance(instanceName: JString; serviceType: JString; txtMap: JMap): JWifiP2pDnsSdServiceInfo; cdecl;
  end;

  [JavaSignature('android/net/wifi/p2p/nsd/WifiP2pDnsSdServiceInfo')]
  JWifiP2pDnsSdServiceInfo = interface(JObject)
    ['{197F4B6F-14C8-4B6C-99F7-2BB639261226}']
  end;

  TJWifiP2pDnsSdServiceInfo = class(TJavaGenericImport<JWifiP2pDnsSdServiceInfoClass, JWifiP2pDnsSdServiceInfo>)
  end;

  JWifiP2pDnsSdServiceRequestClass = interface(JObjectClass)
    ['{F2645300-0EC0-4390-9A0A-BEE3B3F5E8CC}']
    {Methods}
    function newInstance: JWifiP2pDnsSdServiceRequest; cdecl; overload;
    function newInstance(serviceType: JString): JWifiP2pDnsSdServiceRequest; cdecl; overload;
    function newInstance(instanceName: JString; serviceType: JString): JWifiP2pDnsSdServiceRequest; cdecl; overload;
  end;

  [JavaSignature('android/net/wifi/p2p/nsd/WifiP2pDnsSdServiceRequest')]
  JWifiP2pDnsSdServiceRequest = interface(JObject)
    ['{A62D9F8D-E0C4-4F62-A04F-961D6DFF3032}']
  end;

  TJWifiP2pDnsSdServiceRequest = class(TJavaGenericImport<JWifiP2pDnsSdServiceRequestClass, JWifiP2pDnsSdServiceRequest>)
  end;

  JWifiP2pServiceInfoClass = interface(JObjectClass)
    ['{6250FD41-D73A-45DB-B334-75E8151145FA}']
    {Property methods}
    function _GetSERVICE_TYPE_ALL: Integer;
    function _GetSERVICE_TYPE_BONJOUR: Integer;
    function _GetSERVICE_TYPE_UPNP: Integer;
    function _GetSERVICE_TYPE_VENDOR_SPECIFIC: Integer;
    {Properties}
    property SERVICE_TYPE_ALL: Integer read _GetSERVICE_TYPE_ALL;
    property SERVICE_TYPE_BONJOUR: Integer read _GetSERVICE_TYPE_BONJOUR;
    property SERVICE_TYPE_UPNP: Integer read _GetSERVICE_TYPE_UPNP;
    property SERVICE_TYPE_VENDOR_SPECIFIC: Integer read _GetSERVICE_TYPE_VENDOR_SPECIFIC;
  end;

  [JavaSignature('android/net/wifi/p2p/nsd/WifiP2pServiceInfo')]
  JWifiP2pServiceInfo = interface(JObject)
    ['{D4B1F93A-AC4D-4B6F-BA01-14AA5CC16FE5}']
    {Methods}
    function describeContents: Integer; cdecl;
    function equals(o: JObject): Boolean; cdecl;
    function hashCode: Integer; cdecl;
    procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
  end;

  TJWifiP2pServiceInfo = class(TJavaGenericImport<JWifiP2pServiceInfoClass, JWifiP2pServiceInfo>)
  end;

  JWifiP2pServiceRequestClass = interface(JObjectClass)
    ['{2CB80990-900B-438B-BB78-E79561BAACAD}']
    {Methods}
    function newInstance(protocolType: Integer; queryData: JString): JWifiP2pServiceRequest; cdecl; overload;
    function newInstance(protocolType: Integer): JWifiP2pServiceRequest; cdecl; overload;
  end;

  [JavaSignature('android/net/wifi/p2p/nsd/WifiP2pServiceRequest')]
  JWifiP2pServiceRequest = interface(JObject)
    ['{E7932E72-2365-49A5-9A73-57FAE6EDCFDE}']
    {Methods}
    function describeContents: Integer; cdecl;
    function equals(o: JObject): Boolean; cdecl;
    function hashCode: Integer; cdecl;
    procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
  end;

  TJWifiP2pServiceRequest = class(TJavaGenericImport<JWifiP2pServiceRequestClass, JWifiP2pServiceRequest>)
  end;

  JWifiP2pUpnpServiceInfoClass = interface(JObjectClass)
    ['{E05770D4-C307-4ED5-8EED-ACDB5F3BEC9C}']
    {Methods}
    function newInstance(uuid: JString; device: JString; services: JList): JWifiP2pUpnpServiceInfo; cdecl;
  end;

  [JavaSignature('android/net/wifi/p2p/nsd/WifiP2pUpnpServiceInfo')]
  JWifiP2pUpnpServiceInfo = interface(JObject)
    ['{CCFC5210-85BE-4F4B-851B-6A65A8AD0B56}']
  end;

  TJWifiP2pUpnpServiceInfo = class(TJavaGenericImport<JWifiP2pUpnpServiceInfoClass, JWifiP2pUpnpServiceInfo>)
  end;

  JWifiP2pUpnpServiceRequestClass = interface(JObjectClass)
    ['{AEF8E7E8-21A1-4A61-8316-306381F35DA3}']
    {Methods}
    function newInstance: JWifiP2pUpnpServiceRequest; cdecl; overload;
    function newInstance(st: JString): JWifiP2pUpnpServiceRequest; cdecl; overload;
  end;

  [JavaSignature('android/net/wifi/p2p/nsd/WifiP2pUpnpServiceRequest')]
  JWifiP2pUpnpServiceRequest = interface(JObject)
    ['{A3C9B6E1-6360-4D5E-AF16-DC03DF674FDB}']
  end;

  TJWifiP2pUpnpServiceRequest = class(TJavaGenericImport<JWifiP2pUpnpServiceRequestClass, JWifiP2pUpnpServiceRequest>)
  end;

  JWifiP2pConfigClass = interface(JObjectClass)
    ['{451B79A8-57CF-4DA5-B31F-C43B5DF7521F}']
    {Property methods}
    function _GetCREATOR: JParcelable_Creator;
    procedure _SetCREATOR(Value: JParcelable_Creator);
    {Methods}
    function init: JWifiP2pConfig; cdecl; overload;
    function init(source: JWifiP2pConfig): JWifiP2pConfig; cdecl; overload;
    {Properties}
    property CREATOR: JParcelable_Creator read _GetCREATOR write _SetCREATOR;
  end;

  [JavaSignature('android/net/wifi/p2p/WifiP2pConfig')]
  JWifiP2pConfig = interface(JObject)
    ['{390C47E5-5662-4CD2-AAD2-5D605EFF0052}']
    {Property methods}
    function _GetdeviceAddress: JString;
    procedure _SetdeviceAddress(Value: JString);
    function _GetgroupOwnerIntent: Integer;
    procedure _SetgroupOwnerIntent(Value: Integer);
    function _Getwps: JWpsInfo;
    procedure _Setwps(Value: JWpsInfo);
    {Methods}
    function describeContents: Integer; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
    {Properties}
    property deviceAddress: JString read _GetdeviceAddress write _SetdeviceAddress;
    property groupOwnerIntent: Integer read _GetgroupOwnerIntent write _SetgroupOwnerIntent;
    property wps: JWpsInfo read _Getwps write _Setwps;
  end;

  TJWifiP2pConfig = class(TJavaGenericImport<JWifiP2pConfigClass, JWifiP2pConfig>)
  end;

  JWifiP2pDeviceClass = interface(JObjectClass)
    ['{B240BADE-6CFF-4517-8173-BE8CAC7991C0}']
    {Property methods}
    function _GetAVAILABLE: Integer;
    function _GetCONNECTED: Integer;
    function _GetCREATOR: JParcelable_Creator;
    procedure _SetCREATOR(Value: JParcelable_Creator);
    function _GetFAILED: Integer;
    function _GetINVITED: Integer;
    function _GetUNAVAILABLE: Integer;
    {Methods}
    function init: JWifiP2pDevice; cdecl; overload;
    function init(source: JWifiP2pDevice): JWifiP2pDevice; cdecl; overload;
    {Properties}
    property AVAILABLE: Integer read _GetAVAILABLE;
    property CONNECTED: Integer read _GetCONNECTED;
    property CREATOR: JParcelable_Creator read _GetCREATOR write _SetCREATOR;
    property FAILED: Integer read _GetFAILED;
    property INVITED: Integer read _GetINVITED;
    property UNAVAILABLE: Integer read _GetUNAVAILABLE;
  end;

  [JavaSignature('android/net/wifi/p2p/WifiP2pDevice')]
  JWifiP2pDevice = interface(JObject)
    ['{E357D730-D920-41DA-8345-ADFC1945EEC6}']
    {Property methods}
    function _GetdeviceAddress: JString;
    procedure _SetdeviceAddress(Value: JString);
    function _GetdeviceName: JString;
    procedure _SetdeviceName(Value: JString);
    function _GetprimaryDeviceType: JString;
    procedure _SetprimaryDeviceType(Value: JString);
    function _GetsecondaryDeviceType: JString;
    procedure _SetsecondaryDeviceType(Value: JString);
    function _Getstatus: Integer;
    procedure _Setstatus(Value: Integer);
    {Methods}
    function describeContents: Integer; cdecl;
    function equals(obj: JObject): Boolean; cdecl;
    function isGroupOwner: Boolean; cdecl;
    function isServiceDiscoveryCapable: Boolean; cdecl;
    function toString: JString; cdecl;
    function wpsDisplaySupported: Boolean; cdecl;
    function wpsKeypadSupported: Boolean; cdecl;
    function wpsPbcSupported: Boolean; cdecl;
    procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
    {Properties}
    property deviceAddress: JString read _GetdeviceAddress write _SetdeviceAddress;
    property deviceName: JString read _GetdeviceName write _SetdeviceName;
    property primaryDeviceType: JString read _GetprimaryDeviceType write _SetprimaryDeviceType;
    property secondaryDeviceType: JString read _GetsecondaryDeviceType write _SetsecondaryDeviceType;
    property status: Integer read _Getstatus write _Setstatus;
  end;

  TJWifiP2pDevice = class(TJavaGenericImport<JWifiP2pDeviceClass, JWifiP2pDevice>)
  end;

  JWifiP2pDeviceListClass = interface(JObjectClass)
    ['{EBC3B15B-1CDE-4ECC-82F0-7589D86652D7}']
    {Property methods}
    function _GetCREATOR: JParcelable_Creator;
    procedure _SetCREATOR(Value: JParcelable_Creator);
    {Methods}
    function init: JWifiP2pDeviceList; cdecl; overload;
    function init(source: JWifiP2pDeviceList): JWifiP2pDeviceList; cdecl; overload;
    {Properties}
    property CREATOR: JParcelable_Creator read _GetCREATOR write _SetCREATOR;
  end;

  [JavaSignature('android/net/wifi/p2p/WifiP2pDeviceList')]
  JWifiP2pDeviceList = interface(JObject)
    ['{48C14510-241E-43BE-B8B8-92502C984451}']
    {Methods}
    function describeContents: Integer; cdecl;
    function get(deviceAddress: JString): JWifiP2pDevice; cdecl;
    function getDeviceList: JCollection; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
  end;

  TJWifiP2pDeviceList = class(TJavaGenericImport<JWifiP2pDeviceListClass, JWifiP2pDeviceList>)
  end;

  JWifiP2pGroupClass = interface(JObjectClass)
    ['{2B2F9144-DE6F-4AEF-AD57-CF8FB3A680BA}']
    {Property methods}
    function _GetCREATOR: JParcelable_Creator;
    procedure _SetCREATOR(Value: JParcelable_Creator);
    {Methods}
    function init: JWifiP2pGroup; cdecl; overload;
    function init(source: JWifiP2pGroup): JWifiP2pGroup; cdecl; overload;
    {Properties}
    property CREATOR: JParcelable_Creator read _GetCREATOR write _SetCREATOR;
  end;

  [JavaSignature('android/net/wifi/p2p/WifiP2pGroup')]
  JWifiP2pGroup = interface(JObject)
    ['{90EB1BF0-9D61-400E-8D83-AD1E829CC3E4}']
    {Methods}
    function describeContents: Integer; cdecl;
    function getClientList: JCollection; cdecl;
    function getInterface: JString; cdecl;
    function getNetworkName: JString; cdecl;
    function getOwner: JWifiP2pDevice; cdecl;
    function getPassphrase: JString; cdecl;
    function isGroupOwner: Boolean; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
  end;

  TJWifiP2pGroup = class(TJavaGenericImport<JWifiP2pGroupClass, JWifiP2pGroup>)
  end;

  JWifiP2pInfoClass = interface(JObjectClass)
    ['{8B27FF80-24B4-490D-B90D-75579D5FEF84}']
    {Property methods}
    function _GetCREATOR: JParcelable_Creator;
    procedure _SetCREATOR(Value: JParcelable_Creator);
    {Methods}
    function init: JWifiP2pInfo; cdecl; overload;
    function init(source: JWifiP2pInfo): JWifiP2pInfo; cdecl; overload;
    {Properties}
    property CREATOR: JParcelable_Creator read _GetCREATOR write _SetCREATOR;
  end;

  [JavaSignature('android/net/wifi/p2p/WifiP2pInfo')]
  JWifiP2pInfo = interface(JObject)
    ['{4D0FB4AA-AFB9-4432-A39C-8275EF87BD51}']
    {Property methods}
    function _GetgroupFormed: Boolean;
    procedure _SetgroupFormed(Value: Boolean);
    function _GetgroupOwnerAddress: JObject {java/net/InetAddress};
    procedure _SetgroupOwnerAddress(Value: JObject {java/net/InetAddress});
    function _GetisGroupOwner: Boolean;
    procedure _SetisGroupOwner(Value: Boolean);
    {Methods}
    function describeContents: Integer; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
    {Properties}
    property groupFormed: Boolean read _GetgroupFormed write _SetgroupFormed;
    property groupOwnerAddress: JObject {java/net/InetAddress} read _GetgroupOwnerAddress write _SetgroupOwnerAddress;
    property isGroupOwner: Boolean read _GetisGroupOwner write _SetisGroupOwner;
  end;

  TJWifiP2pInfo = class(TJavaGenericImport<JWifiP2pInfoClass, JWifiP2pInfo>)
  end;

  JWifiP2pManager_ActionListenerClass = interface(IJavaClass)
    ['{5E2BC383-D577-4D56-9AD8-3A8000C99562}']
  end;

  [JavaSignature('android/net/wifi/p2p/WifiP2pManager$ActionListener')]
  JWifiP2pManager_ActionListener = interface(IJavaInstance)
    ['{D55237E8-5659-4105-9C6D-683ED58CB740}']
    {Methods}
    procedure onFailure(Param0: Integer); cdecl;
    procedure onSuccess; cdecl;
  end;

  TJWifiP2pManager_ActionListener = class(TJavaGenericImport<JWifiP2pManager_ActionListenerClass, JWifiP2pManager_ActionListener>)
  end;

  JWifiP2pManager_ChannelClass = interface(JObjectClass)
    ['{FF4630B3-98A0-4FD4-862C-F3C9A4B15ED6}']
  end;

  [JavaSignature('android/net/wifi/p2p/WifiP2pManager$Channel')]
  JWifiP2pManager_Channel = interface(JObject)
    ['{C22AEE93-C995-419B-891C-3CE413FBD6E2}']
  end;

  TJWifiP2pManager_Channel = class(TJavaGenericImport<JWifiP2pManager_ChannelClass, JWifiP2pManager_Channel>)
  end;

  JWifiP2pManager_ChannelListenerClass = interface(IJavaClass)
    ['{71DCCE8B-A69F-47E2-A72C-0D68C82DAAC6}']
  end;

  [JavaSignature('android/net/wifi/p2p/WifiP2pManager$ChannelListener')]
  JWifiP2pManager_ChannelListener = interface(IJavaInstance)
    ['{E428E41A-6399-4DC1-A698-9C829442654D}']
    {Methods}
    procedure onChannelDisconnected; cdecl;
  end;

  TJWifiP2pManager_ChannelListener = class(TJavaGenericImport<JWifiP2pManager_ChannelListenerClass, JWifiP2pManager_ChannelListener>)
  end;

  JWifiP2pManager_ConnectionInfoListenerClass = interface(IJavaClass)
    ['{D2A6829A-4F8F-4029-9F40-E6B48AE5A60C}']
  end;

  [JavaSignature('android/net/wifi/p2p/WifiP2pManager$ConnectionInfoListener')]
  JWifiP2pManager_ConnectionInfoListener = interface(IJavaInstance)
    ['{AB42F2BD-E3D8-4A75-93C1-F959A6DC41F8}']
    {Methods}
    procedure onConnectionInfoAvailable(Param0: JWifiP2pInfo); cdecl;
  end;

  TJWifiP2pManager_ConnectionInfoListener = class(TJavaGenericImport<JWifiP2pManager_ConnectionInfoListenerClass, JWifiP2pManager_ConnectionInfoListener>)
  end;

  JWifiP2pManager_DnsSdServiceResponseListenerClass = interface(IJavaClass)
    ['{84F699AA-83F8-4FD7-BC92-C6AB1DB7D254}']
  end;

  [JavaSignature('android/net/wifi/p2p/WifiP2pManager$DnsSdServiceResponseListener')]
  JWifiP2pManager_DnsSdServiceResponseListener = interface(IJavaInstance)
    ['{12300DD9-6FD1-4F92-816A-FB84F94ECC36}']
    {Methods}
    procedure onDnsSdServiceAvailable(Param0: JString; Param1: JString; Param2: JWifiP2pDevice); cdecl;
  end;

  TJWifiP2pManager_DnsSdServiceResponseListener = class(TJavaGenericImport<JWifiP2pManager_DnsSdServiceResponseListenerClass, JWifiP2pManager_DnsSdServiceResponseListener>)
  end;

  JWifiP2pManager_DnsSdTxtRecordListenerClass = interface(IJavaClass)
    ['{CBD88432-A3D9-46C5-9C11-C287EE178A68}']
  end;

  [JavaSignature('android/net/wifi/p2p/WifiP2pManager$DnsSdTxtRecordListener')]
  JWifiP2pManager_DnsSdTxtRecordListener = interface(IJavaInstance)
    ['{3A329D5B-8D08-4A6A-83BF-4DCB8E05C20B}']
    {Methods}
    procedure onDnsSdTxtRecordAvailable(Param0: JString; Param1: JMap; Param2: JWifiP2pDevice); cdecl;
  end;

  TJWifiP2pManager_DnsSdTxtRecordListener = class(TJavaGenericImport<JWifiP2pManager_DnsSdTxtRecordListenerClass, JWifiP2pManager_DnsSdTxtRecordListener>)
  end;

  JWifiP2pManager_GroupInfoListenerClass = interface(IJavaClass)
    ['{2084F104-5F66-4B07-863A-7998A271ECF4}']
  end;

  [JavaSignature('android/net/wifi/p2p/WifiP2pManager$GroupInfoListener')]
  JWifiP2pManager_GroupInfoListener = interface(IJavaInstance)
    ['{2CE23C85-A108-420F-A17C-4E4239B978EE}']
    {Methods}
    procedure onGroupInfoAvailable(Param0: JWifiP2pGroup); cdecl;
  end;

  TJWifiP2pManager_GroupInfoListener = class(TJavaGenericImport<JWifiP2pManager_GroupInfoListenerClass, JWifiP2pManager_GroupInfoListener>)
  end;

  JWifiP2pManager_PeerListListenerClass = interface(IJavaClass)
    ['{4280BD19-FCD0-42B3-8863-340DECB395C4}']
  end;

  [JavaSignature('android/net/wifi/p2p/WifiP2pManager$PeerListListener')]
  JWifiP2pManager_PeerListListener = interface(IJavaInstance)
    ['{F2886057-300B-4F05-A301-44BF647A7CAF}']
    {Methods}
    procedure onPeersAvailable(Param0: JWifiP2pDeviceList); cdecl;
  end;

  TJWifiP2pManager_PeerListListener = class(TJavaGenericImport<JWifiP2pManager_PeerListListenerClass, JWifiP2pManager_PeerListListener>)
  end;

  JWifiP2pManager_ServiceResponseListenerClass = interface(IJavaClass)
    ['{EB2C634D-ABC4-4FFE-8DF9-4FC0FF64F718}']
  end;

  [JavaSignature('android/net/wifi/p2p/WifiP2pManager$ServiceResponseListener')]
  JWifiP2pManager_ServiceResponseListener = interface(IJavaInstance)
    ['{3D70BD28-361B-4AF2-AD10-CB7E1BB6FDC3}']
    {Methods}
    procedure onServiceAvailable(Param0: Integer; Param1: TJavaArray<Byte>; Param2: JWifiP2pDevice); cdecl;
  end;

  TJWifiP2pManager_ServiceResponseListener = class(TJavaGenericImport<JWifiP2pManager_ServiceResponseListenerClass, JWifiP2pManager_ServiceResponseListener>)
  end;

  JWifiP2pManager_UpnpServiceResponseListenerClass = interface(IJavaClass)
    ['{8B5D6009-39D9-4FC1-8084-BC68490F2733}']
  end;

  [JavaSignature('android/net/wifi/p2p/WifiP2pManager$UpnpServiceResponseListener')]
  JWifiP2pManager_UpnpServiceResponseListener = interface(IJavaInstance)
    ['{53734A45-3406-4DF6-816B-AD9EA918F8F1}']
    {Methods}
    procedure onUpnpServiceAvailable(Param0: JList; Param1: JWifiP2pDevice); cdecl;
  end;

  TJWifiP2pManager_UpnpServiceResponseListener = class(TJavaGenericImport<JWifiP2pManager_UpnpServiceResponseListenerClass, JWifiP2pManager_UpnpServiceResponseListener>)
  end;

  JWifiP2pManagerClass = interface(JObjectClass)
    ['{975C028F-B515-4C24-B362-14E6477F7463}']
    {Property methods}
    function _GetBUSY: Integer;
    function _GetERROR: Integer;
    function _GetEXTRA_DISCOVERY_STATE: JString;
    function _GetEXTRA_NETWORK_INFO: JString;
    function _GetEXTRA_P2P_DEVICE_LIST: JString;
    function _GetEXTRA_WIFI_P2P_DEVICE: JString;
    function _GetEXTRA_WIFI_P2P_GROUP: JString;
    function _GetEXTRA_WIFI_P2P_INFO: JString;
    function _GetEXTRA_WIFI_STATE: JString;
    function _GetNO_SERVICE_REQUESTS: Integer;
    function _GetP2P_UNSUPPORTED: Integer;
    function _GetWIFI_P2P_CONNECTION_CHANGED_ACTION: JString;
    function _GetWIFI_P2P_DISCOVERY_CHANGED_ACTION: JString;
    function _GetWIFI_P2P_DISCOVERY_STARTED: Integer;
    function _GetWIFI_P2P_DISCOVERY_STOPPED: Integer;
    function _GetWIFI_P2P_PEERS_CHANGED_ACTION: JString;
    function _GetWIFI_P2P_STATE_CHANGED_ACTION: JString;
    function _GetWIFI_P2P_STATE_DISABLED: Integer;
    function _GetWIFI_P2P_STATE_ENABLED: Integer;
    function _GetWIFI_P2P_THIS_DEVICE_CHANGED_ACTION: JString;
    {Properties}
    property BUSY: Integer read _GetBUSY;
    property ERROR: Integer read _GetERROR;
    property EXTRA_DISCOVERY_STATE: JString read _GetEXTRA_DISCOVERY_STATE;
    property EXTRA_NETWORK_INFO: JString read _GetEXTRA_NETWORK_INFO;
    property EXTRA_P2P_DEVICE_LIST: JString read _GetEXTRA_P2P_DEVICE_LIST;
    property EXTRA_WIFI_P2P_DEVICE: JString read _GetEXTRA_WIFI_P2P_DEVICE;
    property EXTRA_WIFI_P2P_GROUP: JString read _GetEXTRA_WIFI_P2P_GROUP;
    property EXTRA_WIFI_P2P_INFO: JString read _GetEXTRA_WIFI_P2P_INFO;
    property EXTRA_WIFI_STATE: JString read _GetEXTRA_WIFI_STATE;
    property NO_SERVICE_REQUESTS: Integer read _GetNO_SERVICE_REQUESTS;
    property P2P_UNSUPPORTED: Integer read _GetP2P_UNSUPPORTED;
    property WIFI_P2P_CONNECTION_CHANGED_ACTION: JString read _GetWIFI_P2P_CONNECTION_CHANGED_ACTION;
    property WIFI_P2P_DISCOVERY_CHANGED_ACTION: JString read _GetWIFI_P2P_DISCOVERY_CHANGED_ACTION;
    property WIFI_P2P_DISCOVERY_STARTED: Integer read _GetWIFI_P2P_DISCOVERY_STARTED;
    property WIFI_P2P_DISCOVERY_STOPPED: Integer read _GetWIFI_P2P_DISCOVERY_STOPPED;
    property WIFI_P2P_PEERS_CHANGED_ACTION: JString read _GetWIFI_P2P_PEERS_CHANGED_ACTION;
    property WIFI_P2P_STATE_CHANGED_ACTION: JString read _GetWIFI_P2P_STATE_CHANGED_ACTION;
    property WIFI_P2P_STATE_DISABLED: Integer read _GetWIFI_P2P_STATE_DISABLED;
    property WIFI_P2P_STATE_ENABLED: Integer read _GetWIFI_P2P_STATE_ENABLED;
    property WIFI_P2P_THIS_DEVICE_CHANGED_ACTION: JString read _GetWIFI_P2P_THIS_DEVICE_CHANGED_ACTION;
  end;

  [JavaSignature('android/net/wifi/p2p/WifiP2pManager')]
  JWifiP2pManager = interface(JObject)
    ['{187FB938-8749-435C-83E2-0509F119C988}']
    {Methods}
    procedure addLocalService(c: JWifiP2pManager_Channel; servInfo: JWifiP2pServiceInfo; listener: JWifiP2pManager_ActionListener); cdecl;
    procedure addServiceRequest(c: JWifiP2pManager_Channel; req: JWifiP2pServiceRequest; listener: JWifiP2pManager_ActionListener); cdecl;
    procedure cancelConnect(c: JWifiP2pManager_Channel; listener: JWifiP2pManager_ActionListener); cdecl;
    procedure clearLocalServices(c: JWifiP2pManager_Channel; listener: JWifiP2pManager_ActionListener); cdecl;
    procedure clearServiceRequests(c: JWifiP2pManager_Channel; listener: JWifiP2pManager_ActionListener); cdecl;
    procedure connect(c: JWifiP2pManager_Channel; config: JWifiP2pConfig; listener: JWifiP2pManager_ActionListener); cdecl;
    procedure createGroup(c: JWifiP2pManager_Channel; listener: JWifiP2pManager_ActionListener); cdecl;
    procedure discoverPeers(c: JWifiP2pManager_Channel; listener: JWifiP2pManager_ActionListener); cdecl;
    procedure discoverServices(c: JWifiP2pManager_Channel; listener: JWifiP2pManager_ActionListener); cdecl;
    function initialize(srcContext: JContext; srcLooper: JLooper; listener: JWifiP2pManager_ChannelListener): JWifiP2pManager_Channel; cdecl;
    procedure removeGroup(c: JWifiP2pManager_Channel; listener: JWifiP2pManager_ActionListener); cdecl;
    procedure removeLocalService(c: JWifiP2pManager_Channel; servInfo: JWifiP2pServiceInfo; listener: JWifiP2pManager_ActionListener); cdecl;
    procedure removeServiceRequest(c: JWifiP2pManager_Channel; req: JWifiP2pServiceRequest; listener: JWifiP2pManager_ActionListener); cdecl;
    procedure requestConnectionInfo(c: JWifiP2pManager_Channel; listener: JWifiP2pManager_ConnectionInfoListener); cdecl;
    procedure requestGroupInfo(c: JWifiP2pManager_Channel; listener: JWifiP2pManager_GroupInfoListener); cdecl;
    procedure requestPeers(c: JWifiP2pManager_Channel; listener: JWifiP2pManager_PeerListListener); cdecl;
    procedure setDnsSdResponseListeners(c: JWifiP2pManager_Channel; servListener: JWifiP2pManager_DnsSdServiceResponseListener; txtListener: JWifiP2pManager_DnsSdTxtRecordListener); cdecl;
    procedure setServiceResponseListener(c: JWifiP2pManager_Channel; listener: JWifiP2pManager_ServiceResponseListener); cdecl;
    procedure setUpnpServiceResponseListener(c: JWifiP2pManager_Channel; listener: JWifiP2pManager_UpnpServiceResponseListener); cdecl;
    procedure stopPeerDiscovery(c: JWifiP2pManager_Channel; listener: JWifiP2pManager_ActionListener); cdecl;
  end;

  TJWifiP2pManager = class(TJavaGenericImport<JWifiP2pManagerClass, JWifiP2pManager>)
  end;

  JScanResultClass = interface(JObjectClass)
    ['{839ACDC4-2C2E-4D3A-B03E-8B4F87F0711A}']
  end;

  [JavaSignature('android/net/wifi/ScanResult')]
  JScanResult = interface(JObject)
    ['{E2821F9B-F2B8-4575-BF04-03247AB07F9E}']
    {Property methods}
    function _GetBSSID: JString;
    procedure _SetBSSID(Value: JString);
    function _Getcapabilities: JString;
    procedure _Setcapabilities(Value: JString);
    function _Getfrequency: Integer;
    procedure _Setfrequency(Value: Integer);
    function _Getlevel: Integer;
    procedure _Setlevel(Value: Integer);
    function _GetSSID: JString;
    procedure _SetSSID(Value: JString);
    function _Gettimestamp: Int64;
    procedure _Settimestamp(Value: Int64);
    {Methods}
    function describeContents: Integer; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
    {Properties}
    property BSSID: JString read _GetBSSID write _SetBSSID;
    property capabilities: JString read _Getcapabilities write _Setcapabilities;
    property frequency: Integer read _Getfrequency write _Setfrequency;
    property level: Integer read _Getlevel write _Setlevel;
    property SSID: JString read _GetSSID write _SetSSID;
    property timestamp: Int64 read _Gettimestamp write _Settimestamp;
  end;

  TJScanResult = class(TJavaGenericImport<JScanResultClass, JScanResult>)
  end;

  JSupplicantStateClass = interface(JObjectClass)
    ['{F3E528E0-B59C-4246-BB35-94907291219B}']
    {Property methods}
    function _GetASSOCIATED: JSupplicantState;
    procedure _SetASSOCIATED(Value: JSupplicantState);
    function _GetASSOCIATING: JSupplicantState;
    procedure _SetASSOCIATING(Value: JSupplicantState);
    function _GetAUTHENTICATING: JSupplicantState;
    procedure _SetAUTHENTICATING(Value: JSupplicantState);
    function _GetCOMPLETED: JSupplicantState;
    procedure _SetCOMPLETED(Value: JSupplicantState);
    function _GetDISCONNECTED: JSupplicantState;
    procedure _SetDISCONNECTED(Value: JSupplicantState);
    function _GetDORMANT: JSupplicantState;
    procedure _SetDORMANT(Value: JSupplicantState);
    function _GetFOUR_WAY_HANDSHAKE: JSupplicantState;
    procedure _SetFOUR_WAY_HANDSHAKE(Value: JSupplicantState);
    function _GetGROUP_HANDSHAKE: JSupplicantState;
    procedure _SetGROUP_HANDSHAKE(Value: JSupplicantState);
    function _GetINACTIVE: JSupplicantState;
    procedure _SetINACTIVE(Value: JSupplicantState);
    function _GetINTERFACE_DISABLED: JSupplicantState;
    procedure _SetINTERFACE_DISABLED(Value: JSupplicantState);
    function _GetINVALID: JSupplicantState;
    procedure _SetINVALID(Value: JSupplicantState);
    function _GetSCANNING: JSupplicantState;
    procedure _SetSCANNING(Value: JSupplicantState);
    function _GetUNINITIALIZED: JSupplicantState;
    procedure _SetUNINITIALIZED(Value: JSupplicantState);
    {Methods}
    function isValidState(state: JSupplicantState): Boolean; cdecl;
    function valueOf(name: JString): JSupplicantState; cdecl;
    function values: TJavaObjectArray<JSupplicantState>; cdecl;
    {Properties}
    property ASSOCIATED: JSupplicantState read _GetASSOCIATED write _SetASSOCIATED;
    property ASSOCIATING: JSupplicantState read _GetASSOCIATING write _SetASSOCIATING;
    property AUTHENTICATING: JSupplicantState read _GetAUTHENTICATING write _SetAUTHENTICATING;
    property COMPLETED: JSupplicantState read _GetCOMPLETED write _SetCOMPLETED;
    property DISCONNECTED: JSupplicantState read _GetDISCONNECTED write _SetDISCONNECTED;
    property DORMANT: JSupplicantState read _GetDORMANT write _SetDORMANT;
    property FOUR_WAY_HANDSHAKE: JSupplicantState read _GetFOUR_WAY_HANDSHAKE write _SetFOUR_WAY_HANDSHAKE;
    property GROUP_HANDSHAKE: JSupplicantState read _GetGROUP_HANDSHAKE write _SetGROUP_HANDSHAKE;
    property INACTIVE: JSupplicantState read _GetINACTIVE write _SetINACTIVE;
    property INTERFACE_DISABLED: JSupplicantState read _GetINTERFACE_DISABLED write _SetINTERFACE_DISABLED;
    property INVALID: JSupplicantState read _GetINVALID write _SetINVALID;
    property SCANNING: JSupplicantState read _GetSCANNING write _SetSCANNING;
    property UNINITIALIZED: JSupplicantState read _GetUNINITIALIZED write _SetUNINITIALIZED;
  end;

  [JavaSignature('android/net/wifi/SupplicantState')]
  JSupplicantState = interface(JObject)
    ['{FA5FB5F5-A5CF-4C3E-9503-88F44C57132C}']
    {Methods}
    function describeContents: Integer; cdecl;
    procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
  end;

  TJSupplicantState = class(TJavaGenericImport<JSupplicantStateClass, JSupplicantState>)
  end;

  JWifiConfiguration_AuthAlgorithmClass = interface(JObjectClass)
    ['{2FF9DA2B-8DD4-4D58-8900-E8731BBE76F7}']
    {Property methods}
    function _GetLEAP: Integer;
    function _GetOPEN: Integer;
    function _GetSHARED: Integer;
    function _Getstrings: TJavaObjectArray<JString>;
    procedure _Setstrings(Value: TJavaObjectArray<JString>);
    function _GetvarName: JString;
    {Properties}
    property LEAP: Integer read _GetLEAP;
    property OPEN: Integer read _GetOPEN;
    property SHARED: Integer read _GetSHARED;
    property strings: TJavaObjectArray<JString> read _Getstrings write _Setstrings;
    property varName: JString read _GetvarName;
  end;

  [JavaSignature('android/net/wifi/WifiConfiguration$AuthAlgorithm')]
  JWifiConfiguration_AuthAlgorithm = interface(JObject)
    ['{7227D663-1E45-4A94-B91D-4E393C353E9C}']
  end;

  TJWifiConfiguration_AuthAlgorithm = class(TJavaGenericImport<JWifiConfiguration_AuthAlgorithmClass, JWifiConfiguration_AuthAlgorithm>)
  end;

  JWifiConfiguration_GroupCipherClass = interface(JObjectClass)
    ['{826CE899-5312-4970-B501-3941F06605C2}']
    {Property methods}
    function _GetCCMP: Integer;
    function _Getstrings: TJavaObjectArray<JString>;
    procedure _Setstrings(Value: TJavaObjectArray<JString>);
    function _GetTKIP: Integer;
    function _GetvarName: JString;
    function _GetWEP104: Integer;
    function _GetWEP40: Integer;
    {Properties}
    property CCMP: Integer read _GetCCMP;
    property strings: TJavaObjectArray<JString> read _Getstrings write _Setstrings;
    property TKIP: Integer read _GetTKIP;
    property varName: JString read _GetvarName;
    property WEP104: Integer read _GetWEP104;
    property WEP40: Integer read _GetWEP40;
  end;

  [JavaSignature('android/net/wifi/WifiConfiguration$GroupCipher')]
  JWifiConfiguration_GroupCipher = interface(JObject)
    ['{BC025D00-330E-48D2-AD73-EC55348DBD6D}']
  end;

  TJWifiConfiguration_GroupCipher = class(TJavaGenericImport<JWifiConfiguration_GroupCipherClass, JWifiConfiguration_GroupCipher>)
  end;

  JWifiConfiguration_KeyMgmtClass = interface(JObjectClass)
    ['{18A87BB0-A210-427A-A4F0-F3D0C6CD12A2}']
    {Property methods}
    function _GetIEEE8021X: Integer;
    function _GetNONE: Integer;
    function _Getstrings: TJavaObjectArray<JString>;
    procedure _Setstrings(Value: TJavaObjectArray<JString>);
    function _GetvarName: JString;
    function _GetWPA_EAP: Integer;
    function _GetWPA_PSK: Integer;
    {Properties}
    property IEEE8021X: Integer read _GetIEEE8021X;
    property NONE: Integer read _GetNONE;
    property strings: TJavaObjectArray<JString> read _Getstrings write _Setstrings;
    property varName: JString read _GetvarName;
    property WPA_EAP: Integer read _GetWPA_EAP;
    property WPA_PSK: Integer read _GetWPA_PSK;
  end;

  [JavaSignature('android/net/wifi/WifiConfiguration$KeyMgmt')]
  JWifiConfiguration_KeyMgmt = interface(JObject)
    ['{C910A207-A155-4475-87E1-1F8FC4480B99}']
  end;

  TJWifiConfiguration_KeyMgmt = class(TJavaGenericImport<JWifiConfiguration_KeyMgmtClass, JWifiConfiguration_KeyMgmt>)
  end;

  JWifiConfiguration_PairwiseCipherClass = interface(JObjectClass)
    ['{86FACA51-4BDA-40C7-875D-36DEDC60A604}']
    {Property methods}
    function _GetCCMP: Integer;
    function _GetNONE: Integer;
    function _Getstrings: TJavaObjectArray<JString>;
    procedure _Setstrings(Value: TJavaObjectArray<JString>);
    function _GetTKIP: Integer;
    function _GetvarName: JString;
    {Properties}
    property CCMP: Integer read _GetCCMP;
    property NONE: Integer read _GetNONE;
    property strings: TJavaObjectArray<JString> read _Getstrings write _Setstrings;
    property TKIP: Integer read _GetTKIP;
    property varName: JString read _GetvarName;
  end;

  [JavaSignature('android/net/wifi/WifiConfiguration$PairwiseCipher')]
  JWifiConfiguration_PairwiseCipher = interface(JObject)
    ['{B0067C3F-4132-494E-925D-B08FE8948687}']
  end;

  TJWifiConfiguration_PairwiseCipher = class(TJavaGenericImport<JWifiConfiguration_PairwiseCipherClass, JWifiConfiguration_PairwiseCipher>)
  end;

  JWifiConfiguration_ProtocolClass = interface(JObjectClass)
    ['{30360847-4266-4FDD-986B-E63511E0050E}']
    {Property methods}
    function _GetRSN: Integer;
    function _Getstrings: TJavaObjectArray<JString>;
    procedure _Setstrings(Value: TJavaObjectArray<JString>);
    function _GetvarName: JString;
    function _GetWPA: Integer;
    {Properties}
    property RSN: Integer read _GetRSN;
    property strings: TJavaObjectArray<JString> read _Getstrings write _Setstrings;
    property varName: JString read _GetvarName;
    property WPA: Integer read _GetWPA;
  end;

  [JavaSignature('android/net/wifi/WifiConfiguration$Protocol')]
  JWifiConfiguration_Protocol = interface(JObject)
    ['{83B1E9AA-DC4C-43C6-ABDA-290E199D3155}']
  end;

  TJWifiConfiguration_Protocol = class(TJavaGenericImport<JWifiConfiguration_ProtocolClass, JWifiConfiguration_Protocol>)
  end;

  JWifiConfiguration_StatusClass = interface(JObjectClass)
    ['{F33687FF-3601-48FE-9384-A8A8AD500AA9}']
    {Property methods}
    function _GetCURRENT: Integer;
    function _GetDISABLED: Integer;
    function _GetENABLED: Integer;
    function _Getstrings: TJavaObjectArray<JString>;
    procedure _Setstrings(Value: TJavaObjectArray<JString>);
    {Properties}
    property CURRENT: Integer read _GetCURRENT;
    property DISABLED: Integer read _GetDISABLED;
    property ENABLED: Integer read _GetENABLED;
    property strings: TJavaObjectArray<JString> read _Getstrings write _Setstrings;
  end;

  [JavaSignature('android/net/wifi/WifiConfiguration$Status')]
  JWifiConfiguration_Status = interface(JObject)
    ['{48927254-B6D3-4C59-8194-531658A2C01D}']
  end;

  TJWifiConfiguration_Status = class(TJavaGenericImport<JWifiConfiguration_StatusClass, JWifiConfiguration_Status>)
  end;

  JWifiConfigurationClass = interface(JObjectClass)
    ['{6C046670-04F2-4339-869C-FA480E53DB2D}']
    {Methods}
    function init: JWifiConfiguration; cdecl;
  end;

  [JavaSignature('android/net/wifi/WifiConfiguration')]
  JWifiConfiguration = interface(JObject)
    ['{A0BCEA84-2972-4DAB-B7C4-A4BC880A8457}']
    {Property methods}
    function _GetallowedAuthAlgorithms: JObject {java/util/BitSet};
    procedure _SetallowedAuthAlgorithms(Value: JObject {java/util/BitSet});
    function _GetallowedGroupCiphers: JObject {java/util/BitSet};
    procedure _SetallowedGroupCiphers(Value: JObject {java/util/BitSet});
    function _GetallowedKeyManagement: JObject {java/util/BitSet};
    procedure _SetallowedKeyManagement(Value: JObject {java/util/BitSet});
    function _GetallowedPairwiseCiphers: JObject {java/util/BitSet};
    procedure _SetallowedPairwiseCiphers(Value: JObject {java/util/BitSet});
    function _GetallowedProtocols: JObject {java/util/BitSet};
    procedure _SetallowedProtocols(Value: JObject {java/util/BitSet});
    function _GetBSSID: JString;
    procedure _SetBSSID(Value: JString);
    function _GetenterpriseConfig: JWifiEnterpriseConfig;
    procedure _SetenterpriseConfig(Value: JWifiEnterpriseConfig);
    function _GetFQDN: JString;
    procedure _SetFQDN(Value: JString);
    function _GethiddenSSID: Boolean;
    procedure _SethiddenSSID(Value: Boolean);
    function _GetnetworkId: Integer;
    procedure _SetnetworkId(Value: Integer);
    function _GetpreSharedKey: JString;
    procedure _SetpreSharedKey(Value: JString);
    function _Getpriority: Integer;
    procedure _Setpriority(Value: Integer);
    function _GetSSID: JString;
    procedure _SetSSID(Value: JString);
    function _Getstatus: Integer;
    procedure _Setstatus(Value: Integer);
    function _GetwepKeys: TJavaObjectArray<JString>;
    procedure _SetwepKeys(Value: TJavaObjectArray<JString>);
    function _GetwepTxKeyIndex: Integer;
    procedure _SetwepTxKeyIndex(Value: Integer);
    {Methods}
    function describeContents: Integer; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
    {Properties}
    property allowedAuthAlgorithms: JObject {java/util/BitSet} read _GetallowedAuthAlgorithms write _SetallowedAuthAlgorithms;
    property allowedGroupCiphers: JObject {java/util/BitSet} read _GetallowedGroupCiphers write _SetallowedGroupCiphers;
    property allowedKeyManagement: JObject {java/util/BitSet} read _GetallowedKeyManagement write _SetallowedKeyManagement;
    property allowedPairwiseCiphers: JObject {java/util/BitSet} read _GetallowedPairwiseCiphers write _SetallowedPairwiseCiphers;
    property allowedProtocols: JObject {java/util/BitSet} read _GetallowedProtocols write _SetallowedProtocols;
    property BSSID: JString read _GetBSSID write _SetBSSID;
    property enterpriseConfig: JWifiEnterpriseConfig read _GetenterpriseConfig write _SetenterpriseConfig;
    property FQDN: JString read _GetFQDN write _SetFQDN;
    property hiddenSSID: Boolean read _GethiddenSSID write _SethiddenSSID;
    property networkId: Integer read _GetnetworkId write _SetnetworkId;
    property preSharedKey: JString read _GetpreSharedKey write _SetpreSharedKey;
    property priority: Integer read _Getpriority write _Setpriority;
    property SSID: JString read _GetSSID write _SetSSID;
    property status: Integer read _Getstatus write _Setstatus;
    property wepKeys: TJavaObjectArray<JString> read _GetwepKeys write _SetwepKeys;
    property wepTxKeyIndex: Integer read _GetwepTxKeyIndex write _SetwepTxKeyIndex;
  end;

  TJWifiConfiguration = class(TJavaGenericImport<JWifiConfigurationClass, JWifiConfiguration>)
  end;

  JWifiEnterpriseConfig_EapClass = interface(JObjectClass)
    ['{B8573CCA-C314-4866-A5F2-2A23ACAD193D}']
    {Property methods}
    function _GetAKA: Integer;
    function _GetNONE: Integer;
    function _GetPEAP: Integer;
    function _GetPWD: Integer;
    function _GetSIM: Integer;
    function _GetTLS: Integer;
    function _GetTTLS: Integer;
    {Properties}
    property AKA: Integer read _GetAKA;
    property NONE: Integer read _GetNONE;
    property PEAP: Integer read _GetPEAP;
    property PWD: Integer read _GetPWD;
    property SIM: Integer read _GetSIM;
    property TLS: Integer read _GetTLS;
    property TTLS: Integer read _GetTTLS;
  end;

  [JavaSignature('android/net/wifi/WifiEnterpriseConfig$Eap')]
  JWifiEnterpriseConfig_Eap = interface(JObject)
    ['{A336DC87-FA33-4D45-A903-824B0BF29A91}']
  end;

  TJWifiEnterpriseConfig_Eap = class(TJavaGenericImport<JWifiEnterpriseConfig_EapClass, JWifiEnterpriseConfig_Eap>)
  end;

  JWifiEnterpriseConfig_Phase2Class = interface(JObjectClass)
    ['{2B4DD34C-2D41-4606-82A9-F46BC4233FC5}']
    {Property methods}
    function _GetGTC: Integer;
    function _GetMSCHAP: Integer;
    function _GetMSCHAPV2: Integer;
    function _GetNONE: Integer;
    function _GetPAP: Integer;
    {Properties}
    property GTC: Integer read _GetGTC;
    property MSCHAP: Integer read _GetMSCHAP;
    property MSCHAPV2: Integer read _GetMSCHAPV2;
    property NONE: Integer read _GetNONE;
    property PAP: Integer read _GetPAP;
  end;

  [JavaSignature('android/net/wifi/WifiEnterpriseConfig$Phase2')]
  JWifiEnterpriseConfig_Phase2 = interface(JObject)
    ['{5E799160-C2E2-4A16-8469-328500DFBDD1}']
  end;

  TJWifiEnterpriseConfig_Phase2 = class(TJavaGenericImport<JWifiEnterpriseConfig_Phase2Class, JWifiEnterpriseConfig_Phase2>)
  end;

  JWifiEnterpriseConfigClass = interface(JObjectClass)
    ['{112E76C1-77C5-4E70-9057-9748A30CFC83}']
    {Property methods}
    function _GetCREATOR: JParcelable_Creator;
    procedure _SetCREATOR(Value: JParcelable_Creator);
    {Methods}
    function init: JWifiEnterpriseConfig; cdecl; overload;
    function init(source: JWifiEnterpriseConfig): JWifiEnterpriseConfig; cdecl; overload;
    {Properties}
    property CREATOR: JParcelable_Creator read _GetCREATOR write _SetCREATOR;
  end;

  [JavaSignature('android/net/wifi/WifiEnterpriseConfig')]
  JWifiEnterpriseConfig = interface(JObject)
    ['{1390F2DD-C3F8-46AA-8191-FB297CC5DDB6}']
    {Methods}
    function describeContents: Integer; cdecl;
    function getAnonymousIdentity: JString; cdecl;
    function getCaCertificate: JObject {java/security/cert/X509Certificate}; cdecl;
    function getClientCertificate: JObject {java/security/cert/X509Certificate}; cdecl;
    function getEapMethod: Integer; cdecl;
    function getIdentity: JString; cdecl;
    function getPassword: JString; cdecl;
    function getPhase2Method: Integer; cdecl;
    function getSubjectMatch: JString; cdecl;
    procedure setAnonymousIdentity(anonymousIdentity: JString); cdecl;
    procedure setCaCertificate(cert: JObject {java/security/cert/X509Certificate}); cdecl;
    procedure setClientKeyEntry(privateKey: JObject {java/security/PrivateKey}; clientCertificate: JObject {java/security/cert/X509Certificate}); cdecl;
    procedure setEapMethod(eapMethod: Integer); cdecl;
    procedure setIdentity(identity: JString); cdecl;
    procedure setPassword(password: JString); cdecl;
    procedure setPhase2Method(phase2Method: Integer); cdecl;
    procedure setSubjectMatch(subjectMatch: JString); cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
  end;

  TJWifiEnterpriseConfig = class(TJavaGenericImport<JWifiEnterpriseConfigClass, JWifiEnterpriseConfig>)
  end;

  JWifiInfoClass = interface(JObjectClass)
    ['{73382926-DBF8-4E18-9115-6B42DD25BA78}']
    {Property methods}
    function _GetFREQUENCY_UNITS: JString;
    function _GetLINK_SPEED_UNITS: JString;
    {Methods}
    function getDetailedStateOf(suppState: JSupplicantState): JObject {android/net/NetworkInfo_DetailedState}; cdecl;
    {Properties}
    property FREQUENCY_UNITS: JString read _GetFREQUENCY_UNITS;
    property LINK_SPEED_UNITS: JString read _GetLINK_SPEED_UNITS;
  end;

  [JavaSignature('android/net/wifi/WifiInfo')]
  JWifiInfo = interface(JObject)
    ['{D442D812-CD4A-4D10-B9DC-44346AFA903D}']
    {Methods}
    function describeContents: Integer; cdecl;
    function getBSSID: JString; cdecl;
    function getFrequency: Integer; cdecl;
    function getHiddenSSID: Boolean; cdecl;
    function getIpAddress: Integer; cdecl;
    function getLinkSpeed: Integer; cdecl;
    function getMacAddress: JString; cdecl;
    function getNetworkId: Integer; cdecl;
    function getRssi: Integer; cdecl;
    function getSSID: JString; cdecl;
    function getSupplicantState: JSupplicantState; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
  end;

  TJWifiInfo = class(TJavaGenericImport<JWifiInfoClass, JWifiInfo>)
  end;

  JWifiManager_MulticastLockClass = interface(JObjectClass)
    ['{4BA4E377-EF60-404A-B661-FE3C4096CF12}']
  end;

  [JavaSignature('android/net/wifi/WifiManager$MulticastLock')]
  JWifiManager_MulticastLock = interface(JObject)
    ['{1D4AD598-0B83-47D2-8FEE-C66866A5E367}']
    {Methods}
    procedure acquire; cdecl;
    function isHeld: Boolean; cdecl;
    procedure release; cdecl;
    procedure setReferenceCounted(refCounted: Boolean); cdecl;
    function toString: JString; cdecl;
  end;

  TJWifiManager_MulticastLock = class(TJavaGenericImport<JWifiManager_MulticastLockClass, JWifiManager_MulticastLock>)
  end;

  JWifiManager_WifiLockClass = interface(JObjectClass)
    ['{A29F351A-A063-403C-9973-28C18ACFA565}']
  end;

  [JavaSignature('android/net/wifi/WifiManager$WifiLock')]
  JWifiManager_WifiLock = interface(JObject)
    ['{C23C251A-384C-481E-A819-AEF4E4D04D68}']
    {Methods}
    procedure acquire; cdecl;
    function isHeld: Boolean; cdecl;
    procedure release; cdecl;
    procedure setReferenceCounted(refCounted: Boolean); cdecl;
    procedure setWorkSource(ws: JObject {android/os/WorkSource}); cdecl;
    function toString: JString; cdecl;
  end;

  TJWifiManager_WifiLock = class(TJavaGenericImport<JWifiManager_WifiLockClass, JWifiManager_WifiLock>)
  end;

  JWifiManager_WpsCallbackClass = interface(JObjectClass)
    ['{D658C655-5F8D-4AC7-A552-F7BCCA254EE2}']
    {Methods}
    function init: JWifiManager_WpsCallback; cdecl;
  end;

  [JavaSignature('android/net/wifi/WifiManager$WpsCallback')]
  JWifiManager_WpsCallback = interface(JObject)
    ['{3112D1AA-1027-46C3-A879-5EB75A9F0F5F}']
    {Methods}
    procedure onFailed(Param0: Integer); cdecl;
    procedure onStarted(Param0: JString); cdecl;
    procedure onSucceeded; cdecl;
  end;

  TJWifiManager_WpsCallback = class(TJavaGenericImport<JWifiManager_WpsCallbackClass, JWifiManager_WpsCallback>)
  end;

  JWifiManagerClass = interface(JObjectClass)
    ['{E2AE3F67-FE78-47CE-A70C-8345DEA6F3FE}']
    {Property methods}
    function _GetACTION_PICK_WIFI_NETWORK: JString;
    function _GetACTION_REQUEST_SCAN_ALWAYS_AVAILABLE: JString;
    function _GetERROR_AUTHENTICATING: Integer;
    function _GetEXTRA_BSSID: JString;
    function _GetEXTRA_NETWORK_INFO: JString;
    function _GetEXTRA_NEW_RSSI: JString;
    function _GetEXTRA_NEW_STATE: JString;
    function _GetEXTRA_PREVIOUS_WIFI_STATE: JString;
    function _GetEXTRA_SUPPLICANT_CONNECTED: JString;
    function _GetEXTRA_SUPPLICANT_ERROR: JString;
    function _GetEXTRA_WIFI_INFO: JString;
    function _GetEXTRA_WIFI_STATE: JString;
    function _GetNETWORK_IDS_CHANGED_ACTION: JString;
    function _GetNETWORK_STATE_CHANGED_ACTION: JString;
    function _GetRSSI_CHANGED_ACTION: JString;
    function _GetSCAN_RESULTS_AVAILABLE_ACTION: JString;
    function _GetSUPPLICANT_CONNECTION_CHANGE_ACTION: JString;
    function _GetSUPPLICANT_STATE_CHANGED_ACTION: JString;
    function _GetWIFI_MODE_FULL: Integer;
    function _GetWIFI_MODE_FULL_HIGH_PERF: Integer;
    function _GetWIFI_MODE_SCAN_ONLY: Integer;
    function _GetWIFI_STATE_CHANGED_ACTION: JString;
    function _GetWIFI_STATE_DISABLED: Integer;
    function _GetWIFI_STATE_DISABLING: Integer;
    function _GetWIFI_STATE_ENABLED: Integer;
    function _GetWIFI_STATE_ENABLING: Integer;
    function _GetWIFI_STATE_UNKNOWN: Integer;
    function _GetWPS_AUTH_FAILURE: Integer;
    function _GetWPS_OVERLAP_ERROR: Integer;
    function _GetWPS_TIMED_OUT: Integer;
    function _GetWPS_TKIP_ONLY_PROHIBITED: Integer;
    function _GetWPS_WEP_PROHIBITED: Integer;
    {Methods}
    function calculateSignalLevel(rssi: Integer; numLevels: Integer): Integer; cdecl;
    function compareSignalLevel(rssiA: Integer; rssiB: Integer): Integer; cdecl;
    {Properties}
    property ACTION_PICK_WIFI_NETWORK: JString read _GetACTION_PICK_WIFI_NETWORK;
    property ACTION_REQUEST_SCAN_ALWAYS_AVAILABLE: JString read _GetACTION_REQUEST_SCAN_ALWAYS_AVAILABLE;
    property ERROR_AUTHENTICATING: Integer read _GetERROR_AUTHENTICATING;
    property EXTRA_BSSID: JString read _GetEXTRA_BSSID;
    property EXTRA_NETWORK_INFO: JString read _GetEXTRA_NETWORK_INFO;
    property EXTRA_NEW_RSSI: JString read _GetEXTRA_NEW_RSSI;
    property EXTRA_NEW_STATE: JString read _GetEXTRA_NEW_STATE;
    property EXTRA_PREVIOUS_WIFI_STATE: JString read _GetEXTRA_PREVIOUS_WIFI_STATE;
    property EXTRA_SUPPLICANT_CONNECTED: JString read _GetEXTRA_SUPPLICANT_CONNECTED;
    property EXTRA_SUPPLICANT_ERROR: JString read _GetEXTRA_SUPPLICANT_ERROR;
    property EXTRA_WIFI_INFO: JString read _GetEXTRA_WIFI_INFO;
    property EXTRA_WIFI_STATE: JString read _GetEXTRA_WIFI_STATE;
    property NETWORK_IDS_CHANGED_ACTION: JString read _GetNETWORK_IDS_CHANGED_ACTION;
    property NETWORK_STATE_CHANGED_ACTION: JString read _GetNETWORK_STATE_CHANGED_ACTION;
    property RSSI_CHANGED_ACTION: JString read _GetRSSI_CHANGED_ACTION;
    property SCAN_RESULTS_AVAILABLE_ACTION: JString read _GetSCAN_RESULTS_AVAILABLE_ACTION;
    property SUPPLICANT_CONNECTION_CHANGE_ACTION: JString read _GetSUPPLICANT_CONNECTION_CHANGE_ACTION;
    property SUPPLICANT_STATE_CHANGED_ACTION: JString read _GetSUPPLICANT_STATE_CHANGED_ACTION;
    property WIFI_MODE_FULL: Integer read _GetWIFI_MODE_FULL;
    property WIFI_MODE_FULL_HIGH_PERF: Integer read _GetWIFI_MODE_FULL_HIGH_PERF;
    property WIFI_MODE_SCAN_ONLY: Integer read _GetWIFI_MODE_SCAN_ONLY;
    property WIFI_STATE_CHANGED_ACTION: JString read _GetWIFI_STATE_CHANGED_ACTION;
    property WIFI_STATE_DISABLED: Integer read _GetWIFI_STATE_DISABLED;
    property WIFI_STATE_DISABLING: Integer read _GetWIFI_STATE_DISABLING;
    property WIFI_STATE_ENABLED: Integer read _GetWIFI_STATE_ENABLED;
    property WIFI_STATE_ENABLING: Integer read _GetWIFI_STATE_ENABLING;
    property WIFI_STATE_UNKNOWN: Integer read _GetWIFI_STATE_UNKNOWN;
    property WPS_AUTH_FAILURE: Integer read _GetWPS_AUTH_FAILURE;
    property WPS_OVERLAP_ERROR: Integer read _GetWPS_OVERLAP_ERROR;
    property WPS_TIMED_OUT: Integer read _GetWPS_TIMED_OUT;
    property WPS_TKIP_ONLY_PROHIBITED: Integer read _GetWPS_TKIP_ONLY_PROHIBITED;
    property WPS_WEP_PROHIBITED: Integer read _GetWPS_WEP_PROHIBITED;
  end;

  [JavaSignature('android/net/wifi/WifiManager')]
  JWifiManager = interface(JObject)
    ['{E02AAEE8-D4CE-403D-A2E4-28A58F42FCEB}']
    {Methods}
    function addNetwork(config: JWifiConfiguration): Integer; cdecl;
    procedure cancelWps(listener: JWifiManager_WpsCallback); cdecl;
    function createMulticastLock(tag: JString): JWifiManager_MulticastLock; cdecl;
    function createWifiLock(lockType: Integer; tag: JString): JWifiManager_WifiLock; cdecl; overload;
    function createWifiLock(tag: JString): JWifiManager_WifiLock; cdecl; overload;
    function disableNetwork(netId: Integer): Boolean; cdecl;
    function disconnect: Boolean; cdecl;
    function enableNetwork(netId: Integer; disableOthers: Boolean): Boolean; cdecl;
    function getConfiguredNetworks: JList; cdecl;
    function getConnectionInfo: JWifiInfo; cdecl;
    function getDhcpInfo: JObject {android/net/DhcpInfo}; cdecl;
    function getScanResults: JList; cdecl;
    function getWifiState: Integer; cdecl;
    function is5GHzBandSupported: Boolean; cdecl;
    function isDeviceToApRttSupported: Boolean; cdecl;
    function isEnhancedPowerReportingSupported: Boolean; cdecl;
    function isP2pSupported: Boolean; cdecl;
    function isPreferredNetworkOffloadSupported: Boolean; cdecl;
    function isScanAlwaysAvailable: Boolean; cdecl;
    function isTdlsSupported: Boolean; cdecl;
    function isWifiEnabled: Boolean; cdecl;
    function pingSupplicant: Boolean; cdecl;
    function reassociate: Boolean; cdecl;
    function reconnect: Boolean; cdecl;
    function removeNetwork(netId: Integer): Boolean; cdecl;
    function saveConfiguration: Boolean; cdecl;
    procedure setTdlsEnabled(remoteIPAddress: JObject {java/net/InetAddress}; enable: Boolean); cdecl;
    procedure setTdlsEnabledWithMacAddress(remoteMacAddress: JString; enable: Boolean); cdecl;
    function setWifiEnabled(enabled: Boolean): Boolean; cdecl;
    function startScan: Boolean; cdecl;
    procedure startWps(config: JWpsInfo; listener: JWifiManager_WpsCallback); cdecl;
    function updateNetwork(config: JWifiConfiguration): Integer; cdecl;
  end;

  TJWifiManager = class(TJavaGenericImport<JWifiManagerClass, JWifiManager>)
  end;

  JWpsInfoClass = interface(JObjectClass)
    ['{41108D38-FCBF-4A8C-92C0-A5A7AA45C1B4}']
    {Property methods}
    function _GetCREATOR: JParcelable_Creator;
    procedure _SetCREATOR(Value: JParcelable_Creator);
    function _GetDISPLAY: Integer;
    function _GetINVALID: Integer;
    function _GetKEYPAD: Integer;
    function _GetLABEL: Integer;
    function _GetPBC: Integer;
    {Methods}
    function init: JWpsInfo; cdecl; overload;
    function init(source: JWpsInfo): JWpsInfo; cdecl; overload;
    {Properties}
    property CREATOR: JParcelable_Creator read _GetCREATOR write _SetCREATOR;
    property DISPLAY: Integer read _GetDISPLAY;
    property INVALID: Integer read _GetINVALID;
    property KEYPAD: Integer read _GetKEYPAD;
    property &LABEL: Integer read _GetLABEL;
    property PBC: Integer read _GetPBC;
  end;

  [JavaSignature('android/net/wifi/WpsInfo')]
  JWpsInfo = interface(JObject)
    ['{02EF62ED-4183-41C9-8A20-3248879B40D2}']
    {Property methods}
    function _GetBSSID: JString;
    procedure _SetBSSID(Value: JString);
    function _Getpin: JString;
    procedure _Setpin(Value: JString);
    function _Getsetup: Integer;
    procedure _Setsetup(Value: Integer);
    {Methods}
    function describeContents: Integer; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
    {Properties}
    property BSSID: JString read _GetBSSID write _SetBSSID;
    property pin: JString read _Getpin write _Setpin;
    property setup: Integer read _Getsetup write _Setsetup;
  end;

  TJWpsInfo = class(TJavaGenericImport<JWpsInfoClass, JWpsInfo>)
  end;

const
  TJWifiP2pServiceInfo_SERVICE_TYPE_ALL = 0;
  TJWifiP2pServiceInfo_SERVICE_TYPE_BONJOUR = 1;
  TJWifiP2pServiceInfo_SERVICE_TYPE_UPNP = 2;
  TJWifiP2pServiceInfo_SERVICE_TYPE_VENDOR_SPECIFIC = 255;

  TJWifiP2pDevice_AVAILABLE = 3;
  TJWifiP2pDevice_CONNECTED = 0;
  TJWifiP2pDevice_FAILED = 2;
  TJWifiP2pDevice_INVITED = 1;
  TJWifiP2pDevice_UNAVAILABLE = 4;

  TJWifiP2pManager_BUSY = 2;
  TJWifiP2pManager_ERROR = 0;
  TJWifiP2pManager_EXTRA_DISCOVERY_STATE = 'discoveryState';
  TJWifiP2pManager_EXTRA_NETWORK_INFO = 'networkInfo';
  TJWifiP2pManager_EXTRA_P2P_DEVICE_LIST = 'wifiP2pDeviceList';
  TJWifiP2pManager_EXTRA_WIFI_P2P_DEVICE = 'wifiP2pDevice';
  TJWifiP2pManager_EXTRA_WIFI_P2P_GROUP = 'p2pGroupInfo';
  TJWifiP2pManager_EXTRA_WIFI_P2P_INFO = 'wifiP2pInfo';
  TJWifiP2pManager_EXTRA_WIFI_STATE = 'wifi_p2p_state';
  TJWifiP2pManager_NO_SERVICE_REQUESTS = 3;
  TJWifiP2pManager_P2P_UNSUPPORTED = 1;
  TJWifiP2pManager_WIFI_P2P_CONNECTION_CHANGED_ACTION = 'android.net.wifi.p2p.CONNECTION_STATE_CHANGE';
  TJWifiP2pManager_WIFI_P2P_DISCOVERY_CHANGED_ACTION = 'android.net.wifi.p2p.DISCOVERY_STATE_CHANGE';
  TJWifiP2pManager_WIFI_P2P_DISCOVERY_STARTED = 2;
  TJWifiP2pManager_WIFI_P2P_DISCOVERY_STOPPED = 1;
  TJWifiP2pManager_WIFI_P2P_PEERS_CHANGED_ACTION = 'android.net.wifi.p2p.PEERS_CHANGED';
  TJWifiP2pManager_WIFI_P2P_STATE_CHANGED_ACTION = 'android.net.wifi.p2p.STATE_CHANGED';
  TJWifiP2pManager_WIFI_P2P_STATE_DISABLED = 1;
  TJWifiP2pManager_WIFI_P2P_STATE_ENABLED = 2;
  TJWifiP2pManager_WIFI_P2P_THIS_DEVICE_CHANGED_ACTION = 'android.net.wifi.p2p.THIS_DEVICE_CHANGED';

  TJWifiConfiguration_AuthAlgorithm_LEAP = 2;
  TJWifiConfiguration_AuthAlgorithm_OPEN = 0;
  TJWifiConfiguration_AuthAlgorithm_SHARED = 1;
  TJWifiConfiguration_AuthAlgorithm_varName = 'auth_alg';

  TJWifiConfiguration_GroupCipher_CCMP = 3;
  TJWifiConfiguration_GroupCipher_TKIP = 2;
  TJWifiConfiguration_GroupCipher_varName = 'group';
  TJWifiConfiguration_GroupCipher_WEP104 = 1;
  TJWifiConfiguration_GroupCipher_WEP40 = 0;

  TJWifiConfiguration_KeyMgmt_IEEE8021X = 3;
  TJWifiConfiguration_KeyMgmt_NONE = 0;
  TJWifiConfiguration_KeyMgmt_varName = 'key_mgmt';
  TJWifiConfiguration_KeyMgmt_WPA_EAP = 2;
  TJWifiConfiguration_KeyMgmt_WPA_PSK = 1;

  TJWifiConfiguration_PairwiseCipher_CCMP = 2;
  TJWifiConfiguration_PairwiseCipher_NONE = 0;
  TJWifiConfiguration_PairwiseCipher_TKIP = 1;
  TJWifiConfiguration_PairwiseCipher_varName = 'pairwise';

  TJWifiConfiguration_Protocol_RSN = 1;
  TJWifiConfiguration_Protocol_varName = 'proto';
  TJWifiConfiguration_Protocol_WPA = 0;

  TJWifiConfiguration_Status_CURRENT = 0;
  TJWifiConfiguration_Status_DISABLED = 1;
  TJWifiConfiguration_Status_ENABLED = 2;

  TJWifiEnterpriseConfig_Eap_AKA = 5;
  TJWifiEnterpriseConfig_Eap_NONE = -1;
  TJWifiEnterpriseConfig_Eap_PEAP = 0;
  TJWifiEnterpriseConfig_Eap_PWD = 3;
  TJWifiEnterpriseConfig_Eap_SIM = 4;
  TJWifiEnterpriseConfig_Eap_TLS = 1;
  TJWifiEnterpriseConfig_Eap_TTLS = 2;

  TJWifiEnterpriseConfig_Phase2_GTC = 4;
  TJWifiEnterpriseConfig_Phase2_MSCHAP = 2;
  TJWifiEnterpriseConfig_Phase2_MSCHAPV2 = 3;
  TJWifiEnterpriseConfig_Phase2_NONE = 0;
  TJWifiEnterpriseConfig_Phase2_PAP = 1;

  TJWifiInfo_FREQUENCY_UNITS = 'MHz';
  TJWifiInfo_LINK_SPEED_UNITS = 'Mbps';

  TJWifiManager_ACTION_PICK_WIFI_NETWORK = 'android.net.wifi.PICK_WIFI_NETWORK';
  TJWifiManager_ACTION_REQUEST_SCAN_ALWAYS_AVAILABLE = 'android.net.wifi.action.REQUEST_SCAN_ALWAYS_AVAILABLE';
  TJWifiManager_ERROR_AUTHENTICATING = 1;
  TJWifiManager_EXTRA_BSSID = 'bssid';
  TJWifiManager_EXTRA_NETWORK_INFO = 'networkInfo';
  TJWifiManager_EXTRA_NEW_RSSI = 'newRssi';
  TJWifiManager_EXTRA_NEW_STATE = 'newState';
  TJWifiManager_EXTRA_PREVIOUS_WIFI_STATE = 'previous_wifi_state';
  TJWifiManager_EXTRA_SUPPLICANT_CONNECTED = 'connected';
  TJWifiManager_EXTRA_SUPPLICANT_ERROR = 'supplicantError';
  TJWifiManager_EXTRA_WIFI_INFO = 'wifiInfo';
  TJWifiManager_EXTRA_WIFI_STATE = 'wifi_state';
  TJWifiManager_NETWORK_IDS_CHANGED_ACTION = 'android.net.wifi.NETWORK_IDS_CHANGED';
  TJWifiManager_NETWORK_STATE_CHANGED_ACTION = 'android.net.wifi.STATE_CHANGE';
  TJWifiManager_RSSI_CHANGED_ACTION = 'android.net.wifi.RSSI_CHANGED';
  TJWifiManager_SCAN_RESULTS_AVAILABLE_ACTION = 'android.net.wifi.SCAN_RESULTS';
  TJWifiManager_SUPPLICANT_CONNECTION_CHANGE_ACTION = 'android.net.wifi.supplicant.CONNECTION_CHANGE';
  TJWifiManager_SUPPLICANT_STATE_CHANGED_ACTION = 'android.net.wifi.supplicant.STATE_CHANGE';
  TJWifiManager_WIFI_MODE_FULL = 1;
  TJWifiManager_WIFI_MODE_FULL_HIGH_PERF = 3;
  TJWifiManager_WIFI_MODE_SCAN_ONLY = 2;
  TJWifiManager_WIFI_STATE_CHANGED_ACTION = 'android.net.wifi.WIFI_STATE_CHANGED';
  TJWifiManager_WIFI_STATE_DISABLED = 1;
  TJWifiManager_WIFI_STATE_DISABLING = 0;
  TJWifiManager_WIFI_STATE_ENABLED = 3;
  TJWifiManager_WIFI_STATE_ENABLING = 2;
  TJWifiManager_WIFI_STATE_UNKNOWN = 4;
  TJWifiManager_WPS_AUTH_FAILURE = 6;
  TJWifiManager_WPS_OVERLAP_ERROR = 3;
  TJWifiManager_WPS_TIMED_OUT = 7;
  TJWifiManager_WPS_TKIP_ONLY_PROHIBITED = 5;
  TJWifiManager_WPS_WEP_PROHIBITED = 4;

  TJWpsInfo_DISPLAY = 1;
  TJWpsInfo_INVALID = 4;
  TJWpsInfo_KEYPAD = 2;
  TJWpsInfo_LABEL = 3;
  TJWpsInfo_PBC = 0;

implementation

end.