{ ******************************************************* }
{ This is the main jabber client unit }
{ }
{ }
{ Copyright (C) 2014 }
{ }
{ ******************************************************* }

unit fmxJabberClient;

interface

uses Sysutils, fmx.dialogs, System.Classes, fmx.Types,
  FmxJabberTools, fmxJabberXml, fmxSocket, fmxSASLCrypt;

type
  TfmxJabberClient = class(TFmxObject) // The jabber component
  private
    fSocket: TJabberSocket; // The socket core
    fXml: TfmxJabberXml; // the xml core
    fConnected: Boolean;
    fLogin: string;
    fPassword: string;
    fJabberServer: string;
    fJabberPort: Integer;
    FUserStatus: TUserStatus; // userstatus (online, oflfine, ...)
    FUserDisplayMsg: string; // user display message
    fJResource: String;

    fLogged: Boolean;
    fOnLogged: TOnResponseWithNoData;
    fOnGettingContacts: TOnResponseWithNoData;
    fOnUpdateContact: TOnResponseWithStrData;
    fOnNewContact: TOnResponseWithStrData;
    fOnMessageReceived: TOnMessageReceived;

    fOnAskToAddRosterStatus: TOnAddRequestStatus;
    fOnAskedToAddContact: TOnContactAskToAdd;

    fContactsList: TList;

    procedure OnReceivedDataProc(AServerResponse: string);
    procedure OnInitConnectionProc(AId: String; ACompression: String;
      AUseSASL: Boolean; AMechanism: TMechanism);
    procedure OnCryptedSASLProc(ACryptedSignature: String);
    procedure OnSuccessSASL(ASASLResponse: String);
    procedure OnBindInitProc;
    procedure OnSessionInitProc;
    procedure OnIQRequest(AId: string);
    procedure OnGettingContactsProc(AContactsList: String);
    procedure OnFailureProc(AError: String);
    procedure OnUpdateRosterProc(AName, AJID, ASubscription, AGroup: string);
    procedure OnAskToAddRosterStatusProc(AFrom: string; AAccepted: Boolean);
    procedure OnAskedForSubscriptionProc(AJID: string);
    procedure OnMessageReceivedProc(AFrom, AMessage: string);
    procedure OnPresenceCallBackProc(AFrom, ATo, ADisplayMessage: String;
      APresence: TUserStatus);
    procedure AddNewContact(AJID: string);
    procedure ClearContactsList;
    function Getcontact(AIdx: Integer): PContact;
    function GetContactsCount: Integer;

  protected
  public
    constructor Create(AOwner : TComponent);override;
    Destructor Destroy; override;

    procedure InitializeSocket;
    procedure DoLogon;
    procedure Disconnect;
    procedure SetPresence(Astatus: TUserStatus; AMessage: String);
    procedure GetcontactsList;
    procedure AddContact(AUsername, AJID, AGroup: string);
    procedure SendMessage(ATo, AMessage: string);
    procedure RemoveContact(AContactIdx: Integer);

    property Connected: Boolean read fConnected;
    property UserDisplayMsg: string read FUserDisplayMsg write FUserDisplayMsg;
    property UserStatus: TUserStatus read FUserStatus write FUserStatus;
    property Contacts[AIdx: Integer]: PContact read Getcontact;
    property ContactsCount: Integer read GetContactsCount;

    property Logged: Boolean read fLogged write fLogged;

  Published
    property Login: String read fLogin write fLogin;
    property Password: String read fPassword write fPassword;
    property JabberServer: String read fJabberServer write fJabberServer;
    property JabberPort: Integer read fJabberPort write fJabberPort;
    property JResource: string read fJResource write fJResource;

    property OnLogged: TOnResponseWithNoData read fOnLogged write fOnLogged;
    property OnGettingContacts: TOnResponseWithNoData read fOnGettingContacts
      write fOnGettingContacts;
    property OnNewContact: TOnResponseWithStrData read fOnNewContact
      write fOnNewContact;
    property OnContactAskToAdd: TOnContactAskToAdd read fOnAskedToAddContact
      write fOnAskedToAddContact;
    property OnAddContactStatus: TOnAddRequestStatus
      read fOnAskToAddRosterStatus write fOnAskToAddRosterStatus;
    property OnMessageReceived: TOnMessageReceived read fOnMessageReceived
      write fOnMessageReceived;
    property OnUpdatePresence: TOnResponseWithStrData read fOnUpdateContact
      write fOnUpdateContact;

  end;

procedure Register;

implementation

{ TJabberClient }

{ -------------------------------------------------------------------------------
  Procedure: Register
  Arguments: None
  Result:    None
  ------------------------------------------------------------------------------- }
procedure Register;
begin
  RegisterComponents('FmxJabberClient', [TfmxJabberClient]);
end;

{ -------------------------------------------------------------------------------
  Procedure: TfmxJabberClient.Disconnect : Send disconnect request to server
  Arguments: None
  Result:    None
  ------------------------------------------------------------------------------- }
procedure TfmxJabberClient.Disconnect;
begin
  try
    fSocket.SendRequest('<presence type="unavailable"/>');
    fSocket.UnInitialize;
  except
    On E: Exception do
      Raise Exception.Create('[TfmxJabberClient.Disconnect] : ' + E.message);
  end;
end;

{ -------------------------------------------------------------------------------
  Procedure: TfmxJabberClient.DoLogon : Send authentification request
  Arguments: None
  Result:    None
  ------------------------------------------------------------------------------- }
procedure TfmxJabberClient.DoLogon;
var
  wLoginRequest: String;
begin
  try
    wLoginRequest := fXml.CreateInitRequest(fJabberServer);
    fSocket.SendRequest(wLoginRequest);
  except
    On E: Exception do
      Raise Exception.Create('[TfmxJabberClient.DoLogon] : ' + E.message);
  end;
end;

{ -------------------------------------------------------------------------------
  Procedure: TfmxJabberClient.Getcontact : Get a contact data from contacts list
  Arguments: AIdx: integer
  Result:    PContact
  ------------------------------------------------------------------------------- }
function TfmxJabberClient.Getcontact(AIdx: Integer): PContact;
begin
  Result := fContactsList[AIdx];
end;

{ -------------------------------------------------------------------------------
  Procedure: TfmxJabberClient.GetContactsCount : Get contacts count
  Arguments: None
  Result:    integer
  ------------------------------------------------------------------------------- }
function TfmxJabberClient.GetContactsCount: Integer;
begin
  Result := fContactsList.count;
end;

{ -------------------------------------------------------------------------------
  Procedure: TfmxJabberClient.GetcontactsList : Send "Get contacts " request to server
  Author:    kelhedadi
  DateTime:  2014.04.22
  Arguments: None
  Result:    None
  ------------------------------------------------------------------------------- }
procedure TfmxJabberClient.GetcontactsList;
var
  wRequest: String;
begin
  try
    wRequest := fXml.CreateGetcontactsRequest; // create xml request
    fSocket.SendRequest(wRequest); // send request
  except
    On E: Exception do
      Raise Exception.Create('[TfmxJabberClient.GetcontactsList] : ' +
        E.message);
  end;
end;

{ -------------------------------------------------------------------------------
  Procedure: TfmxJabberClient.AddContact : Add a contact to list
  Arguments: AUsername, AJID, AGroup: string
  Result:    None
  ------------------------------------------------------------------------------- }
procedure TfmxJabberClient.AddContact(AUsername, AJID, AGroup: string);
var
  wRequest: string;
  wFrom: string;
begin
  wFrom := fLogin + '@' + fJabberServer;
  wRequest := fXml.CreateAddContactRequest(wFrom, AUsername, AJID, AGroup);
  // Create add request
  fSocket.SendRequest(wRequest); // send request to server
end;

{ -------------------------------------------------------------------------------
  Procedure: TfmxJabberClient.ClearContactsList
  Arguments: None
  Result:    None
  ------------------------------------------------------------------------------- }
procedure TfmxJabberClient.ClearContactsList;
var
  wcontact: PContact;
  i: Integer;
begin
  for i := 0 to fContactsList.count - 1 do
  begin
    wcontact := fContactsList[i];
    Dispose(wcontact);
  end;
  fContactsList.Clear;
end;

{ -------------------------------------------------------------------------------
  Procedure: TfmxJabberClient.Create
  Arguments: None
  Result:    None
  ------------------------------------------------------------------------------- }
constructor TfmxJabberClient.Create(AOwner : TComponent);
begin
  try
    inherited;
    if not(csDesigning in ComponentState) then
    begin
      fLogged := False; // initialize variables and events
      fContactsList := TList.Create;
      fSocket := TJabberSocket.Create;
      fSocket.OnDataReceived := OnReceivedDataProc;
      fXml := TfmxJabberXml.Create;
      fXml.OnInitConnection := OnInitConnectionProc;
      fXml.OnCryptedSASL := OnCryptedSASLProc;
      fXml.OnSuccessSASL := OnSuccessSASL;
      fXml.OnBindInit := OnBindInitProc;
      fXml.OnSessionInit := OnSessionInitProc;
      fXml.OnIQRequest := OnIQRequest;
      fXml.OnGettingContacts := OnGettingContactsProc;
      fXml.OnFaillure := OnFailureProc;
      fXml.OnUpdateRoster := OnUpdateRosterProc;
      fXml.OnAskToAddRosterStatus := OnAskToAddRosterStatusProc;
      fXml.OnAskedForSubscription := OnAskedForSubscriptionProc;
      fXml.OnMessageReceived := OnMessageReceivedProc;
      fXml.OnPresenceCallback := OnPresenceCallBackProc;
      fConnected := False;

    end;
  except
    On E: Exception do
      Raise Exception.Create('[TfmxJabberClient.Create] : ' + E.message);
  end;

end;

{ -------------------------------------------------------------------------------
  Procedure: TfmxJabberClient.destroy
  Arguments: None
  Result:    None
  ------------------------------------------------------------------------------- }
destructor TfmxJabberClient.Destroy;
begin
  try
    if not(csDesigning in ComponentState) then
    begin
      ClearContactsList;
      fContactsList.free;
      if assigned(fXml) then
        fXml.free;
      if assigned(fSocket) then
        fSocket.free;
    end;
    inherited;
  except
    On E: Exception do
      Raise Exception.Create('[TfmxJabberClient.destroy] : ' + E.message);
  end;
end;

{ -------------------------------------------------------------------------------
  Procedure: TfmxJabberClient.InitializeSocket
  Arguments: None
  Result:    None
  ------------------------------------------------------------------------------- }
procedure TfmxJabberClient.InitializeSocket;
begin
  try
    fSocket.Initialize(fJabberServer, fJabberPort);
    // initialize tcp connection with server
    fConnected := fSocket.Connected;
  except
    On E: Exception do
      Raise Exception.Create('[TJabberClient.Initialize] : ' + E.message);
  end;
end;

{ -------------------------------------------------------------------------------
  Procedure: TfmxJabberClient.OnAskToAddRosterStatusProc : received when you ask to add a contact and he accepted your request
  Arguments: AFrom : string; AAccepted : Boolean
  Result:    None
  ------------------------------------------------------------------------------- }
procedure TfmxJabberClient.OnAskToAddRosterStatusProc(AFrom: string;
  AAccepted: Boolean);
begin
  if AAccepted then // contact accept your request
    AddNewContact(AFrom);

  if assigned(fOnAskToAddRosterStatus) then
    fOnAskToAddRosterStatus(AFrom, AAccepted);
end;

{ -------------------------------------------------------------------------------
  Procedure: TfmxJabberClient.OnAskedForSubscriptionProc : when a contact want to add you to his list
  Arguments: AJid : string
  Result:    None
  ------------------------------------------------------------------------------- }
procedure TfmxJabberClient.OnAskedForSubscriptionProc(AJID: string);
var
  wAccept: Boolean;
begin
  wAccept := False;
  if assigned(fOnAskedToAddContact) then
    fOnAskedToAddContact(AJID, wAccept); // ask user for accept request
  if wAccept then // if user accept adding request
    fSocket.SendRequest
      (Format('<presence from="%s" to="%s" type="subscribed"/>',
      [fLogin + '@' + fJabberServer, AJID]) + fXml.CreateAddContactRequest
      (fLogin + '@' + fJabberServer, '', AJID, ''))
  else
    fSocket.SendRequest
      (Format('<presence from="%s" to="%s" type="unsubscribed"/>',
      [fLogin + '@' + fJabberServer, AJID]));
end;

{ -------------------------------------------------------------------------------
  Procedure: TfmxJabberClient.AddNewContact
  Arguments: AJID : string
  Result:    None
  ------------------------------------------------------------------------------- }
procedure TfmxJabberClient.AddNewContact(AJID: string);
var
  wcontact: PContact;
begin
  New(wcontact); // create contact record and add it to contacts list
  wcontact.Name := AJID;
  wcontact.Jid := AJID;
  wcontact.Astatus := usInVisible;
  wcontact.ADisplayMessage := '';
  fContactsList.Add(wcontact);
  if assigned(fOnNewContact) then
    fOnNewContact(AJID);
end;

{ -------------------------------------------------------------------------------
  Procedure: TfmxJabberClient.OnBindInitProc
  Arguments: None
  Result:    None
  ------------------------------------------------------------------------------- }
procedure TfmxJabberClient.OnBindInitProc;
var
  wRequest: string;
begin
  wRequest := fXml.CreateInitBindRequest(fJResource);
  fSocket.SendRequest(wRequest);
end;

{ -------------------------------------------------------------------------------
  Procedure: TfmxJabberClient.OnCryptedSASLProc
  Arguments: ACryptedSignature: String
  Result:    None
  ------------------------------------------------------------------------------- }
procedure TfmxJabberClient.OnCryptedSASLProc(ACryptedSignature: String);
var
  wDecencoded: String;
  wRequest: String;
begin
  try
    wDecencoded := GetSASLResponse(ACryptedSignature, fLogin, fPassword,
      fJabberServer); // Get SASL response from server request
    wRequest := fXml.CreateSASLRequest(wDecencoded); // Create response XML
    fSocket.SendRequest(wRequest);
  except
    On E: Exception do
      Raise Exception.Create('[TfmxJabberClient.OnCryptedSASLProc] : ' +
        E.message);
  end;
end;

{ -------------------------------------------------------------------------------
  Procedure: TfmxJabberClient.OnFailureProc
  Arguments: AError: String
  Result:    None
  ------------------------------------------------------------------------------- }
procedure TfmxJabberClient.OnFailureProc(AError: String);
begin
  Raise Exception.Create(AError);
end;

{ -------------------------------------------------------------------------------
  Procedure: TfmxJabberClient.OnGettingContactsProc : Getting contacts from server
  Arguments: AContactsList: String
  Result:    None
  ------------------------------------------------------------------------------- }
procedure TfmxJabberClient.OnGettingContactsProc(AContactsList: String);
var
  wStrlist, wItems: Tstringlist;
  i: Integer;
  wcontact: PContact;
begin
  try
    ClearContactsList;
    wStrlist := Tstringlist.Create;
    try
      wStrlist.CommaText := AContactsList;
      wItems := Tstringlist.Create;
      try
        for i := 0 to wStrlist.count - 1 do
        begin
          wItems.CommaText := wStrlist[i];
          New(wcontact);
          wcontact.Name := wItems[0];
          wcontact.Jid := wItems[1];
          wcontact.Subscription := CompareText(wItems[2], 'none') <> 0;
          wcontact.Group := wItems[3];
          fContactsList.Add(wcontact);
        end;
      finally
        wItems.free;
      end;
    finally
      wStrlist.free;
    end;
    if assigned(fOnGettingContacts) then
      fOnGettingContacts();
  except
    On E: Exception do
      Raise Exception.Create('[TfmxJabberClient.OnGettingContactsProc] : ' +
        E.message);
  end;
end;

{ -------------------------------------------------------------------------------
  Procedure: TfmxJabberClient.OnInitConnectionProc
  Arguments: AId, ACompression: String; AUseSASL : Boolean; AMechanism : TMechanism
  Result:    None
  ------------------------------------------------------------------------------- }
procedure TfmxJabberClient.OnInitConnectionProc(AId, ACompression: String;
  AUseSASL: Boolean; AMechanism: TMechanism);
var
  wRequest: String;
begin
  if AUseSASL then
  begin
    if AMechanism <> mecNONE then
    begin
      wRequest := fXml.CreateAuthRequest(AMechanism);
      fSocket.SendRequest(wRequest);
    end;
  end;
end;

{ -------------------------------------------------------------------------------
  Procedure: TfmxJabberClient.OnIQRequest
  Arguments: AId: string
  Result:    None
  ------------------------------------------------------------------------------- }
procedure TfmxJabberClient.OnIQRequest(AId: string);
var
  wResponse: String;
begin
  try
    wResponse := fXml.CreateIQResponse(AId, fJabberServer);
    fSocket.SendRequest(wResponse);
    fLogged := True;
    if assigned(fOnLogged) then
      fOnLogged();
  except
    On E: Exception do
      Raise Exception.Create('[TfmxJabberClient.OnIQRequest] : ' + E.message);
  end;
end;

{ -------------------------------------------------------------------------------
  Procedure: TfmxJabberClient.OnMessageReceivedProc
  Arguments: AFrom, AMessage: string
  Result:    None
  ------------------------------------------------------------------------------- }
procedure TfmxJabberClient.OnMessageReceivedProc(AFrom, AMessage: string);
begin
  if assigned(fOnMessageReceived) then
    fOnMessageReceived(AFrom, AMessage);
end;

{ -------------------------------------------------------------------------------
  Procedure: TfmxJabberClient.OnPresenceCallBackProc
  Arguments: AFrom, ATo, ADisplayMessage: String; APresence: TUserStatus
  Result:    None
  ------------------------------------------------------------------------------- }
procedure TfmxJabberClient.OnPresenceCallBackProc(AFrom, ATo, ADisplayMessage
  : String; APresence: TUserStatus);
var
  i: Integer;
  wcontact: PContact;
begin // On receive a status change event
  if AFrom = ATo then // if the event concern the current user
  begin
    FUserStatus := APresence;
    FUserDisplayMsg := ADisplayMessage; // update status
    if assigned(fOnUpdateContact) then
      fOnUpdateContact('-2');
  end
  else
  begin
    for i := 0 to fContactsList.count - 1 do
    // if the event is for a ocntact from the list
    begin
      wcontact := fContactsList[i];
      if pos(uppercase(wcontact.Jid), uppercase(AFrom)) > 0 then
      begin
        wcontact.ADisplayMessage := ADisplayMessage;
        // update contacts status in the list
        wcontact.Astatus := APresence;
        if assigned(fOnUpdateContact) then
          fOnUpdateContact(wcontact.Jid);
        Break;
      end;
    end;
  end;
end;

{ -------------------------------------------------------------------------------
  Procedure: TfmxJabberClient.OnReceivedDataProc
  Arguments: AServerResponse: string
  Result:    None
  ------------------------------------------------------------------------------- }
procedure TfmxJabberClient.OnReceivedDataProc(AServerResponse: string);
begin
  try
    fXml.ParseServerResponse(AServerResponse);
  except
    On E: Exception do
      Raise Exception.Create('[TfmxJabberClient.OnReceivedDataProc] : ' +
        E.message);
  end;
end;

{ -------------------------------------------------------------------------------
  Procedure: TfmxJabberClient.OnSessionInitProc
  Arguments: None
  Result:    None
  ------------------------------------------------------------------------------- }
procedure TfmxJabberClient.OnSessionInitProc;
var
  wRequest: String;
begin
  wRequest := fXml.CreateInitSessionRequest(GetUniqueID);
  fSocket.SendRequest(wRequest);
end;

{ -------------------------------------------------------------------------------
  Procedure: TfmxJabberClient.OnSuccessSASL
  Arguments: ASASLResponse: String
  Result:    None
  ------------------------------------------------------------------------------- }
procedure TfmxJabberClient.OnSuccessSASL(ASASLResponse: String);
var
  wResponse: String;
begin
  try
    wResponse := Format(ASASLResponse, [fJabberServer]);
    fSocket.SendRequest(wResponse);
  except
    On E: Exception do
      Raise Exception.Create('[TfmxJabberClient.OnSuccessSASL] : ' + E.message);
  end;
end;

{ -------------------------------------------------------------------------------
  Procedure: TfmxJabberClient.OnUpdateRosterProc
  Arguments: AName, AJID, ASubscription, AGroup: string
  Result:    None
  ------------------------------------------------------------------------------- }
procedure TfmxJabberClient.OnUpdateRosterProc(AName, AJID, ASubscription,
  AGroup: string);
var
  i: Integer;
  wcontact: PContact;
begin
  for i := 0 to fContactsList.count - 1 do
  begin
    wcontact := Getcontact(i);
    if CompareText(wcontact.Jid, AJID) = 0 then
    begin
      wcontact.Jid := AJID;
      wcontact.Name := AName;
      wcontact.Group := AGroup;
      wcontact.Subscription := CompareText(ASubscription, 'none') <> 0;
      Break
    end;
  end;
end;

{ -------------------------------------------------------------------------------
  Procedure: TfmxJabberClient.RemoveContact  : Remove a contact
  Arguments: AContactIdx: integer
  Result:    None
  ------------------------------------------------------------------------------- }
procedure TfmxJabberClient.RemoveContact(AContactIdx: Integer);
var
  wReq: string;
  wcontact: PContact;
begin
  wcontact := fContactsList[AContactIdx];
  wReq := fXml.CreateDeleteRosterRequest(fLogin + '@' + fJabberServer,
    wcontact.Jid);
  fSocket.SendRequest(wReq); // send request to server
  Dispose(wcontact);
  fContactsList.Delete(AContactIdx); // delete him from list
end;

{ -------------------------------------------------------------------------------
  Procedure: TfmxJabberClient.SendMessage
  Arguments: ATo, AMessage: string
  Result:    None
  ------------------------------------------------------------------------------- }
procedure TfmxJabberClient.SendMessage(ATo, AMessage: string);
var
  wRequest: string;
  wFrom: string;
begin
  try
    wFrom := fLogin + '@' + fJabberServer;
    wRequest := fXml.CreateSendMessageRequest(wFrom, ATo, 'chat', AMessage);
    fSocket.SendRequest(wRequest);
  except
    On E: Exception do
      Raise Exception.Create('[TfmxJabberClient.SendMessage] : ' + E.message);
  end;
end;

{ -------------------------------------------------------------------------------
  Procedure: TfmxJabberClient.SetPresence
  Arguments: Astatus : TUserStatus; AMessage : String
  Result:    None
  ------------------------------------------------------------------------------- }
procedure TfmxJabberClient.SetPresence(Astatus: TUserStatus; AMessage: String);
var
  wRequest: string;
begin
  try
    wRequest := fXml.CreatePresenceRequest(Astatus, AMessage);
    fSocket.SendRequest(wRequest);
    FUserStatus := Astatus;
  except
    On E: Exception do
      Raise Exception.Create('[TfmxJabberClient.SetPresence] : ' + E.message);
  end;
end;

initialization

RegisterFMXClasses([TfmxJabberClient]);

end.
