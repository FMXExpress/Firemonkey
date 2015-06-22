{*******************************************************}
{                                                       }
{ This unit manage all XML message for the jabber exchange  }
{                                                       }
{       Copyright (C) 2014                       }
{                                                       }
{*******************************************************}


unit fmxJabberXml;

interface

uses classes, sysutils, OXmlPDOM, FmxJabberTools, fmx.dialogs, system.StrUtils;

type
  TfmxJabberXml = class
    private
      fUsername : String;       // Connected username
      fPassword  : String;      // password
      fServer : String;         // jabber server
      fAlreadyLogged : Boolean; // Already logged flag

      fOnInitConnection : TOnInitConnection;      //On connection initialized event
      fOnCryptedSASL : TOnResponseWithStrData;    // On decrypting SASL
      fOnSuccessSASL : TOnResponseWithStrData;    // Decrypt successful
      fOnBindInit    : TOnResponseWithNoData;     // On bind process
      fOnSessionInit : TOnResponseWithNoData;     // on session initialized
      fOnIqRequest   : TOnResponseWithStrData;    // On received an IQ request
      fOnGettingContacts : TOnResponseWithStrData; // On getting contacts list
      fOnFaillure    : TOnResponseWithStrData;     // On faillure from server
      fOnUpdateRoster : TOnUpdateRoster;           // On contact informations updated
      fOnAskToAddRosterStatus : TOnAddRequestStatus;  // On asked to add a contact
      fOnAskedForSubscription : TOnResponseWithStrData; // when a contact ask to add you
      fOnMessageReceived : TOnMessageReceived;       // On received a message from a contact
      fOnPresenceCallback : TOnPresenceCallBack;     // On received a status change from a contact

      procedure ProcessStreamStreamResponse(AResponse : PxmlNode);     // Process StreamStream Response
      procedure ProcessStreamFeaturesResponse(AResponse : PxmlNode);   // process stream:feature Response

      procedure ProcessSASLResponse(AResponse: PxmlNode);             // process SASL Response
      procedure ProcessSASLSuccessResponse(AResponse : PxmlNode);     // process SASL success response
      procedure ProcessIQResponse(AResponse : PxmlNode);              // process IQ Request
      procedure ProcessFaillureResponse(AResponse : PxmlNode);        // process faillure Response
      procedure ProcessMessageResponse(AResponse : PxmlNode);         // process Message Response
      procedure ProcessPresenceResponse(AResponse : PxmlNode);        // process presence  Response
      function  GetMechanism(AMchanismsNode : PXMLNode) : TMechanism; // Get mechanism to use for authentification from server response
    public
      Constructor Create;
      Destructor Destroy;Override;
       //Return requests as xml
      function CreateInitRequest(AServer : string) : String;
      function CreateAuthRequest(Amechanism : TMechanism) : string;
      function CreateSASLRequest(ACrypted : String) : String;
      function CreateResourceRequest(AResource : String) : string;
      function CreateInitBindRequest(AResource : String) : String;
      function CreateInitSessionRequest(AUniqueID : String) : String;
      function CreatePresenceRequest(Astatus : TUserStatus; AMsg : String) : string;
      function CreateIQResponse(AId, AServer: string): String;
      function CreateGetcontactsRequest : String;
      function CreateAddContactRequest(AFrom, AUsername, AJID, AGroup : String) : string;
      function CreateSendMessageRequest(AFrom, ATo, AType,  ABody: string): string;
      function CreateDeleteRosterRequest(AFrom, ATo : String) : string;

      //Parse server response
      procedure ParseServerResponse(AXmlResponse : string);

      //public roperties
      property OnInitConnection : TOnInitConnection read fOnInitConnection write fOnInitConnection;
      property OnCryptedSASL : TOnResponseWithStrData read fOnCryptedSASL write fOnCryptedSASL;
      property OnSuccessSASL : TOnResponseWithStrData read fOnSuccessSASL write fOnSuccessSASL;
      property OnBindInit    : TOnResponseWithNoData read fOnBindInit write fOnBindInit;
      property OnSessionInit : TOnResponseWithNoData read fOnSessionInit write fOnSessionInit;
      property OnIqRequest   : TOnResponseWithStrData read fOnIqRequest write fOnIqRequest;
      property OnGettingContacts : TOnResponseWithStrData read fOnGettingContacts write fOnGettingContacts;
      property OnFaillure    : TOnResponseWithStrData read fOnFaillure write fOnFaillure;
      property OnUpdateRoster  : TOnUpdateRoster read fOnUpdateRoster write fOnUpdateRoster;
      property OnAskToAddRosterStatus : TOnAddRequestStatus read fOnAskToAddRosterStatus write fOnAskToAddRosterStatus;
      property OnAskedForSubscription : TOnResponseWithStrData read fOnAskedForSubscription write fOnAskedForSubscription;
      property OnMessageReceived : TOnMessageReceived read fOnMessageReceived write fOnMessageReceived;
      property OnPresenceCallback : TOnPresenceCallBack read fOnPresenceCallback write fOnPresenceCallback;

      property Username : String read fUsername;
      property Password : String read fPassword;
      property Server   : String read fServer;

  end;

implementation

{ TfmxJabberXml }

constructor TfmxJabberXml.Create;
begin
end;

destructor TfmxJabberXml.Destroy;
begin
  inherited;
end;


{-------------------------------------------------------------------------------
  Procedure: TfmxJabberXml.GetMechanism
  Arguments: AMchanismsNode: PXMLNode
  Result:    TMechanism
-------------------------------------------------------------------------------}
function TfmxJabberXml.GetMechanism(AMchanismsNode: PXMLNode): TMechanism;
var i : integer;
    wStrList : TStringlist;
begin
  try
    result := mecNONE;
    wStrList := TStringList.Create;
    try
      for i := 0 to AMchanismsNode.ChildNodes.Count -1 do
        wStrList.Add(AMchanismsNode.ChildNodes[i].ChildNodes[0].NodeValue);

      if wStrList.IndexOf('DIGEST-MD5') <> -1 then
        result := mecDIGEST_MD5
      else if wStrList.IndexOf('PLAIN') <> -1 then
        result := mecPLAIN;
    finally
      wStrList.free;
    end;
  except
    On E:Exception do
      Raise Exception.create('[TfmxJabberXml.GetMechanism] : '+E.message);
  end;
end;


{-------------------------------------------------------------------------------
  Procedure: TfmxJabberXml.CreateAddContactRequest
  Arguments: AFrom, AUsername, AJID, AGroup: String
  Result:    string
-------------------------------------------------------------------------------}
function TfmxJabberXml.CreateAddContactRequest(AFrom, AUsername, AJID, AGroup: String): string;
begin
  Result := '<presence to="' + AJID + '" type="subscribe"/>'
end;


{-------------------------------------------------------------------------------
  Procedure: TfmxJabberXml.CreateAuthRequest
  Arguments: Amechanism: TMechanism
  Result:    string
-------------------------------------------------------------------------------}
function TfmxJabberXml.CreateAuthRequest(Amechanism: TMechanism): string;
var wMechanismStr : string;
begin
  case Amechanism of
    mecDIGEST_MD5: wMechanismStr := 'DIGEST-MD5';
    mecPLAIN: wMechanismStr := 'PLAIN';
  end;
  Result := ('<auth xmlns="urn:ietf:params:xml:ns:xmpp-sasl" mechanism="' + wMechanismStr +  '"/>');
end;


{-------------------------------------------------------------------------------
  Procedure: TfmxJabberXml.CreateDeleteRosterRequest
  Arguments: Afrom, ATo: String
  Result:    string
-------------------------------------------------------------------------------}
function TfmxJabberXml.CreateDeleteRosterRequest(Afrom, ATo: String): string;
begin
  Result := '<iq from="' + Afrom +'" type="set" id="roster_4"><query xmlns="jabber:iq:roster"><item jid="'+ATo+'" subscription="remove"/></query></iq>';
end;


{-------------------------------------------------------------------------------
  Procedure: TfmxJabberXml.CreateGetcontactsRequest
  Arguments: None
  Result:    String
-------------------------------------------------------------------------------}
function TfmxJabberXml.CreateGetcontactsRequest: String;
begin
  Result := '<iq type="get" id="roster_1" ><query xmlns="jabber:iq:roster"/></iq>';
end;


{-------------------------------------------------------------------------------
  Procedure: TfmxJabberXml.CreateInitBindRequest
  Arguments: AResource : String
  Result:    String
-------------------------------------------------------------------------------}
function TfmxJabberXml.CreateInitBindRequest(AResource : String): String;
begin
  Result := '<iq type="set" id="bind_1"><bind xmlns="urn:ietf:params:xml:ns:xmpp-bind"><resource>' + AResource +'</resource></bind></iq>';
end;


{-------------------------------------------------------------------------------
  Procedure: TfmxJabberXml.CreateInitRequest
  Arguments: AServer : string
  Result:    String
-------------------------------------------------------------------------------}
function TfmxJabberXml.CreateInitRequest(AServer : string): String;
begin
  fAlreadyLogged := False;
  Result := '<?xml version="1.0"?><stream:stream xmlns:stream="http://etherx.jabber.org/streams" xmlns="jabber:client" to="' + AServer + '" version="1.0" xml:lang="en" xmlns:xml="http://www.w3.org/XML/1998/namespace">';
end;


{-------------------------------------------------------------------------------
  Procedure: TfmxJabberXml.CreateInitSessionRequest
  Arguments: AUniqueID: String
  Result:    String
-------------------------------------------------------------------------------}
function TfmxJabberXml.CreateInitSessionRequest(AUniqueID: String): String;
begin
  Result := '<iq type="set" id="'+ Trim(AUniqueID) +'"><session xmlns="urn:ietf:params:xml:ns:xmpp-session"/></iq>';
end;


{-------------------------------------------------------------------------------
  Procedure: TfmxJabberXml.CreateIQResponse
  Author:    kelhedadi
  DateTime:  2014.04.18
  Arguments: AId, AServer: string
  Result:    String
-------------------------------------------------------------------------------}
function TfmxJabberXml.CreateIQResponse(AId, AServer: string): String;
begin
  Result := '<iq type="result" to="'+AServer+'" id="'+AId+'"/>';
end;


{-------------------------------------------------------------------------------
  Procedure: TfmxJabberXml.CreatePresenceRequest
  Arguments: Astatus: TUserStatus; AMsg : String
  Result:    string
-------------------------------------------------------------------------------}
function TfmxJabberXml.CreatePresenceRequest( Astatus: TUserStatus; AMsg : String): string;
var wStatutsStr : String;
begin
  wStatutsStr := '';
  case Astatus of
    usOnline       : wStatutsStr := '';
    usAway         : wStatutsStr := 'away';
    usDNotDistrub  : wStatutsStr := 'dnd';
    usFreeForChat  : wStatutsStr := 'chat';
    usExtendedAway : wStatutsStr := 'xa';
  end;
  Result := '<presence><show>' + wStatutsStr + '</show><priority>0</priority><Status>' + AMsg + '</Status></presence>'
end;


{-------------------------------------------------------------------------------
  Procedure: TfmxJabberXml.CreateResourceRequest
  Arguments: AResource: String
  Result:    string
-------------------------------------------------------------------------------}
function TfmxJabberXml.CreateResourceRequest(AResource: String): string;
begin
  Result := '<iq type="set" id="session_1"><session xmlns="urn:ietf:params:xml:ns:xmpp-session"/></iq>';
end;


{-------------------------------------------------------------------------------
  Procedure: TfmxJabberXml.CreateSASLRequest
  Arguments: ACrypted: String
  Result:    String
-------------------------------------------------------------------------------}
function TfmxJabberXml.CreateSASLRequest(ACrypted: String): String;
begin
  try
    if ACrypted <> '' then
      result := '<response xmlns="urn:ietf:params:xml:ns:xmpp-sasl">' + ACrypted + '</response>'
    else
      Result := '<response xmlns="urn:ietf:params:xml:ns:xmpp-sasl"/>';
  except
    On E:Exception do
      Raise Exception.create('[TfmxJabberXml.CreateSASLRequest] : '+E.message);
  end;
end;


{-------------------------------------------------------------------------------
  Procedure: TfmxJabberXml.CreateSendMessageRequest
  Arguments: AFrom, ATo, AType, ABody: string
  Result:    string
-------------------------------------------------------------------------------}
function TfmxJabberXml.CreateSendMessageRequest(AFrom, ATo, AType,  ABody: string): string;
begin
  Result :=  Format('<message to="%s" from="%s" type="%s" id="%s"><body>%s</body></message>',
                    [ATo, AFrom, AType,GetUniqueID ,ABody]);
end;



{-------------------------------------------------------------------------------
  Procedure: TfmxJabberXml.ParseServerResponse
  Arguments: AXmlResponse: string
  Result:    None
-------------------------------------------------------------------------------}
procedure TfmxJabberXml.ParseServerResponse(AXmlResponse: string);
var wXml : OXmlPDOM.IXMLDocument;
    wFixedXml : String;
    wNode : PXMLNode;
    i : integer;
begin
  try
    if pos('<?xml', AXmlResponse) > 0 then
      AXmlResponse := Copy(AXmlResponse, 39,99999);

    wFixedXml := '<?xml version="1.0" encoding="UTF-8"?><Jab>' + AXmlResponse;
    if Pos('stream:stream',wFixedXml) > 0 then
       wFixedXml := wFixedXml + '</stream:stream></Jab>'
    else
       wFixedXml := wFixedXml + '</Jab>';
  //  showmessage('Received : ' + #13 + AXmlResponse);

    wXml := CreateXMLDoc;
    try
      wXml.LoadFromXML(wFixedXml);
      for i := 0 to wXml.DocumentElement.ChildCount -1 do
      begin
        wNode := wXml.DocumentElement.ChildNodes[i];
        if CompareText(wNode.NodeName, 'stream:stream') = 0 then
          ProcessStreamStreamResponse( wNode )
        else  if CompareText(wNode.NodeName, 'stream:features') = 0 then
          ProcessStreamFeaturesResponse( wNode )
        else if CompareText(wNode.NodeName, 'challenge') = 0 then
        begin
          ProcessSaSLResponse(wNode);
        end
        else if CompareText(wNode.NodeName, 'success') = 0 then
        begin
          ProcessSASLSuccessResponse(wNode);
        end
        else if CompareText(wNode.Attributes['id'],'bind_1') = 0 then
        begin
          if Assigned(fOnSessionInit) then
            fOnSessionInit();
        end
        else if CompareText(wNode.NodeName, 'iq') = 0 then
        begin
          ProcessIQResponse(wNode);
        end
        else if CompareText(wNode.NodeName, 'presence') = 0 then
        begin
          ProcessPresenceResponse(wNode);
        end
        else if CompareText(wNode.NodeName, 'message') = 0 then
        begin
          ProcessMessageResponse(wNode);
        end
        else if CompareText(wNode.NodeName, 'failure') = 0 then
        begin
          ProcessFaillureResponse(wNode);
        end
      end;
    finally
      wXml := nil;
    end;

  except
    On E:Exception do
      Raise Exception.create('[TfmxJabberXml.ParseServerResponse] : '+E.message);
  end;

end;


{-------------------------------------------------------------------------------
  Procedure: TfmxJabberXml.ProcessStreamFeaturesResponse
  Arguments: AResponse: PxmlNode
  Result:    None
-------------------------------------------------------------------------------}
procedure TfmxJabberXml.ProcessStreamFeaturesResponse(AResponse: PxmlNode);
var
  wId : string;
  wFeaturesNode : PxmlNode;
  wUseSASL : Boolean;
  wCompression : string;
  wMechanism : TMechanism;
  wNode : PxmlNode;
begin
  try
    wUseSASL := False;
    wMechanism := mecNONE;
    wId := AResponse.Attributes['id'];
    wFeaturesNode := AResponse;
    if wFeaturesNode.SelectNode('./mechanisms',wNode) then
    begin
      wUseSASL := CompareText(wNode.Attributes['xmlns'],XMLNS_XMPP_SASL) = 0;
      wMechanism := GetMechanism(wNode);
      wMechanism := GetMechanism(wNode);
    end;

    if wFeaturesNode.SelectNode('./compression',wNode) then
      wCompression := wNode.ChildNodes[0].NodeValue;

    if wFeaturesNode.SelectNode('./mechanisms',wNode) then
    begin
      if Assigned(fOnInitConnection) then
        fOnInitConnection(wId,wCompression, wUseSASL, wMechanism);
    end;
    if wFeaturesNode.SelectNode('./bind',wNode) then
    begin
      if Assigned(fOnBindInit) then
        fOnBindInit();
    end;
  except
    On E:Exception do
      Raise Exception.create('[TfmxJabberXml.ProcessStreamFeaturesResponse] : '+E.message);
  end;
end;


{-------------------------------------------------------------------------------
  Procedure: TfmxJabberXml.ProcessStreamStreamResponse
  Arguments: AResponse: PxmlNode
  Result:    None
-------------------------------------------------------------------------------}
procedure TfmxJabberXml.ProcessStreamStreamResponse(AResponse: PxmlNode);
var
  wFeaturesNode : PxmlNode;
begin
  try
    if AResponse.ChildNodes.Count > 0 then
    begin
      if AResponse.SelectNode('./stream:features',wFeaturesNode) then
      begin
        ProcessStreamFeaturesResponse(wFeaturesNode);
      end
    end
  except
    On E:Exception do
      Raise Exception.create('[TfmxJabberXml.ProcessInitResponse] : '+E.message);
  end;
end;


{-------------------------------------------------------------------------------
  Procedure: TfmxJabberXml.ProcessFaillureResponse
  Arguments: AResponse: PxmlNode
  Result:    None
-------------------------------------------------------------------------------}
procedure TfmxJabberXml.ProcessFaillureResponse(AResponse: PxmlNode);
var wRootNode : PxmlNode;
begin

  wRootNode := AResponse;
  if wRootNode.ChildNodes.Count > 0 then
  begin
    if CompareText(wRootNode.ChildNodes[0].NodeName,'not-authorized') = 0 then
      if assigned(fOnFaillure) then
        fOnFaillure('Authentification failed');
  end;
end;


{-------------------------------------------------------------------------------
  Procedure: TfmxJabberXml.ProcessIQResponse
  Arguments: AResponse: PxmlNode
  Result:    None
-------------------------------------------------------------------------------}
procedure TfmxJabberXml.ProcessIQResponse(AResponse: PxmlNode);
  procedure GetContactData( ANode : PXMLNode; var Aname,AJID,ASubscription,AGroup : String);
  begin
    Aname := ANode.Attributes['name'];
    AJID := ANode.Attributes['jid'];
    ASubscription := ANode.Attributes['subscription'];
    if ANode.ChildCount > 0  then
      AGroup  := ANode.nodevalue; // read group
  end;
var
  wId, wType : String;
  i,k : integer;
  wstrList : TStringList;
  wName, wJid, wSubscription : String;
  wGroup : string;
  wContact : String;
  wContacts : String;
  wQueryNode : PXMLNode;
begin
  try

      wId := AResponse.Attributes['id'];
      wType := AResponse.Attributes['type'];
      if (CompareText(wId,'roster_2') = 0) and (CompareText(wType,'result') = 0) then
      begin
       // GetContactData(  AResponse.ChildNodes[0].ChildNodes[0], wName, wJID, wSubscription, wGroup );
       // if assigned(fOnUpdateRoster) then
       //   fOnUpdateRoster(wName, wJID, wSubscription, wGroup);
      end
      else if (CompareText(wId,'roster_1') = 0) and (CompareText(wType,'result') = 0) then
      begin
        // getting contacts list
        wstrList := TStringList.Create;
        try
          for i := 0 to AResponse.ChildNodes[0].ChildCount -1 do
          begin
            wGroup := '';
            GetContactData(  AResponse.ChildNodes[0].ChildNodes[i], wName, wJID, wSubscription, wGroup );
            wContact := Format('"%s","%s","%s","%s"',[wName, wJID, wSubscription, wGroup]);
            wstrList.Add(wContact);
          end;
          wContacts := wstrList.CommaText;
        finally
          wstrList.Free;
        end;
        if Assigned(fOnGettingContacts) then
          fOnGettingContacts(wContacts);
      end
      else if (CompareText(wType,'set') = 0) then
      begin
        wQueryNode := AResponse.ChildNodes[0];
        if CompareText(wQueryNode.nodename,'Query') = 0 then
        begin
          if CompareText(wQueryNode.Attributes['xmlns'],'jabber:iq:roster') = 0 then
          begin
            for k := 0 to wQueryNode.ChildCount -1 do
            begin
          //    if wQueryNode.ChildNodes[k].HasAttribute('ask') then
          //    begin
          //      if assigned(fOnAskForSubscription) then
          //        fOnAskForSubscription(wQueryNode.ChildNodes[k].Attributes['jid']);
          //    end
          //    else
          //   if wQueryNode.ChildNodes[k].HasAttribute('subscription') then
          //    begin
          //      wGroup := '';
           //     GetContactData(wQueryNode.ChildNodes[k], wName, wJID, wSubscription, wGroup );
           //     if Comparetext(wSubscription , 'from') = 0 then
           //       if assigned(fOnAddRoster) then
          //          fOnAddRoster(wName, wJID, wSubscription, wGroup);
          //    end;
            end;
          end;
        end
        else begin
          for i := 0 to AResponse.ChildNodes[0].ChildCount -1 do
          begin
          end;
        end;
      end
      else if pos('jabber:iq:roster', AResponse.xml) = 0 then
      begin
        // last authentification IQstep
        if assigned(fOnIqRequest) then
          fOnIqRequest(wId);
      end
  except
    On E:Exception do
      Raise Exception.create('[TfmxJabberXml.ProcessIQResponse] : '+E.message);
  end;
end;


{-------------------------------------------------------------------------------
  Procedure: TfmxJabberXml.ProcessMessageResponse
  Arguments: AResponse: PxmlNode
  Result:    None
-------------------------------------------------------------------------------}
procedure TfmxJabberXml.ProcessMessageResponse(AResponse: PxmlNode);
var wFrom : String;
    wMessage : String;
begin

  wFrom := AResponse.Attributes['from'];
  wMessage := AResponse.ChildNodes[0].ChildNodes[0].NodeValue;
  if Assigned(fOnMessageReceived) then
    fOnMessageReceived(wFrom, wMessage);
end;


{-------------------------------------------------------------------------------
  Procedure: TfmxJabberXml.ProcessPresenceResponse
  Arguments: AResponse: PxmlNode
  Result:    None
-------------------------------------------------------------------------------}
procedure TfmxJabberXml.ProcessPresenceResponse(AResponse: PxmlNode);
var
    wFrom : string;
    wTo : string;
    wMessage : string;
    wStatus : TUserStatus;
    wShow : string;

begin
  wStatus := usInVisible;

    if CompareText(AResponse.NodeName, 'presence') = 0 then
    begin
      wFrom := AResponse.Attributes['from'];
      wTo   := AResponse.Attributes['to'];
      if AResponse.Attributes['type'] ='subscribe' then
      begin
        if assigned(fOnAskedForSubscription) then
          fOnAskedForSubscription(wFrom);
      end
      else if AResponse.Attributes['type'] ='subscribed' then
      begin
        if Assigned(fOnAskToAddRosterStatus) then
          fOnAskToAddRosterStatus(wFrom, True);
      end
      else if AResponse.Attributes['type'] ='unsubscribed' then
      begin
        if Assigned(fOnAskToAddRosterStatus) then
          fOnAskToAddRosterStatus(wFrom, False);
      end;

      if AResponse.SelectNode('Status') <> nil then
        if AResponse.SelectNode('Status').HasChildNodes then
          wMessage := AResponse.SelectNode('Status').childnodes[0].nodevalue;
      if AResponse.SelectNode('show') <> nil then
        if AResponse.SelectNode('show').HasChildNodes then
          wShow := AResponse.SelectNode('show').childnodes[0].nodevalue;

      if wShow = '' then  wStatus := usOnline
      else if wShow = 'away' then  wStatus := usAway
      else if wShow = 'dnd' then  wStatus := usDNotDistrub
      else if wShow = 'chat' then  wStatus := usFreeForChat
      else if wShow = 'xa' then  wStatus := usExtendedAway;
    end;
    if assigned(fOnPresenceCallback) then
      fOnPresenceCallback(wFrom, wTo,wMessage, wStatus);
end;


{-------------------------------------------------------------------------------
  Procedure: TfmxJabberXml.ProcessSASLResponse
  Arguments: AResponse: PxmlNode
  Result:    None
-------------------------------------------------------------------------------}
procedure TfmxJabberXml.ProcessSASLResponse(AResponse: PxmlNode);
var
  wCrypted : String;
begin
  try

    wCrypted := AResponse.ChildNodes[0].NodeValue;
    if Assigned(fOnCryptedSASL) then
      fOnCryptedSASL(wCrypted);
  except
    On E:Exception do
      Raise Exception.create('[TfmxJabberXml.ProcessSASLResponse] : '+E.message);
  end;
end;


{-------------------------------------------------------------------------------
  Procedure: TfmxJabberXml.ProcessSASLSuccessResponse
  Arguments: AResponse: PxmlNode
  Result:    None
-------------------------------------------------------------------------------}
procedure TfmxJabberXml.ProcessSASLSuccessResponse(AResponse: PxmlNode);
begin
  try
    fAlreadyLogged := True;

    if CompareText(AResponse.Attributes['xmlns'] , XMLNS_XMPP_SASL ) = 0 then
      if Assigned(fOnSuccessSASL) then
        fOnSuccessSASL('<stream:stream xmlns="jabber:client" xmlns:stream="http://etherx.jabber.org/streams" to="%s" version="1.0">');
  except
    On E:Exception do
      Raise Exception.create('[TfmxJabberXml.ProcessSASLSuccessResponse] : '+E.message);
  end;
end;

end.
