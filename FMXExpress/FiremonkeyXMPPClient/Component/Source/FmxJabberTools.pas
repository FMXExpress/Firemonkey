{*******************************************************}
{                                                       }
{ This unit describe all custom types and event declaration }
{                                                       }
{       Copyright (C) 2014                              }
{                                                       }
{*******************************************************}


unit FmxJabberTools;

interface

uses Classes, system.SysUtils, IdHashMessageDigest;

Type
  TMechanism = (mecDIGEST_MD5, mecPLAIN, mecNONE);          //Encryption mechanisms  for jabber protocole
  TUserStatus = (usOnline, usAway, usDNotDistrub, usFreeForChat,usInVisible, usExtendedAway);   // user status

  TContact = record       // Record defining a contact
    Jid : String;          // his jaber ID
    Name : String;         // his name
    Group : string;        // his group
    Subscription : Boolean;
    ADisplayMessage : string;    // Displayed message
    AStatus : TUserStatus;       // Connection status
  end;
  PContact = ^TContact;

  TOnResponseWithStrData  = procedure(AData : string) of Object;  //event defining a response with a string paramter
  TonResponseWithBoolData = procedure(AStatus : Boolean) of object; //event defining a response with a boolean paramter
  TOnInitConnection       = procedure(AId : string; ACompression : String; AUseSASL : Boolean; AMechanism : TMechanism) of Object;
  TOnResponseWithNoData   = procedure of Object;   //event defining a response with no paramter
  TOnUpdateRoster         = procedure (AName, JID, ASubscription, AGroup : string) of object;
  TOnMessageReceived      = procedure (AFrom, AMessage : string) of object;  // On receive a message
  TOnPresenceCallBack     = procedure (AFrom, ATo, AStatus: String; APresence: TUserStatus) of Object;   // On presence status for a contact change
  TOnContactAskToAdd      = procedure (AJid : String; var AAccept : Boolean) of Object; // When someone add you as a contact
  TOnAddRequestStatus     = procedure (AFrom : String; AAccepted : Boolean) of object;  //Own request to add a contact


Const   // not all consts used (TODO : replace all direct string in the code by const name
  XMLNS_ROSTER     = 'jabber:iq:roster';
  XMLNS_REGISTER   = 'jabber:iq:register';
  XMLNS_LAST       = 'jabber:iq:last';
  XMLNS_TIME       = 'jabber:iq:time';
  XMLNS_VERSION    = 'jabber:iq:version';
  XMLNS_IQOOB      = 'jabber:iq:oob';
  XMLNS_BROWSE     = 'jabber:iq:browse';
  XMLNS_AGENTS     = 'jabber:iq:agents';
  XMLNS_SEARCH     = 'jabber:iq:search';
  XMLNS_PRIVATE    = 'jabber:iq:private';
  XMLNS_CONFERENCE = 'jabber:iq:conference';

  XMLNS_BM         = 'storage:bookmarks';
  XMLNS_PREFS      = 'storage:imprefs';

  XMLNS_XEVENT     = 'jabber:x:event';
  XMLNS_DELAY      = 'jabber:x:delay';
  XMLNS_XROSTER    = 'jabber:x:roster';
  XMLNS_XCONFERENCE= 'jabber:x:conference';
  XMLNS_XDATA      = 'jabber:x:data';
  XMLNS_XOOB       = 'jabber:x:oob';

  XMLNS_MUC        = 'http://jabber.org/protocol/muc';
  XMLNS_MUCOWNER   = 'http://jabber.org/protocol/muc#owner';
  XMLNS_MUCADMIN   = 'http://jabber.org/protocol/muc#admin';
  XMLNS_MUCUSER    = 'http://jabber.org/protocol/muc#user';

  XMLNS_DISCO      = 'http://jabber.org/protocol/disco';
  XMLNS_DISCOITEMS = 'http://jabber.org/protocol/disco#items';
  XMLNS_DISCOINFO  = 'http://jabber.org/protocol/disco#info';

  XMLNS_SI         = 'http://jabber.org/protocol/si';
  XMLNS_FTPROFILE  = 'http://jabber.org/protocol/si/profile/file-transfer';
  XMLNS_BYTESTREAMS= 'http://jabber.org/protocol/bytestreams';
  XMLNS_FEATNEG    = 'http://jabber.org/protocol/feature-neg';

  XMLNS_CLIENTCAPS = 'http://jabber.org/protocol/caps';

  XMLNS_STREAMERR  = 'urn:ietf:params:xml:ns:xmpp-stanzas';
  XMLNS_XMPP_SASL  = 'urn:ietf:params:xml:ns:xmpp-sasl';
  XMLNS_XMPP_BIND  = 'urn:ietf:params:xml:ns:xmpp-bind';
  XMLNS_XMPP_SESSION  = 'urn:ietf:params:xml:ns:xmpp-session';
  XMLNS_COMMANDS   = 'http://jabber.org/protocol/commands';
  XMLNS_CAPS       = 'http://jabber.org/protocol/caps';
  XMLNS_ADDRESS    = 'http://jabber.org/protocol/address';

  XMLNS_XHTMLIM    = 'http://jabber.org/protocol/xhtml-im';
  XMLNS_XHTML      = 'http://www.w3.org/1999/xhtml';
  XMLNS_SHIM       = 'http://jabber.org/protocol/shim';


  function GetUniqueID: String;    // Get a unique ID string
  function ShaHASH(Fkey: String): String; // HASH function using indy


implementation



{-------------------------------------------------------------------------------
  Procedure: GetUniqueID
  Arguments: None
  Result:    String
-------------------------------------------------------------------------------}
function GetUniqueID: String;
var
  IntTime: Double;
  StrHex: String;
begin
  IntTime := Double(Now) + Random(1000);;
  StrHex := (FloatToStr(IntTime));
  StrHex := ShaHASH(StrHex);
  StrHex := copy(strhex, 1, 10);
  Result := StrHex;
end;



{-------------------------------------------------------------------------------
  Procedure: ShaHASH
  Arguments: Fkey: String
  Result:    String
-------------------------------------------------------------------------------}
function ShaHASH(Fkey: String): String;
var
  fdig, fdigest: string;
  _hasher: TIdHashMessageDigest5;
begin
  fkey := Trim(fkey);
  _hasher := TIdHashMessageDigest5.Create;
  fdigest := _hasher.HashStringAsHex(Fkey);
  FreeAndNil(_hasher);
  fdig := Lowercase(fdigest);
  Result := fdig;
end;

end.
