{==============================================================================|
| Project : Bauglir Internet Library                                           |
|==============================================================================|
| Content: Generic connection and server                                       |
|==============================================================================|
| Copyright (c)2011-2012, Bronislav Klucka                                     |
| All rights reserved.                                                         |
| Source code is licenced under original 4-clause BSD licence:                 |
| http://licence.bauglir.com/bsd4.php                                          |
|                                                                              |
|                                                                              |
| Project download homepage:                                                   |
|   http://code.google.com/p/bauglir-websocket/                                |
| Project homepage:                                                            |
|   http://www.webnt.eu/index.php                                              |
| WebSocket RFC:                                                               |
|   http://tools.ietf.org/html/rfc6455                                         |
|                                                                              |
|                                                                              |
|==============================================================================|
| Requirements: Ararat Synapse (http://www.ararat.cz/synapse/)                 |
|==============================================================================}



{

2.0.4
1/ change: send generic frame SendData public on WSConnection
2/ pascal bugfix: closing connection issues (e.g. infinite sleep)
3/ add: server CloseAllConnections
4/ change: default client version 13 (RFC)
5/ pascal change: CanReceiveOrSend public
6/ pascal bugfix: events missing on erratic traffic
7/ add: make Handschake public property


@todo
* move writing to separate thread
* test for simultaneous i/o operations



http://tools.ietf.org/html/draft-ietf-hybi-thewebsocketprotocol-17
http://tools.ietf.org/html/rfc6455
http://dev.w3.org/html5/websockets/#refsFILEAPI

}



unit WebSocket2;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$H+}

interface

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, blcksock, syncobjs,
  CustomServer2;

const
  {:Constants section defining what kind of data are sent from one pont to another}
  {:Continuation frame }
  wsCodeContinuation = $0;
  {:Text frame }
  wsCodeText         = $1;
  {:Binary frame }
  wsCodeBinary       = $2;
  {:Close frame }
  wsCodeClose        = $8;
  {:Ping frame }
  wsCodePing         = $9;
  {:Frame frame }
  wsCodePong         = $A;


 {:Constants section defining close codes}
 {:Normal valid closure, connection purpose was fulfilled}
 wsCloseNormal              = 1000;
 {:Endpoint is going away (like server shutdown) }
 wsCloseShutdown            = 1001;
 {:Protocol error }
 wsCloseErrorProtocol       = 1002;
 {:Unknown frame data type or data type application cannot handle }
 wsCloseErrorData           = 1003;
 {:Reserved }
 wsCloseReserved1           = 1004;
 {:Close received by peer but without any close code. This close code MUST NOT be sent by application. }
 wsCloseNoStatus            = 1005;
 {:Abnotmal connection shutdown close code. This close code MUST NOT be sent by application. }
 wsCloseErrorClose          = 1006;
 {:Received text data are not valid UTF-8. }
 wsCloseErrorUTF8           = 1007;
 {:Endpoint is terminating the connection because it has received a message that violates its policy. Generic error. }
 wsCloseErrorPolicy         = 1008;
 {:Too large message received }
 wsCloseTooLargeMessage     = 1009;
 {:Client is terminating the connection because it has expected the server to negotiate one or more extension, but the server didn't return them in the response message of the WebSocket handshake }
 wsCloseClientExtensionError= 1010;
 {:Server is terminating the connection because it encountered an unexpected condition that prevented it from fulfilling the request }
 wsCloseErrorServerRequest  = 1011;
 {:Connection was closed due to a failure to perform a TLS handshake. This close code MUST NOT be sent by application. }
 wsCloseErrorTLS            = 1015;
type
  TWebSocketCustomConnection = class;

  {:Event procedural type to hook OnOpen events on connection
  }
  TWebSocketConnectionEvent = procedure (aSender: TWebSocketCustomConnection) of object;

  {:Event procedural type to hook OnPing, OnPong events on connection

  TWebSocketConnectionPingPongEvent = procedure (aSender: TWebSocketCustomConnection; aData: string) of object;
  }
  {:Event procedural type to hook OnClose event on connection
  }
  TWebSocketConnectionClose = procedure (aSender: TWebSocketCustomConnection; aCloseCode: integer; aCloseReason: string; aClosedByPeer: boolean) of object;

  {:Event procedural type to hook OnRead on OnWrite event on connection
  }
  TWebSocketConnectionData = procedure (aSender: TWebSocketCustomConnection; aFinal, aRes1, aRes2, aRes3: boolean; aCode: integer; aData: TMemoryStream) of object;
  
  {:Event procedural type to hook OnReadFull
  }
  TWebSocketConnectionDataFull = procedure (aSender: TWebSocketCustomConnection; aCode: integer; aData: TMemoryStream) of object;

  {:abstract(WebSocket connection)
    class is parent class for server and client connection 
  }
  TWebSocketCustomConnection = class(TCustomConnection)
  private


  protected

    fOnRead: TWebSocketConnectionData;
    fOnReadFull: TWebSocketConnectionDataFull;
    fOnWrite: TWebSocketConnectionData;
    fOnClose: TWebSocketConnectionClose;
    fOnOpen: TWebSocketConnectionEvent;
    //fOnPing: TWebSocketConnectionPingPongEvent;
    //fOnPong: TWebSocketConnectionPingPongEvent;

    fCookie: string;
    fVersion: integer;
    fProtocol: string;
    fResourceName: string;
    fOrigin: string;
    fExtension: string;
    fPort: string;
    fHost: string;
    fHeaders: TStringList;


    fClosedByMe: boolean;
    fClosedByPeer: boolean;
    fMasking: boolean;
    fRequireMasking: boolean;
    fHandshake: boolean;


    fCloseCode: integer;
    fCloseReason: string;
    fClosingByPeer: boolean;


    fReadFinal: boolean;
    fReadRes1: boolean;
    fReadRes2: boolean;
    fReadRes3: boolean;
    fReadCode: integer;
    fReadStream: TMemoryStream;

    fWriteFinal: boolean;
    fWriteRes1: boolean;
    fWriteRes2: boolean;
    fWriteRes3: boolean;
    fWriteCode: integer;
    fWriteStream: TMemoryStream;

    fSendCriticalSection: TCriticalSection;

    fFullDataProcess: boolean;
    fFullDataStream: TMemoryStream;

    function GetClosed: boolean;
    function GetClosing: boolean;




    procedure ExecuteConnection; override;
    function ReadData(var aFinal, aRes1, aRes2, aRes3: boolean; var aCode: integer; aData: TMemoryStream): integer; virtual;
    function ValidConnection: boolean;


    procedure DoSyncClose;
    procedure DoSyncOpen;
    //procedure DoSyncPing;
    //procedure DoSyncPong;
    procedure DoSyncRead;
    procedure DoSyncReadFull;
    procedure DoSyncWrite;

    procedure SyncClose;
    procedure SyncOpen;
    //procedure SyncPing;
    //procedure SyncPong;
    procedure SyncRead;
    procedure SyncReadFull;
    procedure SyncWrite;

    {:Overload this function to process connection close (not at socket level, but as an actual WebSocket frame)
      aCloseCode represents close code (see wsClose constants)
      aCloseReason represents textual information transfered with frame (there is no specified format or meaning)
      aClosedByPeer whether connection has been closed by this connection object or by peer endpoint
    }
    procedure ProcessClose(aCloseCode: integer; aCloseReason: string; aClosedByPeer: boolean); virtual;


    {:Overload this function to process data as soon as they are read before other Process<data> function is called
      this function should be used by extensions to modify incomming data before the are process based on code
    }
    procedure ProcessData(var aFinal: boolean; var aRes1: boolean; var aRes2: boolean; var aRes3: boolean; var aCode: integer; aData: TMemoryStream); virtual;


    {:Overload this function to process ping frame)
      aData represents textual information transfered with frame (there is no specified format or meaning)
    }
    procedure ProcessPing(aData: string); virtual;

    {:Overload this function to process pong frame)
      aData represents textual information transfered with frame (there is no specified format or meaning)
    }
    procedure ProcessPong(aData: string); virtual;

    {:Overload this function to process binary frame)
      aFinal whether frame is final frame or continuing
      aRes1 whether 1st extension bit is set up
      aRes2 whether 2nd extension bit is set up
      aRes3 whether 3rd extension bit is set up
      aData data stream

      second version is for contuniation frames
    }
    procedure ProcessStream(aFinal, aRes1, aRes2, aRes3: boolean; aData: TMemoryStream); virtual;
    procedure ProcessStreamContinuation(aFinal, aRes1, aRes2, aRes3: boolean; aData: TMemoryStream); virtual;
    procedure ProcessStreamFull(aData: TMemoryStream); virtual;

    {:Overload this function to process text frame)
      aFinal whether frame is final frame or continuing
      aRes1 whether 1st extension bit is set up
      aRes2 whether 2nd extension bit is set up
      aRes3 whether 3rd extension bit is set up
      aData textual data

      second version is for contuniation frames
    }
    procedure ProcessText(aFinal, aRes1, aRes2, aRes3: boolean; aData: string);  virtual;
    procedure ProcessTextContinuation(aFinal, aRes1, aRes2, aRes3: boolean; aData: string);  virtual;
    procedure ProcessTextFull(aData: string);  virtual;

  published

  public
    constructor Create(aSocket: TTCPCustomConnectionSocket); override;
    destructor Destroy; override;

    {:
      Whether connection is in active state (not closed, closing, socket, exists, i/o  threads not terminated..)
    }
    function CanReceiveOrSend: boolean;

    {:Procedure to close connection
      aCloseCode represents close code (see wsClose constants)
      aCloseReason represents textual information transfered with frame (there is no specified format or meaning) the string can only be 123 bytes length
    }
    procedure Close(aCode: integer; aCloseReason: string); virtual; abstract;

    {:Send binary frame
      aData data stream
      aFinal whether frame is final frame or continuing
      aRes1 1st extension bit
      aRes2 2nd extension bit
      aRes3 3rd extension bit
    }
    procedure SendBinary(aData: TStream; aFinal: boolean = true; aRes1: boolean = false;  aRes2: boolean = false;  aRes3: boolean = false);

    {:Send binary continuation frame
      aData data stream
      aFinal whether frame is final frame or continuing
      aRes1 1st extension bit
      aRes2 2nd extension bit
      aRes3 3rd extension bit
    }
    procedure SendBinaryContinuation(aData: TStream; aFinal: boolean = true; aRes1: boolean = false;  aRes2: boolean = false;  aRes3: boolean = false);

    {:Send generic frame
      aFinal whether frame is final frame or continuing
      aRes1 1st extension bit
      aRes2 2nd extension bit
      aRes3 3rd extension bit
      aCode frame code
      aData data stream or string
    }
    function SendData(aFinal, aRes1, aRes2, aRes3: boolean; aCode: integer; aData: TStream): integer; overload; virtual;
    function SendData(aFinal, aRes1, aRes2, aRes3: boolean; aCode: integer; aData: string): integer; overload; virtual;


    {:Send textual frame
      aData data string (MUST be UTF-8)
      aFinal whether frame is final frame or continuing
      aRes1 1st extension bit
      aRes2 2nd extension bit
      aRes3 3rd extension bit
    }
    procedure SendText(aData: string; aFinal: boolean = true; aRes1: boolean = false;  aRes2: boolean = false;  aRes3: boolean = false); virtual;

    {:Send textual continuation frame
      aData data string (MUST be UTF-8)
      aFinal whether frame is final frame or continuing
      aRes1 1st extension bit
      aRes2 2nd extension bit
      aRes3 3rd extension bit
    }
    procedure SendTextContinuation(aData: string; aFinal: boolean = true; aRes1: boolean = false;  aRes2: boolean = false;  aRes3: boolean = false);

    {:Send Ping
      aData ping informations
    }
    procedure Ping(aData: string);

    {:Send Pong
      aData pong informations
    }
    procedure Pong(aData: string);

    {:Temination procedure
      This method should be called instead of Terminate to terminate thread,
      it internally calls Terminate, but can be overloaded,
      and can be used for data clean up
    }
    procedure TerminateThread; override;



    {: Whether connection has been closed
      (either socket has been closed or thread has been terminated or WebSocket has been closed by this and peer connection)
     }
    property Closed: boolean read GetClosed;

    {: Whether WebSocket has been closed by this and peer connection }
    property Closing: boolean read GetClosing;

    {: WebSocket connection cookies
      Property is regular unparsed Cookie header string
      e.g. cookie1=value1;cookie2=value2

      empty string represents that no cookies are present
    }
    property Cookie: string read fCookie;

    {: WebSocket connection extensions
      Property is regular unparsed Sec-WebSocket-Extensions header string
      e.g. foo, bar; baz=2

      On both client and server connection this value represents the extension(s) selected by server to be used
      as a result of extension negotioation

      value - represents that no extension was negotiated and no header will be sent to client
      it is the default value
    }
    property Extension: string read fExtension;

    {:Whether to register for full data processing
    (callink @link(ProcessFullText), @link(ProcessFullStream) @link(OnFullRead)
    those methods/events are called if FullDataProcess is @true and whole message is read (after final frame)
    }
    property FullDataProcess: boolean read fFullDataProcess write fFullDataProcess;


    {:
      Whether WebSocket handshake was succecfull (and connection is afer WS handshake) 
    }
    property Handshake: boolean read fHandshake;

    {: WebSocket connection host
      Property is regular unparsed Host header string
      e.g. server.example.com
    }
    property Host: string read fHost;

    {: WebSocket connection origin
      Property is regular unparsed Sec-WebSocket-Origin header string
      e.g. http://example.com
    }
    property Origin: string read fOrigin;

    {: WebSocket connection protocol
      Property is regular unparsed Sec-WebSocket-Protocol header string
      e.g. chat, superchat

      On both client and server connection this value represents the protocol(s) selected by server to be used
      as a result of protocol negotioation

      value - represents that no protocol was negotiated and no header will be sent to client
      it is the default value
    }
    property Protocol: string read fProtocol;

    {: Connection port }
    property Port: string read fPort;
    
    {: Connection resource
      e.g. /path1/path2/path3/file.ext
    }
    property ResourceName: string read fResourceName;

    {: WebSocket version (either 7 or 8 or 13)}
    property Version: integer read fVersion;

    {: WebSocket Close frame event }
    property OnClose: TWebSocketConnectionClose read fOnClose write fOnClose;

    {: WebSocket connection successfully }
    property OnOpen: TWebSocketConnectionEvent read fOnOpen write fOnOpen;

    { : WebSocket ping
    property OnPing: TWebSocketConnectionPingPongEvent read fOnPing write fOnPing;
    }

    { : WebSocket pong
    property OnPong: TWebSocketConnectionPingPongEvent read fOnPong write fOnPong;
    }

    {: WebSocket frame read }
    property OnRead: TWebSocketConnectionData read fOnRead write fOnRead;

    {: WebSocket read full data}
    property OnReadFull: TWebSocketConnectionDataFull read fOnReadFull write fOnReadFull;

    {: WebSocket frame written }
    property OnWrite: TWebSocketConnectionData read fOnWrite write fOnWrite;
  end;

  {: Class of WebSocket connections }
  TWebSocketCustomConnections = class of TWebSocketCustomConnection;

  {: WebSocket server connection automatically created by server on incoming connection }
  TWebSocketServerConnection = class(TWebSocketCustomConnection)
  public
    constructor Create(aSocket: TTCPCustomConnectionSocket); override;
    procedure Close(aCode: integer; aCloseReason: string); override;
    procedure TerminateThread; override;

    {: List of all headers
      keys are lowercased header name
      e.g host, connection, sec-websocket-key
    }
    property Header: TStringList read fHeaders;

  end;

  {: Class of WebSocket server connections }
  TWebSocketServerConnections = class of TWebSocketServerConnection;

  {: WebSocket client connection, this object shoud be created to establish client to server connection  }
  TWebSocketClientConnection = class(TWebSocketCustomConnection)
  protected
    function BeforeExecuteConnection: boolean; override;
  public
    {: construstor to create connection,
      parameters has the same meaning as corresponging connection properties (see 2 differences below) and
      should be formated according to headers values

      aProtocol and aExtension in constructor represents protocol(s) and extension(s)
      client is trying to negotiate, obejst properties then represents
      protocol(s) and extension(s) the server is supporting (the negotiation result)

      Version must be >= 8
    }
    constructor Create(aHost, aPort, aResourceName: string;
    aOrigin: string = '-'; aProtocol: string = '-'; aExtension: string = '-';
    aCookie: string = '-'; aVersion: integer = 13); reintroduce; virtual;

    procedure Close(aCode: integer; aCloseReason: string); override;
    procedure Execute; override;
  end;


  TWebSocketServer = class;

  {:Event procedural type to hook OnReceiveConnection events on server
    every time new server connection is about to be created (client is connecting to server)
    this event is called

    properties are representing connection properties as defined in @link(TWebSocketServerConnection)

    Protocol and Extension represents corresponding headers sent by client, as their out value
    server must define what kind of protocol(s) and extension(s) server is supporting, if event
    is not implemented, both values are considered as - (no value at all)

    HttpResult represents the HTTP result to be send in response, if connection is about to be
    accepted, the value MUST BE 101, any other value meand that the client will be informed about the
    result (using the HTTP code meaning) and connection will be closed, if event is not implemented
    101 is used as a default value 
  }
  TWebSocketServerReceiveConnection = procedure (
    Server: TWebSocketServer; Socket: TTCPCustomConnectionSocket;
    Header: TStringList;
    ResourceName, Host, Port, Origin, Cookie: string;
    HttpResult: integer;
    Protocol, Extensions: string
  ) of object;



  TWebSocketServer = class(TCustomServer)
  protected
    {CreateServerConnection sync variables}
    fncSocket: TTCPCustomConnectionSocket;
    fncResourceName: string;
    fncHost: string;
    fncPort: string;
    fncOrigin: string;
    fncProtocol: string;
    fncExtensions: string;
    fncCookie: string;
    fncHeaders: string;
    fncResultHttp: integer;

    fOnReceiveConnection: TWebSocketServerReceiveConnection;  protected
    function CreateServerConnection(aSocket: TTCPCustomConnectionSocket): TCustomConnection; override;
    procedure DoSyncReceiveConnection;
    procedure SyncReceiveConnection;
    property Terminated;

    {:This function defines what kind of TWebSocketServerConnection implementation should be used as
      a connection object.
      The servers default return value is TWebSocketServerConnection.

      If new connection class based on TWebSocketServerConnection is implemented,
      new server should be implemented as well with this method overloaded

      properties are representing connection properties as defined in @link(TWebSocketServerConnection)

      Protocol and Extension represents corresponding headers sent by client, as their out value
      server must define what kind of protocol(s) and extension(s) server is supporting, if event
      is not implemented, both values are cosidered as - (no value at all)

      HttpResult represents the HTTP result to be send in response, if connection is about to be
      accepted, the value MUST BE 101, any other value meand that the client will be informed about the
      result (using the HTTP code meaning) and connection will be closed, if event is not implemented
      101 is used as a default value

    }
    function GetWebSocketConnectionClass(
      Socket: TTCPCustomConnectionSocket;
      Header: TStringList;
      ResourceName, Host, Port, Origin, Cookie: string;
      out HttpResult: integer;
      var Protocol, Extensions: string
    ): TWebSocketServerConnections; virtual;

  public
    {: WebSocket connection received }
    property OnReceiveConnection: TWebSocketServerReceiveConnection read fOnReceiveConnection write fOnReceiveConnection;

    {: close all connections
    for parameters see connection Close method
    }
    procedure CloseAllConnections(aCloseCode: integer; aReason: string);


    {:Temination procedure
      This method should be called instead of Terminate to terminate thread,
      it internally calls Terminate, but can be overloaded,
      and can be used for data clean up
    }
    procedure TerminateThread; override;

    {: Method to send binary data to all connected clients
      see @link(TWebSocketServerConnection.SendBinary) for parameters description
    }
    procedure BroadcastBinary(aData: TStream; aFinal: boolean = true; aRes1: boolean = false;  aRes2: boolean = false;  aRes3: boolean = false);

    {: Method to send text data to all connected clients
      see @link(TWebSocketServerConnection.SendText) for parameters description
    }
    procedure BroadcastText(aData: string; aFinal: boolean = true; aRes1: boolean = false;  aRes2: boolean = false;  aRes3: boolean = false);

  end;

implementation
uses Math, synautil, synacode, synsock {$IFDEF Win32}, Windows{$ENDIF Win32},
  BClasses, synachar;



{$IFDEF Win32} {$O-} {$ENDIF Win32}


function httpCode(code: integer): string;
begin
  case (code) of
     100: result := 'Continue'; 
     101: result := 'Switching Protocols'; 
     200: result := 'OK'; 
     201: result := 'Created'; 
     202: result := 'Accepted'; 
     203: result := 'Non-Authoritative Information'; 
     204: result := 'No Content'; 
     205: result := 'Reset Content';
     206: result := 'Partial Content'; 
     300: result := 'Multiple Choices'; 
     301: result := 'Moved Permanently'; 
     302: result := 'Found'; 
     303: result := 'See Other'; 
     304: result := 'Not Modified'; 
     305: result := 'Use Proxy'; 
     307: result := 'Temporary Redirect'; 
     400: result := 'Bad Request'; 
     401: result := 'Unauthorized'; 
     402: result := 'Payment Required'; 
     403: result := 'Forbidden'; 
     404: result := 'Not Found'; 
     405: result := 'Method Not Allowed'; 
     406: result := 'Not Acceptable'; 
     407: result := 'Proxy Authentication Required'; 
     408: result := 'Request Time-out'; 
     409: result := 'Conflict'; 
     410: result := 'Gone'; 
     411: result := 'Length Required'; 
     412: result := 'Precondition Failed'; 
     413: result := 'Request Entity Too Large';
     414: result := 'Request-URI Too Large'; 
     415: result := 'Unsupported Media Type'; 
     416: result := 'Requested range not satisfiable'; 
     417: result := 'Expectation Failed'; 
     500: result := 'Internal Server Error'; 
     501: result := 'Not Implemented'; 
     502: result := 'Bad Gateway'; 
     503: result := 'Service Unavailable';
     504: result := 'Gateway Time-out';
     else result := 'unknown code: $code';
  end;
end;


function ReadHttpHeaders(aSocket: TTCPCustomConnectionSocket; var aGet: string; aHeaders: TStrings): boolean;
var s, name: string;
begin
  aGet := '';
  aHeaders.Clear;
  result := true;
  repeat
    aSocket.MaxLineLength := 1024 * 1024; // not to attack memory on server
    s := aSocket.RecvString(30 * 1000); // not to hang up connection
    if (aSocket.LastError <> 0) then
    begin
      result := false;
      break;
    end;
    if (s = '') then
      break;
    if (aGet = '') then
      aGet := s
    else
    begin
      name := LowerCase(trim(SeparateLeft(s, ':')));
      if (aHeaders.Values[name] = '') then
        aHeaders.Values[name] := trim(SeparateRight(s, ':'))
      else
        aHeaders.Values[name] := aHeaders.Values[name] + ',' + trim(SeparateRight(s, ':'));
    end;
  until {IsTerminated} false;
  aSocket.MaxLineLength := 0;
end;

procedure ODS(aStr: string); overload;
begin
  {$IFDEF Win32}
  OutputDebugString(pChar(FormatDateTime('yyyy-mm-dd hh:nn:ss', now) + ': ' + aStr));
  {$ENDIF Win32}
end;

procedure ODS(aStr: string; aData: array of const); overload;
begin
  {$IFDEF Win32}
  ODS(Format(aStr, aData));
  {$ENDIF Win32}
end;

{ TWebSocketServer }

procedure TWebSocketServer.BroadcastBinary(aData: TStream; aFinal: boolean = true; aRes1: boolean = false;  aRes2: boolean = false;  aRes3: boolean = false);
var i: integer;
begin
  LockTermination;
  for i := 0 to fConnections.Count - 1 do
  begin
    if (not TWebSocketServerConnection(fConnections[i]).IsTerminated) then
      TWebSocketServerConnection(fConnections[i]).SendBinary(aData, aFinal, aRes1, aRes2,  aRes3);
  end;
  UnLockTermination;
end;

procedure TWebSocketServer.BroadcastText(aData: string; aFinal: boolean = true; aRes1: boolean = false;  aRes2: boolean = false;  aRes3: boolean = false);
var i: integer;
begin
  LockTermination;
  for i := 0 to fConnections.Count - 1 do
  begin
    if (not TWebSocketServerConnection(fConnections[i]).IsTerminated) then
      TWebSocketServerConnection(fConnections[i]).SendText(aData, aFinal, aRes1, aRes2,  aRes3);
  end;
  UnLockTermination;
end;

procedure TWebSocketServer.CloseAllConnections(aCloseCode: integer; aReason: string);
var i: integer;
begin
  LockTermination;
  //for i := 0 to fConnections.Count - 1 do
  for i := fConnections.Count - 1 downto 0 do
  begin
    if (not TWebSocketServerConnection(fConnections[i]).IsTerminated) then
      TWebSocketServerConnection(fConnections[i]).Close(aCloseCode, aReason);// SendBinary(aData, aFinal, aRes1, aRes2,  aRes3);
  end;
  UnLockTermination;

end;

function TWebSocketServer.CreateServerConnection(aSocket: TTCPCustomConnectionSocket): TCustomConnection;
var headers, hrs: TStringList;
    get: string;
    s{, resName, host, port}, key, version{, origin, protocol, extensions, cookie}: string;
    iversion, vv: integer;
    res: boolean;
    r : TWebSocketServerConnections;
begin
  fncSocket := aSocket;
  result := inherited CreateServerConnection(aSocket);
  headers := TStringList.Create;
  try
    res := ReadHttpHeaders(aSocket, get, headers);
    if (res) then
    begin
      res := false;
      try
        //CHECK HTTP GET
        if ((Pos('GET ', Uppercase(get)) <> 0) and (Pos(' HTTP/1.1', Uppercase(get)) <> 0)) then
        begin
          fncResourceName := SeparateRight(get, ' ');
          fncResourceName := SeparateLeft(fncResourceName, ' ');
        end
        else exit;
        fncResourceName := trim(fncResourceName);

  {
      : string;
      : string;
      : string;
      : string;
      : string;
      : string;
      : string;
      fncHeaders: string;
  }

        //CHECK HOST AND PORT
        s := headers.Values['host'];
        if (s <> '') then
        begin
          fncHost := trim(s);
          fncPort := SeparateRight(fncHost, ':');
          fncHost := SeparateLeft(fncHost, ':');
        end;
        fncHost := trim(fncHost);
        fncPort := trim(fncPort);

        if (fncHost = '') then exit;
        //if (fncPort <> '') and (fncPort <> self.port) then exit;

        {
        if  (self.host <> '0.0.0.0') and (self.Host <> '127.0.0.1') and
            (self.host <> 'localhost') and (fncHost <> self.host) then exit;
        }    

        //WEBSOCKET KEY
        s := headers.Values['sec-websocket-key'];
        if (s <> '') then
        begin
          if (Length(DecodeBase64(s)) = 16) then
          begin
            key := s;
          end;

        end;
        if (key = '') then exit;
        key := trim(key);

        //WEBSOCKET VERSION
        s := headers.Values['sec-websocket-version'];
        if (s <> '') then
        begin
          vv := StrToIntDef(s, -1);

          if ((vv >= 7) and (vv <= 13)) then
          begin
            version := s;
          end;
        end;
        if (version = '') then exit;
        version := trim(version);
        iversion := StrToIntDef(version, 13);

        if (LowerCase(headers.Values['upgrade']) <> LowerCase('websocket')) or (pos('upgrade', LowerCase(headers.Values['connection'])) = 0) then
          exit;

        //COOKIES


        fncProtocol := '-';
        fncExtensions := '-';
        fncCookie := '-';
        fncOrigin := '-';

        if (iversion < 13) then
        begin
          if (headers.IndexOfName('sec-websocket-origin') > -1) then
            fncOrigin := trim(headers.Values['sec-websocket-origin']);
        end
        else begin
          if (headers.IndexOfName('origin') > -1) then
            fncOrigin := trim(headers.Values['origin']);
        end;

        if (headers.IndexOfName('sec-websocket-protocol') > -1) then
          fncProtocol := trim(headers.Values['sec-websocket-protocol']);
        if (headers.IndexOfName('sec-websocket-extensions') > -1) then
          fncExtensions := trim(headers.Values['sec-websocket-extensions']);
        if (headers.IndexOfName('cookie') > -1) then
          fncCookie := trim(headers.Values['cookie']);

        fncHeaders := trim(headers.text);

        {
        ODS(get);
        ODS(fncHeaders);
        ODS('ResourceName: %s', [fncResourceName]);
        ODS('Host: %s', [fncHost]);
        ODS('Post: %s', [fncPort]);
        ODS('Key: %s', [key]);
        ODS('Version: %s', [version]);
        ODS('Origin: %s', [fncOrigin]);
        ODS('Protocol: %s', [fncProtocol]);
        ODS('Extensions: %s', [fncExtensions]);
        ODS('Cookie: %s', [fncCookie]);
        {}

        res := true;
      finally
        if (res) then
        begin
          fncResultHttp := 101;
          hrs := TStringList.Create;
          hrs.Assign(headers);
          r := GetWebSocketConnectionClass(
            fncSocket,
            hrs,
            fncResourceName, fncHost, fncPort, fncOrigin, fncCookie,
            fncResultHttp, fncProtocol,  fncExtensions
          );
          if (assigned(r)) then
          begin
            DoSyncReceiveConnection;
            if (fncResultHttp <> 101) then //HTTP ERROR FALLBACK
            begin
              aSocket.SendString(Format('HTTP/1.1 %d %s'+#13#10, [fncResultHttp, httpCode(fncResultHttp)]));
              aSocket.SendString(Format('%d %s'+#13#10#13#10, [fncResultHttp, httpCode(fncResultHttp)]));
            end
            else
            begin

              key := EncodeBase64(SHA1(key + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11'));

              s :=        'HTTP/1.1 101 Switching Protocols' + #13#10;
              s := s +    'Upgrade: websocket' + #13#10;
              s := s +    'Connection: Upgrade' + #13#10;
              s := s +    'Sec-WebSocket-Accept: ' + key + #13#10;
              if (fncProtocol <> '-') then
              begin
                s := s +  'Sec-WebSocket-Protocol: ' + fncProtocol + #13#10;
              end;
              if (fncExtensions <> '-') then
              begin
                s := s +  'Sec-WebSocket-Extensions: ' + fncExtensions + #13#10;
              end;
              s := s + #13#10;

              aSocket.SendString(s);
              if (aSocket.LastError = 0) then
              begin
                result := r.Create(aSocket);
                TWebSocketCustomConnection(result).fCookie := fncCookie;
                TWebSocketCustomConnection(result).fVersion := StrToInt(version);
                TWebSocketCustomConnection(result).fProtocol := fncProtocol;
                TWebSocketCustomConnection(result).fResourceName := fncResourceName;
                TWebSocketCustomConnection(result).fOrigin := fncOrigin;
                TWebSocketCustomConnection(result).fExtension := fncExtensions;
                TWebSocketCustomConnection(result).fPort := fncPort;
                TWebSocketCustomConnection(result).fHost := fncHost;
                TWebSocketCustomConnection(result).fHeaders.Assign(headers);
                TWebSocketCustomConnection(result).fHandshake := true;
              end;
            end;
          end;
          hrs.Free;
        end;
      end;
    end;
  finally
    headers.Free;
  end;
end;

procedure TWebSocketServer.DoSyncReceiveConnection;
begin
  if (assigned(fOnReceiveConnection)) then
    Synchronize(SyncReceiveConnection)
end;

function TWebSocketServer.GetWebSocketConnectionClass(      Socket: TTCPCustomConnectionSocket;
      Header: TStringList;
      ResourceName, Host, Port, Origin, Cookie: string;
      out HttpResult: integer;
      var Protocol, Extensions: string
): TWebSocketServerConnections;
begin
  result := TWebSocketServerConnection;
end;

procedure TWebSocketServer.SyncReceiveConnection;
var h: TStringList;
begin
  if (assigned(fOnReceiveConnection)) then
  begin
    h := TStringList.Create;
    h.Text := fncHeaders;
    fOnReceiveConnection(
      self, fncSocket,
      h,
      fncResourceName, fncHost, fncPort, fncOrigin, fncCookie,
      fncResultHttp, fncProtocol,  fncExtensions
    );
    h.Free;
  end;
end;

procedure TWebSocketServer.TerminateThread;
begin
  if (terminated) then exit;
  fOnReceiveConnection := nil;  
  inherited;
end;

{ TWebSocketCustomConnection }

function TWebSocketCustomConnection.CanReceiveOrSend: boolean;
begin
  result := ValidConnection and not (fClosedByMe or fClosedByPeer) and fHandshake;
end;


constructor TWebSocketCustomConnection.Create(aSocket: TTCPCustomConnectionSocket);
begin
  fHeaders := TStringList.Create;
  fCookie := '';
  fVersion := 0;
  fProtocol := '-';
  fResourceName := '';
  fOrigin := '';
  fExtension := '-';
  fPort := '';
  fHost := '';
  fClosedByMe := false;
  fClosedByPeer := false;
  fMasking := false;
  fClosingByPeer := false;
  fRequireMasking := false;


  fReadFinal := false;
  fReadRes1 := false;
  fReadRes2 := false;
  fReadRes3 := false;
  fReadCode := 0;
  fReadStream := TMemoryStream.Create;

  fWriteFinal := false;
  fWriteRes1 := false;
  fWriteRes2 := false;
  fWriteRes3 := false;
  fWriteCode := 0;
  fWriteStream := TMemoryStream.Create;

  fFullDataProcess := false;
  fFullDataStream := TMemoryStream.Create;

  fSendCriticalSection := TCriticalSection.Create;
  fHandshake := false;

  inherited;

end;

destructor TWebSocketCustomConnection.Destroy;
begin
  fSendCriticalSection.Free;
  fFullDataStream.Free;
  fWriteStream.Free;
  fReadStream.Free;
  fHeaders.Free;
  inherited;
end;

procedure TWebSocketCustomConnection.DoSyncClose;
begin
  if (assigned(fOnClose)) then
    Synchronize(SyncClose);

end;



procedure TWebSocketCustomConnection.DoSyncOpen;
begin
  if (assigned(fOnOpen)) then
    Synchronize(SyncOpen);
end;
{
procedure TWebSocketCustomConnection.DoSyncPing;
begin

end;

procedure TWebSocketCustomConnection.DoSyncPong;
begin

end;
}
procedure TWebSocketCustomConnection.DoSyncRead;
begin
  fReadStream.Position := 0;
  if (assigned(fOnRead)) then
    Synchronize(SyncRead);

end;

procedure TWebSocketCustomConnection.DoSyncReadFull;
begin
  fFullDataStream.Position := 0;
  if (assigned(fOnReadFull)) then
    Synchronize(SyncReadFull);
end;

procedure TWebSocketCustomConnection.DoSyncWrite;
begin
  if (assigned(fOnWrite)) then
    Synchronize(SyncWrite);
end;

procedure TWebSocketCustomConnection.ExecuteConnection;
var
    result: integer;
    //Data: string;
    closeCode: integer;
    closeResult: string;
    s: string;
    lastDataCode, lastDataCode2: integer;
    //Data: TStringStream;
begin
  DoSyncOpen;
  try
    //while(not IsTerminated) or fClosed do
    lastDataCode := -1;
    lastDataCode2 := -1;
    while CanReceiveOrSend do
    begin
      //OutputDebugString(pChar(Format('execute %d', [fIndex])));
      result := ReadData(fReadFinal,  fReadRes1, fReadRes2, fReadRes3, fReadCode, fReadStream);
      if (CanReceiveOrSend)  then
      begin
        if (result = 0) then // no socket error occured
        begin
          fReadStream.Position := 0;
          ProcessData(fReadFinal,  fReadRes1, fReadRes2, fReadRes3, fReadCode, fReadStream);
          fReadStream.Position := 0;

          if (fReadCode in [wsCodeText, wsCodeBinary]) and fFullDataProcess then
          begin
            fFullDataStream.Size := 0;
            fFullDataStream.Position := 0;
          end;
          if (fReadCode in [wsCodeContinuation, wsCodeText, wsCodeBinary]) and fFullDataProcess then
          begin
            fReadStream.Position := 0;
            fFullDataStream.CopyFrom(fReadStream, fReadStream.Size);
            fReadStream.Position := 0;
          end;
          //if (fReadFinal) then //final frame
          begin
            case fReadCode of
              wsCodeContinuation: begin
                if (lastDataCode = wsCodeText) then
                begin
                  s := ReadStrFromStream(fReadStream, fReadStream.size);
                  ProcessTextContinuation(fReadFinal,  fReadRes1, fReadRes2, fReadRes3, s);
                  DoSyncRead;
                end
                else if (lastDataCode = wsCodeBinary) then
                begin
                  ProcessStreamContinuation(fReadFinal,  fReadRes1, fReadRes2, fReadRes3, fReadStream);
                  DoSyncRead;
                end
                else Close(wsCloseErrorProtocol, 'Unknown continuaton');
                if (fReadFinal) then lastDataCode := -1;
              end;
              wsCodeText: begin // text, binary frame
                s := ReadStrFromStream(fReadStream, fReadStream.size);
                ProcessText(fReadFinal,  fReadRes1, fReadRes2, fReadRes3, s);
                DoSyncRead;
                if (not fReadFinal) then lastDataCode := wsCodeText
                else lastDataCode := -1;
                lastDataCode2 := wsCodeText;
              end;
              wsCodeBinary: begin // text, binary frame
                ProcessStream(fReadFinal,  fReadRes1, fReadRes2, fReadRes3, fReadStream);
                DoSyncRead;
                if (not fReadFinal) then lastDataCode := wsCodeBinary
                else lastDataCode := -1;
                lastDataCode2 := wsCodeBinary;
              end;
              wsCodeClose: begin //connection close
                closeCode := wsCloseNoStatus;
                closeResult := ReadStrFromStream(fReadStream, fReadStream.size);
                if (length(closeResult) > 1) then
                begin
                  closeCode := ord(closeResult[1])*256 + ord(closeResult[2]);
                  delete(closeResult, 1, 2);
                end;
                fClosedByPeer := true;
                //OutputDebugString(pChar(Format('closing1 %d', [fIndex])));
                ProcessClose(closeCode, closeResult, true);
                //OutputDebugString(pChar(Format('closing2 %d', [fIndex])));
                TerminateThread;
                //OutputDebugString(pChar(Format('closing3 %d', [fIndex])));
                fSendCriticalSection.Enter;
              end;
              wsCodePing: begin // ping
                ProcessPing(ReadStrFromStream(fReadStream, fReadStream.size));
                DoSyncRead;
              end;
              wsCodePong: begin // pong
                ProcessPong(ReadStrFromStream(fReadStream, fReadStream.size));
                DoSyncRead;
              end
              else begin //ERROR
                Close(wsCloseErrorData, Format('Unknown data type: %d', [fReadCode]));
              end;

            end;
          end;

          if (fReadCode in [wsCodeContinuation, wsCodeText, wsCodeBinary]) and fFullDataProcess and fReadFinal then
          begin
            fFullDataStream.Position := 0;
            if (lastDataCode2 = wsCodeText) then
            begin
              s := ReadStrFromStream(fFullDataStream, fFullDataStream.size);
              ProcessTextFull(s);
            end
            else if (lastDataCode2 = wsCodeBinary) then ProcessStreamFull(fFullDataStream);
            SyncReadFull;
          end;
        end
        else
          TerminateThread;
      end;
    end;
  finally
    {$IFDEF UNIX} sleep(2000); {$ENDIF UNIX}
  end;
  while not terminated do sleep(500);
  //OutputDebugString(pChar(Format('terminating %d', [fIndex])));
  fSendCriticalSection.Enter;
end;

function TWebSocketCustomConnection.GetClosed: boolean;
begin
  result := not CanReceiveOrSend;
end;

function TWebSocketCustomConnection.GetClosing: boolean;
begin
  result := (fClosedByMe or fClosedByPeer);
end;

procedure TWebSocketCustomConnection.Ping(aData: string);
begin
  if (CanReceiveOrSend) then
  begin
    SendData(true, false, false, false, wsCodePing, aData);
  end;
end;

procedure TWebSocketCustomConnection.Pong(aData: string);
begin
  if (CanReceiveOrSend) then
  begin
    SendData(true, false, false, false, wsCodePong, aData);
  end;
end;

procedure TWebSocketCustomConnection.ProcessClose(aCloseCode: integer; aCloseReason: string; aClosedByPeer: boolean);
begin
  fCloseCode := aCloseCode;
  fCloseReason := aCloseReason;
  fClosingByPeer := aClosedByPeer;
  DoSyncClose;
end;


procedure TWebSocketCustomConnection.ProcessData(var aFinal, aRes1, aRes2,
  aRes3: boolean; var aCode: integer; aData: TMemoryStream);
begin

end;

procedure TWebSocketCustomConnection.ProcessPing(aData: string);
begin
  Pong(aData);
end;

procedure TWebSocketCustomConnection.ProcessPong(aData: string);
begin

end;

procedure TWebSocketCustomConnection.ProcessStream(aFinal, aRes1, aRes2,
  aRes3: boolean; aData: TMemoryStream);
begin

end;

procedure TWebSocketCustomConnection.ProcessStreamContinuation(aFinal,
  aRes1, aRes2, aRes3: boolean; aData: TMemoryStream);
begin

end;

procedure TWebSocketCustomConnection.ProcessStreamFull(
  aData: TMemoryStream);
begin

end;

procedure TWebSocketCustomConnection.ProcessText(aFinal, aRes1, aRes2,
  aRes3: boolean; aData: string);
begin

end;

procedure TWebSocketCustomConnection.ProcessTextContinuation(aFinal, aRes1,
  aRes2, aRes3: boolean; aData: string);
begin

end;

procedure TWebSocketCustomConnection.ProcessTextFull(aData: string);
begin

end;

function GetByte(aSocket: TTCPCustomConnectionSocket; var aByte: Byte; var aTimeout: integer): integer;
begin
  aByte := aSocket.RecvByte(aTimeout);
  result := aSocket.LastError;
end;

function hexToStr(aDec: integer; aLength: integer): string;
var tmp: string;
    i: integer;
begin
  tmp := IntToHex(aDec, aLength);
  result := '';
  for i := 1 to (Length(tmp)+1) div 2 do
  begin
    result := result + ansichar(StrToInt('$'+Copy(tmp, i * 2 - 1, 2)));
  end;
end;

function StrToHexstr2(str: string): string;
var i: integer;
begin
  result := '';
  for i := 1 to Length(str) do result := result + IntToHex(ord(str[i]), 2) + ' ';
end;


function TWebSocketCustomConnection.ReadData(var aFinal, aRes1, aRes2, aRes3: boolean;
  var aCode: integer; aData: TMemoryStream): integer;
var timeout: integer;
    b: byte;
    mask: boolean;
    len, i: int64;
    mBytes: array[0..3] of byte;
    ms: TMemoryStream;
begin
  result := 0;
  len := 0;
  //aCode := 0;
  repeat
    timeout := 10 * 1000;
    if CanReceiveOrSend then
    begin
      //OutputDebugString(pChar(Format('%d', [Index])));
      if (fSocket.CanReadEx(1000)) then
      begin
        if CanReceiveOrSend then
        begin
          b := fSocket.RecvByte(1000);
          if (fSocket.LastError = 0) then
          begin
            try
              try
                // BASIC INFORMATIONS
                aFinal := (b and $80) = $80;
                aRes1 := (b and $40) = $40;
                aRes2 := (b and $20) = $20;
                aRes3 := (b and $10) = $10;
                aCode := b and $F;


                // MASK AND LENGTH
                mask := false;
                result := GetByte(fSocket, b, timeout);
                if (result = 0) then
                begin
                  mask := (b and $80) = $80;
                  len := (b and $7F);
                  if (len = 126) then
                  begin
                    result := GetByte(fSocket, b, timeout);
                    if (result = 0) then
                    begin
                      len := b * $100; // 00 00
                      result := GetByte(fSocket, b, timeout);
                      if (result = 0) then
                      begin
                        len := len + b;
                      end;
                    end;
                  end
                  else if (len = 127) then    //00 00 00 00 00 00 00 00
                  begin

                    //TODO nesting og get byte should be different
                    result := GetByte(fSocket, b, timeout);
                    if (result = 0) then
                    begin
                      len := b * $100000000000000;
                      if (result = 0) then
                      begin
                        result := GetByte(fSocket, b, timeout);
                        len := len + b * $1000000000000;
                      end;
                      if (result = 0) then
                      begin
                        result := GetByte(fSocket, b, timeout);
                        len := len + b * $10000000000;
                      end;
                      if (result = 0) then
                      begin
                        result := GetByte(fSocket, b, timeout);
                        len := len + b * $100000000;
                      end;
                      if (result = 0) then
                      begin
                        result := GetByte(fSocket, b, timeout);
                        len := len + b * $1000000;
                      end;
                      if (result = 0) then
                      begin
                        result := GetByte(fSocket, b, timeout);
                        len := len + b * $10000;
                      end;
                      if (result = 0) then
                      begin
                        result := GetByte(fSocket, b, timeout);
                        len := len + b * $100;
                      end;
                      if (result = 0) then
                      begin
                        result := GetByte(fSocket, b, timeout);
                        len := len + b;
                      end;
                    end;
                  end;
                end;

                if (result = 0) and (fRequireMasking) and (not mask) then
                begin
                  // TODO some protocol error
                  raise Exception.Create('mask');
                end;

                // MASKING KEY
                if (mask) and (result = 0) then
                begin
                  result := GetByte(fSocket, mBytes[0], timeout);
                  if (result = 0) then result := GetByte(fSocket, mBytes[1], timeout);
                  if (result = 0) then result := GetByte(fSocket, mBytes[2], timeout);
                  if (result = 0) then result := GetByte(fSocket, mBytes[3], timeout);
                end;
                // READ DATA
                if (result = 0) then
                begin
                  aData.Clear;
                  ms := TMemoryStream.Create;
                  try
                    timeout := 1000 * 60 * 60 * 2; //(len div (1024 * 1024)) * 1000 * 60;
                    if (mask) then fSocket.RecvStreamSize(ms, timeout, len)
                    else fSocket.RecvStreamSize(aData, timeout, len);

                    ms.Position := 0;
                    aData.Position := 0;
                    result := fSocket.LastError;
                    if (result = 0) then
                    begin
                      if (mask) then
                      begin
                        i := 0;
                        while i < len  do
                        begin
                          ms.ReadBuffer(b, sizeOf(b));
                          b := b xor mBytes[i mod 4];
                          aData.WriteBuffer(b, SizeOf(b));
                          inc(i);
                        end;
                      end;
                    end;
                  finally
                    ms.free;
                  end;
                  aData.Position := 0;
                  break;
                end;
              except
                result := -1;
              end;
            finally
            end;
          end
          else
          begin
            result := -1;
          end;
        end
        else
        begin
          result := -1;
        end;
      end
      else
      begin
//        if (fSocket.CanRead(0)) then
//          ODS(StrToHexstr2(fSocket.RecvBufferStr(10, 1000)));
        if (fSocket.LastError <> WSAETIMEDOUT) and (fSocket.LastError <> 0) then
        begin
          //if (fSocket.LastError = WS then
          
          result := -1;
        end;
      end;
    end
    else
    begin
      result := -1;
    end;
    if (result <> 0) then
    begin
      if (not Terminated) then
      begin
        if (fSocket.LastError = WSAECONNRESET) then
        begin
          result := 0;
          aCode := wsCodeClose;
          aFinal := true;
          aRes1 := false;
          aRes2 := false;
          aRes3 := false;
          aData.Size := 0;
          WriteStrToStream(aData, ansichar(wsCloseErrorClose div 256) + ansichar(wsCloseErrorClose mod 256));
          aData.Position := 0;
        end
        else
        begin
          if (not fClosedByMe) then
          begin
            Close(wsCloseErrorProtocol, '');
            TerminateThread;
          end;
        end;
      end;
      break;
    end
  until false;
end;


function TWebSocketCustomConnection.SendData(aFinal, aRes1, aRes2, aRes3: boolean; aCode: integer; aData: TStream): integer;
var b: byte;
    s: ansistring;
    mBytes: array[0..3] of byte;
    ms: TMemoryStream;
    i, len: int64;
begin
  result := 0;
  if (CanReceiveOrSend) or ((aCode = wsCodeClose) and (not fClosedByPeer)) then
  begin
    fSendCriticalSection.Enter;
    try

      s := '';

      // BASIC INFORMATIONS
      b := IfThen(aFinal, 1, 0) * $80;
      b := b + IfThen(aRes1, 1, 0) * $40;
      b := b + IfThen(aRes2, 1, 0) * $20;
      b := b + IfThen(aRes3, 1, 0) * $10;
      b := b + aCode;
      s := s + ansichar(b);

      // MASK AND LENGTH
      b := IfThen(fMasking, 1, 0) * $80;
      if (aData.Size < 126) then
        b := b + aData.Size
      else if (aData.Size < 65536) then
        b := b + 126
      else
        b := b + 127;
      s := s + ansichar(b);
      if (aData.Size >= 126) then
      begin
        if (aData.Size < 65536) then
        begin
          s := s + hexToStr(aData.Size, 4);
        end
        else
        begin
          s := s + hexToStr(aData.Size, 16);
        end;
      end;

      // MASKING KEY
      if (fMasking) then
      begin
        mBytes[0] := Random(256);
        mBytes[1] := Random(256);
        mBytes[2] := Random(256);
        mBytes[3] := Random(256);


        s := s + ansichar(mBytes[0]);
        s := s + ansichar(mBytes[1]);
        s := s + ansichar(mBytes[2]);
        s := s + ansichar(mBytes[3]);
      end;

      fSocket.SendString(s);
      result := fSocket.LastError;
      if (result = 0) then
      begin
        aData.Position := 0;
        ms := TMemoryStream.Create;
        try
          if (not fMasking) then
          begin
            fSocket.SendStreamRaw(aData);
          end
          else
          begin
            i := 0;
            len := aData.Size;
            while i < len  do
            begin
              aData.ReadBuffer(b, sizeOf(b));
              b := b xor mBytes[i mod 4];
              ms.WriteBuffer(b, SizeOf(b));
              inc(i);
            end;
            ms.Position := 0;
            fSocket.SendStreamRaw(ms);
          end;

          result := fSocket.LastError;
          if (result = 0) then
          begin
            fWriteFinal := aFinal;
            fWriteRes1 := aRes1;
            fWriteRes2 := aRes2;
            fWriteRes3 := aRes3;
            fWriteCode := aCode;
            aData.Position := 0;
            fWriteStream.Clear;
            fWriteStream.LoadFromStream(aData);
            DoSyncWrite;
          end;

        finally
          ms.Free;
        end;
      end;
    finally
      if (aCode <> wsCodeClose) then
        while not fSocket.CanWrite(10) do sleep(10);
      fSendCriticalSection.Leave;
    end;
  end;
end;

function TWebSocketCustomConnection.SendData(aFinal, aRes1, aRes2, aRes3: boolean; aCode: integer; aData: string): integer;
var ms : TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    WriteStrToStream(ms, aData);
    result := SendData(aFinal, aRes1, aRes2, aRes3, aCode, ms);
  finally
    ms.Free;
  end;
end;

procedure TWebSocketCustomConnection.SendBinary(aData: TStream; aFinal: boolean = true; aRes1: boolean = false;  aRes2: boolean = false;  aRes3: boolean = false);
begin
  SendData(aFinal, aRes1, aRes2, aRes3, wsCodeBinary, aData);
end;

procedure TWebSocketCustomConnection.SendBinaryContinuation(aData: TStream; aFinal, aRes1, aRes2, aRes3: boolean);
begin
  SendData(aFinal, aRes1, aRes2, aRes3, wsCodeContinuation, aData);
end;

procedure TWebSocketCustomConnection.SendText(aData: string; aFinal: boolean = true; aRes1: boolean = false;  aRes2: boolean = false;  aRes3: boolean = false);
begin
  SendData(aFinal, aRes1, aRes2, aRes3, wsCodeText, aData);
end;

procedure TWebSocketCustomConnection.SendTextContinuation(aData: string; aFinal, aRes1, aRes2, aRes3: boolean);
begin
  SendData(aFinal, aRes1, aRes2, aRes3, wsCodeContinuation, aData);
end;

{
procedure TWebSocketCustomConnection.SendStream(aFinal, aRes1, aRes2, aRes3: boolean; aData: TStream);
begin
  if (CanReceiveOrSend) then
  begin
    SendData(aFinal, aRes1, aRes2, aRes3, wsCodeBinary, aData);
  end;
end;
}
{
procedure TWebSocketCustomConnection.SendStream(aData: TStream);
begin
  //SendStream(aFinal, false, false, false, aData);
end;
}
{
procedure TWebSocketCustomConnection.SendText(aFinal, aRes1, aRes2, aRes3: boolean; aData: string);
//var tmp: string;
begin
  if (CanReceiveOrSend) then
  begin
    SendData(aFinal, false, false, false, wsCodeText, aData);
  end;
end;
}
{
procedure TWebSocketCustomConnection.SendText(aData: string);
begin
  //SendText(true, false, false, false, aData);
  //SendData(true, false, false
end;
}
procedure TWebSocketCustomConnection.SyncClose;
begin
  if (assigned(fOnClose)) then
    fOnClose(self, fCloseCode, fCloseReason, fClosingByPeer);
end;

procedure TWebSocketCustomConnection.SyncOpen;
begin
  if (assigned(fOnOpen)) then
    fOnOpen(self);
end;
{
procedure TWebSocketCustomConnection.SyncPing;
begin

end;

procedure TWebSocketCustomConnection.SyncPong;
begin

end;
}
procedure TWebSocketCustomConnection.SyncRead;
begin
  fReadStream.Position := 0;
  if (assigned(fOnRead)) then
    fOnRead(self, fReadFinal, fReadRes1, fReadRes2, fReadRes3, fReadCode, fReadStream);
end;

procedure TWebSocketCustomConnection.SyncReadFull;
begin
  fFullDataStream.Position := 0;
  if (assigned(fOnReadFull)) then
    fOnReadFull(self, fReadCode, fFullDataStream);
end;

procedure TWebSocketCustomConnection.SyncWrite;
begin
  fWriteStream.Position := 0;
  if (assigned(fOnWrite)) then
    fOnWrite(self, fWriteFinal, fWriteRes1, fWriteRes2, fWriteRes3, fWriteCode, fWriteStream);
end;

procedure TWebSocketCustomConnection.TerminateThread;
begin
  if (Terminated) then exit;

  if (not Closed) then
    DoSyncClose;
  Socket.OnSyncStatus := nil;
  Socket.OnStatus := nil;
  fOnRead := nil;
  fOnReadFull := nil;
  fOnWrite := nil;
  fOnClose := nil;
  fOnOpen := nil;
  {
  if not Closing then
  begin
    SendData(true, false, false, false, wsCodeClose, '1001');
  end;
  }
  inherited;
end;

function TWebSocketCustomConnection.ValidConnection: boolean;
begin
  result := (not IsTerminated) and (Socket.Socket <> INVALID_SOCKET);
end;



{ TWebSocketServerConnection }

procedure TWebSocketServerConnection.Close(aCode: integer; aCloseReason: string);
begin
  if (Socket.Socket <> INVALID_SOCKET) and (not fClosedByMe) then
  begin
    fClosedByMe := true;
    if (not fClosedByPeer) then
    begin
      SendData(true, false, false, false, wsCodeClose, hexToStr(aCode, 4) + copy(aCloseReason, 1, 123));
      //Sleep(2000);
      ProcessClose(aCode, aCloseReason, false);
    end;

    TerminateThread;
  end;
end;

constructor TWebSocketServerConnection.Create(aSocket: TTCPCustomConnectionSocket);
begin
  inherited;
  fRequireMasking := true;
end;

procedure TWebSocketServerConnection.TerminateThread;
begin
  if (Terminated) then exit;
  //if (not TWebSocketServer(fParent).Terminated) and (not fClosedByMe) then DoSyncClose;
  fOnClose := nil;
  inherited;

end;

{ TWebSocketClientConnection }

function TWebSocketClientConnection.BeforeExecuteConnection: boolean;
var key, s, get: string;
    i: integer;
    headers: TStringList;
begin
  Result := not IsTerminated;
  if (Result) then
  begin
    s := Format('GET %s HTTP/1.1' + #13#10, [fResourceName]);
    s := s + Format('Upgrade: websocket' + #13#10, []);
    s := s + Format('Connection: Upgrade' + #13#10, []);
    s := s + Format('Host: %s:%s' + #13#10, [fHost, fPort]);

    for I := 1 to 16 do key := key + ansichar(Random(85) + 32);
    key := EncodeBase64(key);
    s := s + Format('Sec-WebSocket-Key: %s' + #13#10, [(key)]);
    s := s + Format('Sec-WebSocket-Version: %d' + #13#10, [fVersion]);

    //TODO extensions
    if (fProtocol <> '-') then
      s := s + Format('Sec-WebSocket-Protocol: %s' + #13#10, [fProtocol]);
    if (fOrigin <> '-') then
    begin
      if (fVersion < 13) then
        s := s + Format('Sec-WebSocket-Origin: %s' + #13#10, [fOrigin])
      else
        s := s + Format('Origin: %s' + #13#10, [fOrigin]);
    end;
    if (fCookie <> '-') then
      s := s + Format('Cookie: %s' + #13#10, [(fCookie)]);
    if (fExtension <> '-') then
      s := s + Format('Sec-WebSocket-Extensions: %s' + #13#10, [fExtension]);
    s := s + #13#10;
    fSocket.SendString(s);
    Result := (not IsTerminated) and (fSocket.LastError = 0);
    if (result) then
    begin
      headers := TStringList.Create;
      try
        result := ReadHttpHeaders(fSocket, get, headers);
        if (result) then result := pos(LowerCase('HTTP/1.1 101'), LowerCase(get)) = 1;
        if (result) then result := (LowerCase(headers.Values['upgrade']) = LowerCase('websocket')) and (LowerCase(headers.Values['connection']) = 'upgrade');
        fProtocol := '-';
        fExtension := '-';
        if (headers.IndexOfName('sec-websocket-protocol') > -1) then
          fProtocol := trim(headers.Values['sec-websocket-protocol']);
        if (headers.IndexOfName('sec-websocket-extensions') > -1) then
          fExtension := trim(headers.Values['sec-websocket-extensions']);
        if (result) then result := (headers.Values['sec-websocket-accept'] = EncodeBase64(SHA1(key + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11')));

      finally
        headers.Free;
      end;
    end;

  end;
  if (result) then fHandshake := true;
  
end;

procedure TWebSocketClientConnection.Close(aCode: integer; aCloseReason: string);
begin
  if ValidConnection and (not fClosedByMe) then
  begin
    fClosedByMe := true;
    if (not fClosedByPeer) then
    begin
      SendData(true, false, false, false, wsCodeClose, hexToStr(aCode, 4) + copy(aCloseReason, 1, 123));
      //Sleep(2000);
      ProcessClose(aCode, aCloseReason, false);
    end;

    TerminateThread;
  end;
end;

constructor TWebSocketClientConnection.Create(aHost, aPort,
  aResourceName, aOrigin, aProtocol: string; aExtension: string;  aCookie: string;  aVersion: integer);
begin
  fSocket := TTCPCustomConnectionSocket.Create;
  inherited Create(fSocket);
  fOrigin := aOrigin;
  fHost := aHost;
  fPort := aPort;
  fResourceName := aResourceName;
  fProtocol := aProtocol;
  fVersion := aVersion;
  fMasking := true;
  fCookie := aCookie;
  fExtension := aExtension;
end;

{
procedure TWebSocketClientConnection.DoConnect;
begin
  if (assigned(fOnConnect)) then
    Synchronize(SyncConnect);

end;

procedure TWebSocketClientConnection.DoDisconnect;
begin
  if (assigned(fOnDisConnect)) then
    Synchronize(SyncDisconnect);

end;
}
procedure TWebSocketClientConnection.Execute;
begin
  if (not IsTerminated) and (fVersion >= 8) then
  begin
    fSocket.Connect(fHost, fPort);
    if (SSL) then
      fSocket.SSLDoConnect;
    if (fSocket.LastError = 0) then
    begin
      //DoConnect;
      inherited Execute;
      //DoDisconnect;
    end
    else TerminateThread;
  end;
end;
{
procedure TWebSocketClientConnection.SyncConnect;
begin
  fOnConnect(self);
end;

procedure TWebSocketClientConnection.SyncDisconnect;
begin
  fOnDisConnect(self);
end;
}


initialization
Randomize;

{
GET / HTTP/1.1
Upgrade: websocket
Connection: Upgrade
Host: 81.0.231.149:81
Sec-WebSocket-Origin: http://html5.bauglir.dev
Sec-WebSocket-Key: Q9ceXTuzjdF2o23CRYvnuA==
Sec-WebSocket-Version: 8


GET / HTTP/1.1
Host: 81.0.231.149:81
User-Agent: Mozilla/5.0 (Windows NT 6.1; WOW64; rv:6.0) Gecko/20100101 Firefox/6.0
Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
Accept-Language: sk,cs;q=0.8,en-us;q=0.5,en;q=0.3
Accept-Encoding: gzip, deflate
Accept-Charset: ISO-8859-2,utf-8;q=0.7,*;q=0.7
Connection: keep-alive, Upgrade
Sec-WebSocket-Version: 7
Sec-WebSocket-Origin: http://html5.bauglir.dev
Sec-WebSocket-Key: HgBKcPfdBSzjCYxGnWCO3g==
Pragma: no-cache
Cache-Control: no-cache
Upgrade: websocket
Cookie: __utma=72544661.1949147240.1313811966.1313811966.1313811966.1; __utmb=72544661.3.10.1313811966; __utmc=72544661; __utmz=72544661.1313811966.1.1.utmcsr=localhost|utmccn=(referral)|utmcmd=referral|utmcct=/websocket/index.php
1300}

end.
