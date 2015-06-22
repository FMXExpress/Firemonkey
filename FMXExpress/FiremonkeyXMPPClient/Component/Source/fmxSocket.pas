{*******************************************************}
{                                                       }
{       SoftName                                        }
{                                                       }
{       Copyright (C) 2014                              }
{                                                       }
{  This unit describe tcp socket client                 }
{                                                       }
{*******************************************************}


unit fmxSocket;

interface

uses
  System.sysutils, idTcpClient, fmx.dialogs,
  FmxJabberTools,fmxReceiveThread;


type
  TJabberSocket = Class
    private
      fIdSocket : TIdTCPClient;      // TCP Client
      fReceiveThread : TfmxReceiveThread;  //thread receiving tcp responses
      fOnReceivedData : TOnResponseWithStrData;  // event fired once a jabber response is received

      function GetConnectedStatus : Boolean;
    public
      constructor Create;
      Destructor Destroy;Override;
      procedure Initialize(AServer : string; APort : Integer);  //Initialize TCP client
      procedure UnInitialize;                                   // finalize Tcp client
      procedure SendRequest(ARequest : String);    // send jabber request

      property Connected : Boolean read GetConnectedStatus;
      property OnDataReceived : TOnResponseWithStrData read fOnReceivedData write fOnReceivedData;
  End;

implementation

{ TJabberSocket }

{-------------------------------------------------------------------------------
  Procedure: TJabberSocket.Create : constructor
  Arguments: None
  Result:    None
-------------------------------------------------------------------------------}
constructor TJabberSocket.Create;
begin
  try
    fIdSocket := TIdTCPClient.Create(nil);    // Create indy tcp client
  except
    On E:Exception do
      Raise Exception.create('[TJabberSocket.Create] : '+E.message);
  end;
end;

{-------------------------------------------------------------------------------
  Procedure: TJabberSocket.Destroy
  Arguments: None
  Result:    None
-------------------------------------------------------------------------------}
destructor TJabberSocket.Destroy;
begin
  try
    if assigned(fReceiveThread) then
    begin
      fReceiveThread.Terminate;     // Terminate thread
      Sleep(1000);                  // wait a second for the thread destroying (TODO : must be replaced y an event system)
    end;
    if Assigned(fIdSocket) then
      fIdSocket.Free;
    inherited;
  except
    On E:Exception do
      Raise Exception.create('[TJabberSocket.Destroy] : '+E.message);
  end;
end;


{-------------------------------------------------------------------------------
  Procedure: TJabberSocket.GetConnectedStatus
  Arguments: None
  Result:    Boolean
-------------------------------------------------------------------------------}
function TJabberSocket.GetConnectedStatus: Boolean;
begin
  try
    Result := fIdSocket.Connected;   // get if socket is connected
  except
    On E:Exception do
      Raise Exception.create('[TJabberSocket.GetConnectedStatus] : '+E.message);
  end;
end;


{-------------------------------------------------------------------------------
  Procedure: TJabberSocket.Initialize
  Arguments: AServer: string; APort: Integer
  Result:    None
-------------------------------------------------------------------------------}
procedure TJabberSocket.Initialize(AServer: string; APort: Integer);
begin
  try
    fIdSocket.Host := AServer;
    fIdSocket.Port := APort;
    fIdSocket.Connect;             // Connect tcp client to server
    if not fIdSocket.Connected then
      raise Exception.Createfmt('Cannot connect to server %s:%d',[AServer,APort]);
    fReceiveThread := TfmxReceiveThread.create(fIdSocket);       //Create listening thread
    if assigned(fOnReceivedData) then
      fReceiveThread.OnDataRecieved := fOnReceivedData;          //Attach onmessagereceived event
    fReceiveThread.Resume;                                       // start thread
  except
    On E:Exception do
      Raise Exception.create('[TJabberSocket.Initialize] : '+E.message);
  end;
end;


{-------------------------------------------------------------------------------
  Procedure: TJabberSocket.SendRequest
  Arguments: ARequest: String
  Result:    None
-------------------------------------------------------------------------------}
procedure TJabberSocket.SendRequest(ARequest: String);
begin
  try
    fIdSocket.IOHandler.WriteLn(ARequest);  // Send message to server
  except
    On E:Exception do
      Raise Exception.create('[TJabberSocket.SendRequest] : '+E.message);
  end;
end;


{-------------------------------------------------------------------------------
  Procedure: TJabberSocket.UnInitialize
  Arguments: None
  Result:    None
-------------------------------------------------------------------------------}
procedure TJabberSocket.UnInitialize;
begin
  fReceiveThread.Terminate;
  fIdSocket.Disconnect;
end;

end.
