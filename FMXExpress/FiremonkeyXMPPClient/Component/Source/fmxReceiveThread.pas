{*******************************************************}
{                                                       }
{       Thread listening get messages sent by server    }
{                                                       }
{       Copyright (C) 2014 Company                      }
{                                                       }
{*******************************************************}


unit fmxReceiveThread;

interface

uses classes, sysutils,IdTcpClient, fmx.dialogs,
  FmxJabberTools;

Type
  TfmxReceiveThread = Class(TThread)
    private
      fOnDataReceived : TOnResponseWithStrData;  // Even fired when a message is received
      fIdTcpClient : TidTcpClient;
    protected
      procedure Execute;override;
    public
      Constructor Create(AIdClient : TidTcpClient);reintroduce;
      property OnDataRecieved : TOnResponseWithStrData read fOnDataReceived write fOnDataReceived;

  End;
implementation

{ TfmxReceiveThread }

constructor TfmxReceiveThread.Create(AIdClient: TidTcpClient);
begin
  try
    FreeOnTerminate := True;
    fIdTcpClient := AIdClient;      // Get TCP client
    inherited create(True);
  except
    On E:Exception do
      Raise Exception.create('[TfmxReceiveThread.Create] : '+E.message);
  end;
end;

procedure TfmxReceiveThread.Execute;
var wReceivedData : String;
begin
  try
    inherited;
    wReceivedData := '';
    while (fIdTcpClient.Connected) and (not Terminated) do   // while connected to server
    begin
      if fIdTcpClient.IOHandler.InputBufferIsEmpty then   // if rmessage buffer is empty so there is a new message
      begin
        if (Trim(wReceivedData) <> '') and (Assigned(fOnDataReceived)) then //hack to avoid loosing messages if server send two message without sleep
           fOnDataReceived(wReceivedData);                                  // if a message is received so fire event

        wReceivedData := '';                                                // clear the received data
        fIdTcpClient.IOHandler.CheckForDataOnSource(50);                    // Check if a data is received
      end;
      wReceivedData := wReceivedData + fIdTcpClient.IOHandler.InputBufferAsString(); // concatenate Old data (not prcessed yet) with the new one
                                                                                     // if the old one was processed, so wReceivedData will be
    end;                                                                             //empty so wReceivedData = fIdTcpClient.IOHandler.InputBufferAsString();
  except
  end;
end;

end.
