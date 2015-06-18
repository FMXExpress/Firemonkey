{ Leap Websocket interface Usign BauglirWebsocket

  Copyright (C) 2013 Michael Van Canneyt (michael@freepascal.org);

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit wsleap;

interface

uses
  Classes, SysUtils, leapdata,
  dleapjson, websocket2;

Type

  { TWebSocketLeapController }
  TFrameErrorEvent = Procedure (Sender : TObject; Error : Exception) of object;

  TWebSocketLeapController = Class(TLeapController)
    procedure DoOnOpen(aSender: TWebSocketCustomConnection);
  private
    FEnabled: Boolean;
    FHostName: String;
    FOnFrameError: TFrameErrorEvent;
    FPort: Integer;
    FPackets : Integer;
    FwebSocket: TWebSocketClientConnection;
    FConverter : TJSONFrameConverter;
    procedure ReadFrame(Data: TStream);
    procedure SetEnabled(AValue: Boolean);
    procedure SethostName(AValue: String);
    procedure SetPort(AValue: Integer);
  protected
    Procedure DoFrameError(E : Exception); virtual;
    procedure ReadVersion(Data: TStream);virtual;
    procedure OnReadPacket(aSender: TWebSocketCustomConnection; aFinal, aRes1, aRes2, aRes3: boolean; aCode: integer; aData: TMemoryStream);virtual;
    Procedure CheckInactive(Const AOperation : String = '');
    Procedure ConnectWebSocket;
    Procedure DisconnectWebSocket;
    Property WebSocket : TWebSocketClientConnection Read FwebSocket;
  Public
    Constructor Create(AOwner : TComponent);override;
    Destructor Destroy; override;
    Property Packets : Integer Read FPackets;
  Published
    Property HostName : String Read FHostName Write SethostName;
    Property Port : Integer Read FPort Write SetPort;
    Property Enabled : Boolean Read FEnabled Write SetEnabled;
    Property OnFrameError : TFrameErrorEvent Read FOnFrameError Write FOnFrameError;
  end;


implementation

{ TWebSocketLeapController }

procedure TWebSocketLeapController.ReadVersion(Data : TStream);

begin
  SetVersion(ExtractVersion(Data));
end;

procedure TWebSocketLeapController.DoOnOpen(aSender: TWebSocketCustomConnection);

begin
  aSender.SendText(WriteEnableGestures(EnableGestures));
end;

procedure TWebSocketLeapController.ReadFrame(Data : TStream);

Var
  F : TFrame;

begin
  If (FConverter=Nil) then
    FConverter:=TJSONFrameConverter.Create(Self);
  Data.Position:=0;
  F:=Nil;
  try
    F:=FConverter.FrameFromStream(Data);
  except
    On E : Exception do
      DoFrameError(E);
  end;
  AddFrame(F);
end;

procedure TWebSocketLeapController.OnReadPacket(
  aSender: TWebSocketCustomConnection; aFinal, aRes1, aRes2, aRes3: boolean;
  aCode: integer; aData: TMemoryStream);
begin
  Inc(FPackets);
  If (Fpackets=1) then
    ReadVersion(AData)
  else
    ReadFrame(AData);
end;

procedure TWebSocketLeapController.SetEnabled(AValue: Boolean);
begin
  if FEnabled=AValue then Exit;
  If AValue then
    ConnectWebSocket
  else
    DisConnectWebSocket;
  FEnabled:=AValue;
end;

procedure TWebSocketLeapController.SethostName(AValue: String);
begin
  if FHostName=AValue then Exit;
  CheckInActive('Set hostname');
  FHostName:=AValue;
end;

procedure TWebSocketLeapController.SetPort(AValue: Integer);
begin
  if FPort=AValue then Exit;
  CheckInActive('Set port');
  FPort:=AValue;
end;

procedure TWebSocketLeapController.DoFrameError(E: Exception);
begin
  if Assigned(FOnFrameError) then
    FOnFrameError(Self,E);
end;

procedure TWebSocketLeapController.CheckInactive(const AOperation: String);

Var
  M : String;
begin
  If Enabled then
    begin
    If (AOperation='') then
      M:='Cannot perform this operation while active.'
    else
      M:=Format('Cannot perform operation "%s" while active.',[AOperation]);
    Raise ELeap.Create(M);
    end;
end;

procedure TWebSocketLeapController.ConnectWebSocket;
begin
  if (FWebSocket=Nil) then
    begin
    FWebSocket:=TWebSocketClientConnection.Create(HostName,IntToStr(Port),'/');
    FWebSocket.OnRead:=@OnReadPacket;
    FWebSocket.OnOpen:=@DoOnOpen;
    FWebSocket.Start;
    end;
  FPackets:=0;
end;

procedure TWebSocketLeapController.DisconnectWebSocket;
begin
  FWebSocket.FreeOnTerminate:=True;
  FWebSocket.Close(wsCloseNormal,'');
end;

constructor TWebSocketLeapController.Create(AOwner: TComponent);
begin
  Inherited;
  FHostName:='127.0.0.1';
  FPort:= 6437;
end;

destructor TWebSocketLeapController.Destroy;
begin
  FreeAndNil(FConverter);
  inherited Destroy;
end;

end.

