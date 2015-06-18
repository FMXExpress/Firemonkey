unit BluetoothStream;

interface

uses System.SysUtils, System.Classes;

const
  kPacketLength = 512 - 1; // bytes

type
  TBluetoothStreamProgressEvent = procedure(Sender: TObject; Progress: Integer; BytesProcessed: Int64) of object;
  TBluetoothStreamFinishedEvent = procedure(Sender: TObject) of object;
  TBluetoothStreamErrorEvent = procedure(Sender: TObject) of object;

  TBluetoothStream = class
  protected
    FLastPosition: Int64;
    FStarted: Boolean;
    FStream: TStream;
    FOnBluetoothStreamProgressEvent: TBluetoothStreamProgressEvent;
    function CalculatePacketLength: Integer;
  public
    constructor Create(AStream: TStream); overload; virtual;
    function GetProgress: Integer;
    function GetBytesProcessed: Int64;
    function GetFinished: Boolean;
    procedure Reset;
    property Stream: TStream read FStream;
    property Finished: Boolean read GetFinished;
    property Progress: Integer read GetProgress;
    property BytesProcessed: Int64 read GetBytesProcessed;
    property OnBluetoothStreamProgressEvent: TBluetoothStreamProgressEvent read FOnBluetoothStreamProgressEvent
      write FOnBluetoothStreamProgressEvent;
  end;

  TBluetoothStreamSender = class(TBluetoothStream)
  public
    function CreatePacket: TBytes;
    function CreateHeaderPacket: TBytes;
    function CreateDataPacket: TBytes;
  end;

  TBluetoothStreamReceiver = class(TBluetoothStream)
  public
    procedure ProcessPacket(const Packet: TBytes);
    procedure ProcessHeaderPacket(const Packet: TBytes);
    procedure ProcessDataPacket(const Packet: TBytes);
  end;

implementation

{ TStreamPacket }

procedure TBluetoothStreamReceiver.ProcessDataPacket(const Packet: TBytes);
begin
  FStream.Write(Packet[1], CalculatePacketLength);
end;

procedure TBluetoothStreamReceiver.ProcessHeaderPacket(const Packet: TBytes);
var
  Size: Int64;
begin
  Move(Packet[1], Size, SizeOf(Size));
  FStream.Position := 0;
  FStream.Size := Size;
  FStarted := True;
end;

procedure TBluetoothStreamReceiver.ProcessPacket(const Packet: TBytes);
var
  PacketLength: Integer;
  Buffer: TBytes;
  Size: Int64;
begin
  if (Packet = nil) or (Length(Packet) = 0) then
    Exit;

  FLastPosition := FStream.Position;

  case Packet[0] of
    0: ProcessHeaderPacket(Packet);
    1: ProcessDataPacket(Packet);
  end;

  if Assigned(FOnBluetoothStreamProgressEvent) then
    FOnBluetoothStreamProgressEvent(Self, Progress, BytesProcessed);
end;

{ TStreamSender }

function TBluetoothStream.CalculatePacketLength: Integer;
begin
  if (FStream.Size >= (FStream.Position + kPacketLength)) then
    Result := kPacketLength
  else
    Result := FStream.Size - FStream.Position;
end;

constructor TBluetoothStream.Create(AStream: TStream);
begin
  FStream := AStream;
end;

function TBluetoothStreamSender.CreateDataPacket: TBytes;
var
  PacketLength: Integer;
begin
  PacketLength := CalculatePacketLength;
  SetLength(Result, PacketLength + 1);
  Result[0] := 1;
  FStream.Read(Result[1], PacketLength);
end;

function TBluetoothStreamSender.CreateHeaderPacket: TBytes;
var
  Size: Int64;
begin
  FStream.Position := 0;
  Size := FStream.Size;
  SetLength(Result, SizeOf(Byte) + SizeOf(Int64));
  Result[0] := 0;
  Move(Size, Result[1], SizeOf(Int64));
  FStarted := True;
end;

function TBluetoothStreamSender.CreatePacket: TBytes;
begin
  FLastPosition := FStream.Position;
  if not FStarted then
    Result := CreateHeaderPacket
  else
    Result := CreateDataPacket;

  if Assigned(FOnBluetoothStreamProgressEvent) then
    FOnBluetoothStreamProgressEvent(Self, Progress, BytesProcessed);
end;

function TBluetoothStream.GetBytesProcessed: Int64;
begin
  Result := Stream.Position;
end;

function TBluetoothStream.GetFinished: Boolean;
begin
  if not FStarted then
    Result := False
  else
    Result := FStream.Position = FStream.Size;
end;

function TBluetoothStream.GetProgress: Integer;
begin
  if FStream.Size = 0 then
    Result := 0
  else
    Result := Round((FStream.Position + 1) / FStream.Size * 100);
end;

procedure TBluetoothStream.Reset;
begin
  FStarted := False;
end;

end.
