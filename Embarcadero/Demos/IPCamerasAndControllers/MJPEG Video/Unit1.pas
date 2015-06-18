unit Unit1;

interface

uses
  Winapi.Windows, System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.Objects, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, IdHTTP, IdIntercept, IdIOHandler, IdIOHandlerSocket,
  IdIOHandlerStack, IdGlobal;

type

  TMemoryStreamBuffer = class(TMemoryStream)
  private

    // Events
    FOnWrite: TNotifyEvent;

    // Event triggering
    procedure TriggerOnWrite;

  public

    // Buffer management
    procedure Delete(Amount: Integer);

    // Write overrides
    function Write(const Buffer; Count: Longint): Longint; override;
    function Write(const Buffer: TBytes; Offset, Count: Longint): Longint; override;

    // Events
    property OnWrite: TNotifyEvent read FOnWrite write FOnWrite;

  end;

  TForm1 = class(TForm)
    btnGetMJPEG: TButton;
    imgCameraImage: TImage;
    idHTTP: TIdHTTP;
    idIOHandler: TIdIOHandlerStack;
    idConnectionIntercept: TIdConnectionIntercept;
    procedure btnGetMJPEGClick(Sender: TObject);
    procedure idConnectionInterceptReceive(ASender: TIdConnectionIntercept;
      var ABuffer: TIdBytes);
  private

    // Parsing
    FReceivedData: TMemoryStreamBuffer;
    FParsedData: TMemoryStreamBuffer;

    procedure ParseJPEGData;

  public

    // Constructor and destructor
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

{$REGION 'TMemoryStreamBuffer'}
////////////////////////////////////////////////////////////////////////////////
// Name: TriggerOnWrite
//
// Description:
//   Trigger the OnWrite event
//
// Author: Éric Fleming Bonilha
//
// Parameters List:
//   None
//
// Returns:
//   None
////////////////////////////////////////////////////////////////////////////////
procedure TMemoryStreamBuffer.TriggerOnWrite;
begin

  if Assigned(FOnWrite) then
    FOnWrite(Self);

end;

////////////////////////////////////////////////////////////////////////////////
// Name: Delete
//
// Description:
//   Delete the amount of specified bytes
//
// Author: Éric Fleming Bonilha
//
// Parameters List:
//   None
//
// Returns:
//   None
////////////////////////////////////////////////////////////////////////////////
procedure TMemoryStreamBuffer.Delete(Amount: Integer);
begin

  // Check size
  if (Amount < 0) then
    Amount := 0
  else if Amount > Size then
    Amount := Size;

  // Delete from buffer
  CopyMemory(Memory, @TBytes(Memory)[Amount], Size - Amount);
  Size := Size - Amount;

end;

////////////////////////////////////////////////////////////////////////////////
// Name: Write
//
// Description:
//   Write buffer
//
// Author: Éric Fleming Bonilha
//
// Parameters List:
//   [IN] Buffer - Data to read
//   [IN] Count  - Amount of data
//
// Returns:
//   [Longint] - Data written
////////////////////////////////////////////////////////////////////////////////
function TMemoryStreamBuffer.Write(const Buffer; Count: Longint): Longint;
begin

  Result := inherited;

  // Trigger the write event
  TriggerOnWrite;

end;

////////////////////////////////////////////////////////////////////////////////
// Name: Write
//
// Description:
//   Write data to buffer
//
// Author: Éric Fleming Bonilha
//
// Parameters List:
//   [IN] Buffer - Source data
//   [IN] Offset - Offset from source
//   [IN] Count  - Amount of bytes to read from
//
// Returns:
//   [Longint] - Amount of written data
////////////////////////////////////////////////////////////////////////////////
function TMemoryStreamBuffer.Write(const Buffer: TBytes; Offset, Count: Longint): Longint;
begin

  Result := inherited;

  // Trigger the write event
  TriggerOnWrite;

end;
{$ENDREGION}

////////////////////////////////////////////////////////////////////////////////
// Name: Create
//
// Description:
//   Class constructor
//
// Author: Éric Fleming Bonilha
//
// Parameters List:
//   [IN] AOwner - Component owner
//
// Returns:
//   None
////////////////////////////////////////////////////////////////////////////////
constructor TForm1.Create(AOwner: TComponent);
begin

  inherited;

  // Create buffers
  FReceivedData := TMemoryStreamBuffer.Create;
  FParsedData   := TMemoryStreamBuffer.Create;

end;

////////////////////////////////////////////////////////////////////////////////
// Name: Destroy
//
// Description:
//   Class destructor
//
// Author: Éric Fleming Bonilha
//
// Parameters List:
//   None
//
// Returns:
//   None
////////////////////////////////////////////////////////////////////////////////
destructor TForm1.Destroy;
begin

  FParsedData.Free;
  FReceivedData.Free;

  inherited;

end;

////////////////////////////////////////////////////////////////////////////////
// Name: btnGetMJPEGClick
//
// Description:
//   Start MJPEG transmission
//
// Author: Éric Fleming Bonilha
//
// Parameters List:
//   [IN] Sender - Sender object
//
// Returns:
//   None
////////////////////////////////////////////////////////////////////////////////
procedure TForm1.btnGetMJPEGClick(Sender: TObject);
begin

  // Request single JPEG from camera
  IdHTTP.Request.BasicAuthentication := TRUE;
  IdHTTP.Request.Username            := 'convidado';
  IdHTTP.Request.Password            := 'convidado';
  IdHTTP.Get('http://10.1.10.10:8601/Interface/Cameras/GetJPEGStream?Camera=40', FReceivedData);

end;

////////////////////////////////////////////////////////////////////////////////
// Name: ParseJPEGData
//
// Description:
//   Parse JPEG data
//
// Author: Éric Fleming Bonilha
//
// Parameters List:
//   None
//
// Returns:
//   None
////////////////////////////////////////////////////////////////////////////////
procedure TForm1.ParseJPEGData;
var
  i: Integer;
  StartPos, EndPos: Integer;
  StartPt: Pointer;
  Data, DataNext: PByte;
  Amount: Integer;
begin

  // Check size
  if (FReceivedData.Size < 2) then
    Exit;

  // Initialize
  Amount   := 0;
  StartPos := -1;
  StartPt  := nil;

  // Data pointers
  Data     := FReceivedData.Memory;
  DataNext := Data;
  Inc(DataNext);

  try

    // Parsing loop
    for i := 0 to FReceivedData.Size -2 do
    begin

      // Check if we have found the start code
      if (StartPos = -1) and (Data^ = $FF) and (DataNext^ = $D8) then
      begin
        StartPos := i;
        StartPt  := Data;
      end

      // Check if we have found the JPEG end code
      else if (StartPos >= 0) and (Data^ = $FF) and (DataNext^ = $D9) then
      begin

        // End position
        EndPos := i;

        // Copy data (Higher performance than passing whole stream to load,
        // because FBitmap.LoadFromStream will create a memory stream and copy
        // everything if Position <> 0
        FReceivedData.Position := StartPos;
        FParsedData.SetSize((EndPos + 2) - StartPos);
        FParsedData.Position := 0;
        CopyMemory(FParsedData.Memory, StartPt, (EndPos + 2) - StartPos);

        // Load bitmap on screen
        imgCameraImage.Bitmap.LoadFromStream(FParsedData);
        Application.ProcessMessages;

        // Amount of data to delete
        Amount := EndPos + 2;

        // Reset
        StartPos := -1;
        StartPt  := nil;

      end;

      // Next pointer
      Inc(Data);
      Inc(DataNext);

    end;

    // Delete the amount of parsed data
    FReceivedData.Delete(Amount);
    FReceivedData.Position := FReceivedData.Size;

  except
    FReceivedData.Clear;
  end;

end;

////////////////////////////////////////////////////////////////////////////////
// Name: idConnectionInterceptReceive
//
// Description:
//   Process the event that data has been received
//
// Author: Éric Fleming Bonilha
//
// Parameters List:
//   [IN] ASender - Sender object
//   [IN] ABuffer - Received data
//
// Returns:
//   None
////////////////////////////////////////////////////////////////////////////////
procedure TForm1.idConnectionInterceptReceive(ASender: TIdConnectionIntercept;
  var ABuffer: TIdBytes);
begin

  ParseJPEGData;

end;

end.
