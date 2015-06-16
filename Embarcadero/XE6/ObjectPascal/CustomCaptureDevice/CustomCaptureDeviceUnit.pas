
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit CustomCaptureDeviceUnit;

interface

uses 
  System.Classes, System.Types, System.SysUtils, System.Math, System.UIConsts,
  System.UITypes, FMX.Consts, FMX.Types, FMX.Media, FMX.Graphics;

type

  TMyCaptureDevice = class;

{ TMyCaptureThread }

  TMyCaptureThread = class(TThread)
  private
    FDevice: TMyCaptureDevice;
    FTime: Int64;
    procedure DoSynchronizedSampleBufferReady;
  protected
    procedure Execute; override;
  public
    constructor Create(ADevice: TMyCaptureDevice);
  end;

{ TMyCaptureDevice }

  // This sample capture device. This device do useless task - just as example how to create custom capture video device

  TMyCaptureDevice = class(TVideoCaptureDevice)
  private
    FThread: TMyCaptureThread;
  protected
    function GetDeviceProperty(const Prop: TCaptureDevice.TProperty): string; override;
    function GetDeviceState: TCaptureDeviceState; override;
    procedure DoStartCapture; override;
    procedure DoStopCapture; override;
    procedure DoSampleBufferToBitmap(const ABitmap: TBitmap; const ASetSize: Boolean); override;
  public
    constructor Create(const AManager: TCaptureDeviceManager; const ADefault: Boolean); override;
  end;

implementation

{ TMyCaptureThread }

constructor TMyCaptureThread.Create(ADevice: TMyCaptureDevice);
begin
  inherited Create;
  FDevice := ADevice;
  FreeOnTerminate := True;
end;

procedure TMyCaptureThread.DoSynchronizedSampleBufferReady;
begin
  FDevice.SampleBufferReady(FTime);
end;


procedure TMyCaptureThread.Execute;
const
  FrameRate = 25; // frame per second
begin
  FTime := 0;
  while not Terminated do
  begin
    Synchronize(DoSynchronizedSampleBufferReady);
    Sleep(round(1000 / FrameRate));
    FTime := FTime + FrameRate;
  end;
end;

{ TMyCaptureDevice }

constructor TMyCaptureDevice.Create(const AManager: TCaptureDeviceManager; const ADefault: Boolean);
begin
  inherited Create(AManager, False);
end;

procedure TMyCaptureDevice.DoSampleBufferToBitmap(const ABitmap: TBitmap; const ASetSize: Boolean);
var
  I, J: Integer;
  M: TBitmapData;
begin
  if ASetSize then
    ABitmap.SetSize(320, 240);

  if ABitmap.Map(TMapAccess.Write, M) then
  try
    for I := 0 to ABitmap.Width - 1 do
      for J := 0 to ABitmap.Height - 1 do
        M.SetPixel(I, J, $FF000000 or random($FFFFFF));
  finally
    ABitmap.Unmap(M);
  end;
end;

procedure TMyCaptureDevice.DoStartCapture;
begin
  if FThread = nil then
    FThread := TMyCaptureThread.Create(Self);
end;

procedure TMyCaptureDevice.DoStopCapture;
begin
  if FThread <> nil then
  begin
    FThread.Terminate;
    FThread := nil;
  end;
end;

function TMyCaptureDevice.GetDeviceProperty(const Prop: TCaptureDevice.TProperty): string;
begin
  case Prop of
    TProperty.Description: 
      Result := 'This is example capture device.';
    TProperty.Name:
      Result := 'My Video Generator';
    TProperty.UniqueID:
      Result := IntToStr(Integer(TMyCaptureDevice));
  else
    Result := '';
  end;
end;

function TMyCaptureDevice.GetDeviceState: TCaptureDeviceState;
begin
  if FThread <> nil then
    Result := TCaptureDeviceState.Capturing
  else
    Result := TCaptureDeviceState.Stopped
end;

initialization
  TCaptureDeviceManager.RegisterCaptureDeviceClass(TMyCaptureDevice);
end.
