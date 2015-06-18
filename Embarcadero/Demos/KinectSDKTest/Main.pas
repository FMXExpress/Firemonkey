unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, NuiApi, NuiSensor, EventDispatcherThread, StdCtrls, ExtCtrls;

type
  TMainForm = class(TForm)
    rightPanel: TPanel;
    leftPanel: TPanel;
    ckPlayer: TCheckBox;
    ckHands: TCheckBox;
    ckDepth: TCheckBox;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private-Deklarationen }
    FskeletonEvents,
    FdepthEvents : TEventDispatcherThread;

    Fsensor : INuiSensor;
    FdepthEvent,
    FskeletonEvent,
    FdepthStream : NativeUint;

    Fbitmap : TBitmap;

    function openFirstSensor : boolean;

    procedure eventDispatcher(var msg : TMessage); message WM_USER;

    procedure OnNewSkeletonFrame;
    procedure OnNewDepthFrame;
  public
    { Public-Deklarationen }
  end;

var
  MainForm: TMainForm;

implementation

uses NuiImageCamera, NuiSkeleton;

{$R *.dfm}

procedure TMainForm.eventDispatcher(var msg: TMessage);
begin
  if msg.WParam = cardinal(FskeletonEvents) then
    OnNewSkeletonFrame
  else if msg.WParam = cardinal(FdepthEvents) then
    OnNewDepthFrame;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FskeletonEvent := INVALID_HANDLE_VALUE;
  FdepthEvent := INVALID_HANDLE_VALUE;
  FdepthStream := INVALID_HANDLE_VALUE;

  if not LoadNuiLibrary then exit;

  openFirstSensor;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if assigned(Fsensor) then begin
    Fsensor.NuiSkeletonTrackingDisable;
    Fsensor.NuiShutdown;
  end;

  if FskeletonEvent <> INVALID_HANDLE_VALUE then
    CloseHandle(FskeletonEvent);
  if FdepthEvent <> INVALID_HANDLE_VALUE then
    CloseHandle(FdepthEvent);

  if assigned(FskeletonEvents) then begin
    FskeletonEvents.Terminate;
    FskeletonEvents.WaitFor;
    FskeletonEvents.Free;
  end;
  if assigned(FdepthEvents) then begin
    FdepthEvents.Terminate;
    FdepthEvents.WaitFor;
    FdepthEvents.Free;
  end;
end;

procedure TMainForm.OnNewDepthFrame;
var
  imageFrame : NUI_IMAGE_FRAME;
  texture : INuiFrameTexture;
  lock : NUI_LOCKED_RECT;

  depth : pword;
  color : pinteger;
  x, y : integer;
  t1, t2, freq : int64;

  w, h : cardinal;
begin
  QueryPerformanceCounter(t1);

  if (FdepthStream = INVALID_HANDLE_VALUE) or failed(Fsensor.
    NuiImageStreamGetNextFrame(FdepthStream, 0, @imageFrame)) then exit;

  NuiImageResolutionToSize(imageFrame.eResolution, w, h); 

  try
    texture := imageFrame.pFrameTexture;
    if not assigned(texture) then exit;

    if failed(texture.LockRect(0, @lock, nil, 0)) then exit;
    try
      if cardinal(lock.Pitch) <> (2 * w) then exit;

      depth := lock.pBits;
      for y := 0 to h - 1 do begin
        color := Fbitmap.ScanLine[y];
        for x := 0 to w - 1 do begin
          if ckPlayer.Checked and ((depth^ and NUI_IMAGE_PLAYER_INDEX_MASK) <> 0) then
            color^:= $FF
          else if ckDepth.Checked and (depth^ >= NUI_IMAGE_DEPTH_MINIMUM_NEAR_MODE) and
            (depth^ <= NUI_IMAGE_DEPTH_MAXIMUM_NEAR_MODE) then begin
              color^ := round(((depth^ shr NUI_IMAGE_PLAYER_INDEX_SHIFT) -
              (NUI_IMAGE_DEPTH_MINIMUM_NEAR_MODE shr
                NUI_IMAGE_PLAYER_INDEX_SHIFT)) /
              (NUI_IMAGE_DEPTH_MAXIMUM_NEAR_MODE shr
                NUI_IMAGE_PLAYER_INDEX_SHIFT) * 255);
                
            color^:= (color^ and $FF) or ((color^ and $FF) shl 8) or
              ((color^ and $FF) shl 16);
          end else
            color^ := 0;

          inc(color);
          inc(depth);
        end;
      end;

    finally
      texture.UnlockRect(0);
    end;

  finally
    Fsensor.NuiImageStreamReleaseFrame(FdepthStream, @imageFrame);
  end;

  Canvas.Draw(0, 0, Fbitmap);

  QueryPerformanceCounter(t2);
  QueryPerformanceFrequency(freq);
end;

procedure TMainForm.OnNewSkeletonFrame;
var
  frame : NUI_SKELETON_FRAME;
  sp : NUI_TRANSFORM_SMOOTH_PARAMETERS;

  index,
  i : integer;
  px, py : single;
begin
  if not ckHands.Checked then exit;

  fillchar(frame, sizeof(frame), 0);

  if failed(Fsensor.NuiSkeletonGetNextFrame(0, @frame)) then exit;

  index := -1;
  for i := 0 to NUI_SKELETON_COUNT - 1 do
    if frame.SkeletonData[i].eTrackingState =
                                    NUI_SKELETON_TRACKED then begin
      index := i;
      break;
    end;

  if index < 0 then exit;

	sp.fSmoothing := 0.7;
	sp.fCorrection := 0.3;
	sp.fPrediction := 0.4;
	sp.fJitterRadius := 1.0;
	sp.fMaxDeviationRadius := 0.5;

  if failed(Fsensor.NuiTransformSmooth(@frame, @sp)) then exit;

  if frame.SkeletonData[index].eSkeletonPositionTrackingState[
    integer(NUI_SKELETON_POSITION_HAND_LEFT)] =
      NUI_SKELETON_POSITION_TRACKED then begin

    NuiTransformSkeletonToDepthImage(frame.SkeletonData[index].
      SkeletonPositions[integer(NUI_SKELETON_POSITION_HAND_LEFT)], px, py,
        NUI_IMAGE_RESOLUTION_640x480);

    leftPanel.Visible := true;
    leftPanel.Left := round(px) - leftPanel.Width shr 1;
    leftPanel.Top := round(py) - leftPanel.Height shr 1;
  end else
    leftPanel.Visible := false;

  if frame.SkeletonData[index].eSkeletonPositionTrackingState[
    integer(NUI_SKELETON_POSITION_HAND_RIGHT)] =
      NUI_SKELETON_POSITION_TRACKED then begin

    NuiTransformSkeletonToDepthImage(frame.SkeletonData[index].
      SkeletonPositions[integer(NUI_SKELETON_POSITION_HAND_RIGHT)], px, py,
        NUI_IMAGE_RESOLUTION_640x480);

    rightPanel.Visible := true;
    rightPanel.Left := round(px) - rightPanel.Width shr 1;
    rightPanel.Top := round(py) - rightPanel.Height shr 1;
  end else
    rightPanel.Visible := false;
end;

function TMainForm.openFirstSensor : boolean;
var
  sensorEnum : INuiSensor;
  count,
  i : integer;
  w, h: cardinal;
  format : NUI_IMAGE_RESOLUTION;
begin
  result := false;
  Fsensor := nil;

  format := NUI_IMAGE_RESOLUTION_640x480;

  if failed(NuiGetSensorCount(count)) then exit;

  for i := 0 to count - 1 do begin
    if failed(NuiCreateSensorByIndex(i, sensorEnum)) then continue;

    if sensorEnum.NuiStatus = S_OK then begin
      Fsensor := sensorEnum;
      break;
    end;
  end;

  if not assigned(Fsensor) then exit;

  if failed(Fsensor.NuiInitialize(NUI_INITIALIZE_FLAG_USES_SKELETON or
    NUI_INITIALIZE_FLAG_USES_DEPTH_AND_PLAYER_INDEX)) then exit;

  FskeletonEvent := CreateEvent(nil, true, false, nil);
  FdepthEvent := CreateEvent(nil, true, false, nil);

  Fsensor.NuiSkeletonTrackingEnable(FskeletonEvent,
    NUI_SKELETON_TRACKING_FLAG_ENABLE_SEATED_SUPPORT or
    NUI_SKELETON_TRACKING_FLAG_ENABLE_IN_NEAR_RANGE);
  FskeletonEvents := TEventDispatcherThread.createWith(Handle,
    FskeletonEvent);

  if failed(Fsensor.NuiImageStreamOpen(NUI_IMAGE_TYPE_DEPTH_AND_PLAYER_INDEX,
    format, 0, 2, FdepthEvent, FdepthStream)) then exit;

  if failed(Fsensor.NuiImageStreamSetImageFrameFlags(FdepthStream,
    NUI_IMAGE_STREAM_FLAG_ENABLE_NEAR_MODE)) then exit;

  FdepthEvents := TEventDispatcherThread.createWith(Handle,
    FdepthEvent);

  NuiImageResolutionToSize(format, w, h);

  Fbitmap := TBitmap.Create;
  Fbitmap.Width := w;
  Fbitmap.Height := h;
  Fbitmap.PixelFormat := pf32bit;

  ClientWidth := w;
  ClientHeight := h + cardinal(panel1.Height);

  result := true;
end;

end.
