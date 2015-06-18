(************************************************************************
*                                                                       *
*   NuiApi.h -- This module aggregates all the Natural User             *
*               Interface(NUI) API headers.                             *
*                                                                       *
*   Copyright (c) Microsoft Corp. All rights reserved.                  *
*                                                                       *
************************************************************************)
{$Z4}
unit NuiApi;

interface

uses Windows, NuiSensor, NuiSkeleton, NuiImageCamera;

//
// NUI Common Initialization Declarations
//

const
  DLL_NAME = 'kinect10.dll';

  NUI_INITIALIZE_FLAG_USES_AUDIO                  = $10000000;
  NUI_INITIALIZE_FLAG_USES_DEPTH_AND_PLAYER_INDEX = $00000001;
  NUI_INITIALIZE_FLAG_USES_COLOR                  = $00000002;
  NUI_INITIALIZE_FLAG_USES_SKELETON               = $00000008;
  NUI_INITIALIZE_FLAG_USES_DEPTH                  = $00000020;
  NUI_INITIALIZE_FLAG_USES_HIGH_QUALITY_COLOR     = $00000040; // implies COLOR stream will be from uncompressed YUY2 @ 15fps

  NUI_INITIALIZE_DEFAULT_HARDWARE_THREAD          = $FFFFFFFF;


/// <summary>
/// Initializes the sensor. If the sensor is already initialized, this will shut down the sensor
/// and reinitialize it.
/// </summary>
/// <param name="dwFlags">
/// The NUI subsystems to initialize, as a bitwise-OR combination of the NUI_INITIALIZE constants.
/// </param>
/// <returns>
/// <para>Returns S_OK if successful; otherwise, returns a failure code.</para>
/// </returns>

type
  TNuiInitialize = function (dwFlags : DWORD) : HRESULT; stdcall;

/// <summary>
/// Shuts down the sensor. If the sensor is already shut down, nothing happens.
/// </summary>
  TNuiShutdown = procedure; stdcall;
//
// Define NUI specific error codes
// **** ALSO DEFINED IN NuiError.h.  Keep in sync! ****
//

//
// Define NUI error codes derived from win32 errors
//

const
  ERROR_DEVICE_NOT_CONNECTED  = 1167;

  E_NUI_DEVICE_NOT_CONNECTED  = ERROR_DEVICE_NOT_CONNECTED;
  E_NUI_DEVICE_NOT_READY      = ERROR_NOT_READY;
  E_NUI_ALREADY_INITIALIZED   = ERROR_ALREADY_INITIALIZED;
  E_NUI_NO_MORE_ITEMS         = ERROR_NO_MORE_ITEMS;

  FACILITY_NUI = $301;
  S_NUI_INITIALIZING                      = $03010001;
  E_NUI_FRAME_NO_DATA                     = $83010001;
  E_NUI_STREAM_NOT_ENABLED                = $83010002;
  E_NUI_IMAGE_STREAM_IN_USE               = $83010003;
  E_NUI_FRAME_LIMIT_EXCEEDED              = $83010004;
  E_NUI_FEATURE_NOT_INITIALIZED           = $83010005;
  E_NUI_NOTGENUINE                        = $83010006;
  E_NUI_INSUFFICIENTBANDWIDTH             = $83010007;
  E_NUI_NOTSUPPORTED                      = $83010008;
  E_NUI_DEVICE_IN_USE                     = $83010009;

  E_NUI_DATABASE_NOT_FOUND                = $8301000D;
  E_NUI_DATABASE_VERSION_MISMATCH         = $8301000E;
// The requested feateure is not available on this version of the hardware
  E_NUI_HARDWARE_FEATURE_UNAVAILABLE      = $8301000F;
// The hub is no longer connected to the machine
  E_NUI_NOTCONNECTED                      = $83010014;
// Some part of the device is not connected.
  E_NUI_NOTREADY                          = $83010015;
// Skeletal engine is already in use
  E_NUI_SKELETAL_ENGINE_BUSY              = $830100AA;
// The hub and motor are connected, but the camera is not
  E_NUI_NOTPOWERED                        = $8301027F;
// Bad index passed in to NuiCreateInstanceByXXX
  E_NUI_BADINDEX                          = $83010585;
  E_NUI_BADIINDEX                         = E_NUI_BADINDEX; // V 1.0 compatibility

var
  NuiInitialize : TNuiInitialize = nil;
  NuiShutdown : TNuiShutdown = nil;
  NuiGetSensorCount : TNuiGetSensorCount = nil;
  NuiCreateSensorByIndex : TNuiCreateSensorByIndex = nil;
  NuiCreateSensorById : TNuiCreateSensorById = nil;
  NuiGetAudioSource : TNuiGetAudioSource = nil;
  NuiSetDeviceStatusCallback : TNuiSetDeviceStatusCallback = nil;
//  NuiGetMicrophoneArrayDevices : TNuiGetMicrophoneArrayDevices = nil;
//  NuiGetSpeakerDevices : TNuiGetSpeakerDevices = nil;
  NuiSkeletonTrackingEnable : TNuiSkeletonTrackingEnable = nil;
  NuiSkeletonTrackingDisable : TNuiSkeletonTrackingDisable = nil;
  NuiSkeletonGetNextFrame : TNuiSkeletonGetNextFrame = nil;
  NuiSkeletonSetTrackedSkeletons : TNuiSkeletonSetTrackedSkeletons = nil;
  NuiTransformSmooth : TNuiTransformSmooth = nil;
  NuiSkeletonCalculateBoneOrientations : TNuiSkeletonCalculateBoneOrientations = nil;
  NuiImageStreamSetImageFrameFlags : TNuiImageStreamSetImageFrameFlags = nil;
  NuiImageStreamGetImageFrameFlags : TNuiImageStreamGetImageFrameFlags = nil;
  NuiSetFrameEndEvent : TNuiSetFrameEndEvent = nil;
  NuiImageStreamOpen : TNuiImageStreamOpen = nil;
  NuiImageStreamGetNextFrame : TNuiImageStreamGetNextFrame = nil;
  NuiImageStreamReleaseFrame : TNuiImageStreamReleaseFrame = nil;
  NuiImageGetColorPixelCoordinatesFromDepthPixel : TNuiImageGetColorPixelCoordinatesFromDepthPixel = nil;
  NuiImageGetColorPixelCoordinatesFromDepthPixelAtResolution : TNuiImageGetColorPixelCoordinatesFromDepthPixelAtResolution = nil;
  NuiCameraElevationGetAngle : TNuiCameraElevationGetAngle = nil;
  NuiCameraElevationSetAngle : TNuiCameraElevationSetAngle = nil;

function LoadNuiLibrary(filename : string = DLL_NAME) : boolean;
procedure UnloadNuiLibrary;

implementation

var
  _libModule : HMODULE = 0;


function LoadNuiLibrary(filename : string) : boolean;
begin
  result := false;
  if _libModule <> 0 then exit;

  _libModule := LoadLibrary(PChar(filename));
  if _libModule = 0 then exit;

  NuiInitialize := GetProcAddress(_libModule, 'NuiInitialize');
  if not assigned(NuiInitialize) then exit;

  NuiShutdown := GetProcAddress(_libModule, 'NuiShutdown');
  if not assigned(NuiShutdown) then exit;

  NuiGetSensorCount := GetProcAddress(_libModule, 'NuiGetSensorCount');
  if not assigned(NuiGetSensorCount) then exit;

  NuiCreateSensorByIndex := GetProcAddress(_libModule, 'NuiCreateSensorByIndex');
  if not assigned(NuiCreateSensorByIndex) then exit;

  NuiCreateSensorById := GetProcAddress(_libModule, 'NuiCreateSensorById');
  if not assigned(NuiCreateSensorById) then exit;

  NuiGetAudioSource := GetProcAddress(_libModule, 'NuiGetAudioSource');
  if not assigned(NuiGetAudioSource) then exit;

  NuiSetDeviceStatusCallback := GetProcAddress(_libModule, 'NuiSetDeviceStatusCallback');
  if not assigned(NuiSetDeviceStatusCallback) then exit;

//  NuiGetMicrophoneArrayDevices := GetProcAddress(_libModule, 'NuiGetMicrophoneArrayDevices');
//  if not assigned(NuiGetMicrophoneArrayDevices) then exit;

//  NuiGetSpeakerDevices := GetProcAddress(_libModule, 'NuiGetSpeakerDevices');
//  if not assigned(NuiGetSpeakerDevices) then exit;

  NuiSkeletonTrackingEnable := GetProcAddress(_libModule, 'NuiSkeletonTrackingEnable');
  if not assigned(NuiSkeletonTrackingEnable) then exit;

  NuiSkeletonTrackingDisable := GetProcAddress(_libModule, 'NuiSkeletonTrackingDisable');
  if not assigned(NuiSkeletonTrackingDisable) then exit;

  NuiSkeletonGetNextFrame := GetProcAddress(_libModule, 'NuiSkeletonGetNextFrame');
  if not assigned(NuiSkeletonGetNextFrame) then exit;

  NuiSkeletonSetTrackedSkeletons := GetProcAddress(_libModule, 'NuiSkeletonSetTrackedSkeletons');
  if not assigned(NuiSkeletonSetTrackedSkeletons) then exit;

  NuiTransformSmooth := GetProcAddress(_libModule, 'NuiTransformSmooth');
  if not assigned(NuiTransformSmooth) then exit;

  NuiSkeletonCalculateBoneOrientations := GetProcAddress(_libModule, 'NuiSkeletonCalculateBoneOrientations');
  if not assigned(NuiSkeletonCalculateBoneOrientations) then exit;

  NuiImageStreamSetImageFrameFlags := GetProcAddress(_libModule, 'NuiImageStreamSetImageFrameFlags');
  if not assigned(NuiImageStreamSetImageFrameFlags) then exit;

  NuiImageStreamGetImageFrameFlags := GetProcAddress(_libModule, 'NuiImageStreamGetImageFrameFlags');
  if not assigned(NuiImageStreamGetImageFrameFlags) then exit;

  NuiSetFrameEndEvent := GetProcAddress(_libModule, 'NuiSetFrameEndEvent');
  if not assigned(NuiSetFrameEndEvent) then exit;

  NuiImageStreamOpen := GetProcAddress(_libModule, 'NuiImageStreamOpen');
  if not assigned(NuiImageStreamOpen) then exit;

  NuiImageStreamGetNextFrame := GetProcAddress(_libModule, 'NuiImageStreamGetNextFrame');
  if not assigned(NuiImageStreamGetNextFrame) then exit;

  NuiImageStreamReleaseFrame := GetProcAddress(_libModule, 'NuiImageStreamReleaseFrame');
  if not assigned(NuiImageStreamReleaseFrame) then exit;

  NuiImageGetColorPixelCoordinatesFromDepthPixel := GetProcAddress(_libModule, 'NuiImageGetColorPixelCoordinatesFromDepthPixel');
  if not assigned(NuiImageGetColorPixelCoordinatesFromDepthPixel) then exit;

  NuiImageGetColorPixelCoordinatesFromDepthPixelAtResolution := GetProcAddress(_libModule, 'NuiImageGetColorPixelCoordinatesFromDepthPixelAtResolution');
  if not assigned(NuiImageGetColorPixelCoordinatesFromDepthPixelAtResolution) then exit;

  NuiCameraElevationGetAngle := GetProcAddress(_libModule, 'NuiCameraElevationGetAngle');
  if not assigned(NuiCameraElevationGetAngle) then exit;

  NuiCameraElevationSetAngle := GetProcAddress(_libModule, 'NuiCameraElevationSetAngle');
  if not assigned(NuiCameraElevationSetAngle) then exit;

  result := true;
end;

procedure UnloadNuiLibrary;
begin
  NuiInitialize := nil;
  NuiShutdown := nil;
  NuiGetSensorCount := nil;
  NuiCreateSensorByIndex := nil;
  NuiCreateSensorById := nil;
  NuiGetAudioSource := nil;
  NuiSetDeviceStatusCallback := nil;
//  NuiGetMicrophoneArrayDevices := nil;
//  NuiGetSpeakerDevices := nil;
  NuiSkeletonTrackingEnable := nil;
  NuiSkeletonTrackingDisable := nil;
  NuiSkeletonGetNextFrame := nil;
  NuiSkeletonSetTrackedSkeletons := nil;
  NuiTransformSmooth := nil;
  NuiSkeletonCalculateBoneOrientations := nil;
  NuiImageStreamSetImageFrameFlags := nil;
  NuiImageStreamGetImageFrameFlags := nil;
  NuiSetFrameEndEvent := nil;
  NuiImageStreamOpen := nil;
  NuiImageStreamGetNextFrame := nil;
  NuiImageStreamReleaseFrame := nil;
  NuiImageGetColorPixelCoordinatesFromDepthPixel := nil;
  NuiImageGetColorPixelCoordinatesFromDepthPixelAtResolution := nil;
  NuiCameraElevationGetAngle := nil;
  NuiCameraElevationSetAngle := nil;

  if _libModule = 0 then exit;

  FreeLibrary(_libModule);

  _libModule := 0;
end;

initialization

finalization
//  UnloadNuiLibrary;

end.
