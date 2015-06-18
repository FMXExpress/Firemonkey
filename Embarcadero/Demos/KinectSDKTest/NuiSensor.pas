(* this ALWAYS GENERATED file contains the definitions for the interfaces *)


(* File created by MIDL compiler version 7.00.0555 *)
(* at Tue May 15 15:05:27 2012
 *)
(* Compiler settings for ..\inc_public\Kinect.idl:
    Oicf, W1, Zp8, env=Win32 (32b run), target_arch=X86 7.00.0555
    protocol : dce , ms_ext, c_ext, robust
    error checks: allocation ref bounds_check enum stub_data
    VC __declspec() decoration level:
         __declspec(uuid()), __declspec(selectany), __declspec(novtable)
         DECLSPEC_UUID(), MIDL_INTERFACE()
*)
(* @@MIDL_FILE_HEADING(  ) *)
{$Z4}
unit NuiSensor;

interface

uses Windows;

const
  NUI_SKELETON_COUNT = 6;

type
  TVector4 = record
    x,
    y,
    z,
    w : Single;
  end;
  PVector4 = ^Vector4;
  Vector4 = TVector4;

  TMatrix4 = record
    M11, M12, M13, M14,
    M21, M22, M23, M24,
    M31, M32, M33, M34,
    M41, M42, M43, M44 : Single;
  end;
  PMatrix4 = ^Matrix4;
  Matrix4 = TMatrix4;

  TNUI_SKELETON_POSITION_INDEX = (
   NUI_SKELETON_POSITION_HIP_CENTER	= 0,
    NUI_SKELETON_POSITION_SPINE	= ( NUI_SKELETON_POSITION_HIP_CENTER + 1 ) ,
    NUI_SKELETON_POSITION_SHOULDER_CENTER	= ( NUI_SKELETON_POSITION_SPINE + 1 ) ,
    NUI_SKELETON_POSITION_HEAD	= ( NUI_SKELETON_POSITION_SHOULDER_CENTER + 1 ) ,
    NUI_SKELETON_POSITION_SHOULDER_LEFT	= ( NUI_SKELETON_POSITION_HEAD + 1 ) ,
    NUI_SKELETON_POSITION_ELBOW_LEFT	= ( NUI_SKELETON_POSITION_SHOULDER_LEFT + 1 ) ,
    NUI_SKELETON_POSITION_WRIST_LEFT	= ( NUI_SKELETON_POSITION_ELBOW_LEFT + 1 ) ,
    NUI_SKELETON_POSITION_HAND_LEFT	= ( NUI_SKELETON_POSITION_WRIST_LEFT + 1 ) ,
    NUI_SKELETON_POSITION_SHOULDER_RIGHT	= ( NUI_SKELETON_POSITION_HAND_LEFT + 1 ) ,
    NUI_SKELETON_POSITION_ELBOW_RIGHT	= ( NUI_SKELETON_POSITION_SHOULDER_RIGHT + 1 ) ,
    NUI_SKELETON_POSITION_WRIST_RIGHT	= ( NUI_SKELETON_POSITION_ELBOW_RIGHT + 1 ) ,
    NUI_SKELETON_POSITION_HAND_RIGHT	= ( NUI_SKELETON_POSITION_WRIST_RIGHT + 1 ) ,
    NUI_SKELETON_POSITION_HIP_LEFT	= ( NUI_SKELETON_POSITION_HAND_RIGHT + 1 ) ,
    NUI_SKELETON_POSITION_KNEE_LEFT	= ( NUI_SKELETON_POSITION_HIP_LEFT + 1 ) ,
    NUI_SKELETON_POSITION_ANKLE_LEFT	= ( NUI_SKELETON_POSITION_KNEE_LEFT + 1 ) ,
    NUI_SKELETON_POSITION_FOOT_LEFT	= ( NUI_SKELETON_POSITION_ANKLE_LEFT + 1 ) ,
    NUI_SKELETON_POSITION_HIP_RIGHT	= ( NUI_SKELETON_POSITION_FOOT_LEFT + 1 ) ,
    NUI_SKELETON_POSITION_KNEE_RIGHT	= ( NUI_SKELETON_POSITION_HIP_RIGHT + 1 ) ,
    NUI_SKELETON_POSITION_ANKLE_RIGHT	= ( NUI_SKELETON_POSITION_KNEE_RIGHT + 1 ) ,
    NUI_SKELETON_POSITION_FOOT_RIGHT	= ( NUI_SKELETON_POSITION_ANKLE_RIGHT + 1 ) ,
    NUI_SKELETON_POSITION_COUNT	= ( NUI_SKELETON_POSITION_FOOT_RIGHT + 1 )
  );
  PNUI_SKELETON_POSITION_INDEX = ^NUI_SKELETON_POSITION_INDEX;
  NUI_SKELETON_POSITION_INDEX = TNUI_SKELETON_POSITION_INDEX; 

  TNUI_IMAGE_TYPE = (
    NUI_IMAGE_TYPE_DEPTH_AND_PLAYER_INDEX	= 0,
    NUI_IMAGE_TYPE_COLOR	= ( NUI_IMAGE_TYPE_DEPTH_AND_PLAYER_INDEX + 1 ) ,
    NUI_IMAGE_TYPE_COLOR_YUV	= ( NUI_IMAGE_TYPE_COLOR + 1 ) ,
    NUI_IMAGE_TYPE_COLOR_RAW_YUV	= ( NUI_IMAGE_TYPE_COLOR_YUV + 1 ) ,
    NUI_IMAGE_TYPE_DEPTH	= ( NUI_IMAGE_TYPE_COLOR_RAW_YUV + 1 )
  );
  PNUI_IMAGE_TYPE = ^NUI_IMAGE_TYPE;
  NUI_IMAGE_TYPE = TNUI_IMAGE_TYPE;

  TNUI_IMAGE_RESOLUTION = (
    NUI_IMAGE_RESOLUTION_INVALID	= -1,
    NUI_IMAGE_RESOLUTION_80x60	= 0,
    NUI_IMAGE_RESOLUTION_320x240	= ( NUI_IMAGE_RESOLUTION_80x60 + 1 ) ,
    NUI_IMAGE_RESOLUTION_640x480	= ( NUI_IMAGE_RESOLUTION_320x240 + 1 ) ,
    NUI_IMAGE_RESOLUTION_1280x960	= ( NUI_IMAGE_RESOLUTION_640x480 + 1 )
  );
  PNUI_IMAGE_RESOLUTION = ^NUI_IMAGE_RESOLUTION;
  NUI_IMAGE_RESOLUTION = TNUI_IMAGE_RESOLUTION;

  TNUI_IMAGE_VIEW_AREA = record
    eDigitalZoom : integer;
    lCenterX : Longint;
    lCenterY : Longint;
  end;
  PNUI_IMAGE_VIEW_AREA = ^NUI_IMAGE_VIEW_AREA;
  NUI_IMAGE_VIEW_AREA = TNUI_IMAGE_VIEW_AREA;

  TNUI_TRANSFORM_SMOOTH_PARAMETERS = record
    fSmoothing,
    fCorrection,
    fPrediction,
    fJitterRadius,
    fMaxDeviationRadius : single;
  end;
  PNUI_TRANSFORM_SMOOTH_PARAMETERS = ^NUI_TRANSFORM_SMOOTH_PARAMETERS;
  NUI_TRANSFORM_SMOOTH_PARAMETERS = TNUI_TRANSFORM_SMOOTH_PARAMETERS;

  TNUI_SURFACE_DESC = record
    Width : cardinal;
    Height : cardinal;
  end;
  PNUI_SURFACE_DESC = ^NUI_SURFACE_DESC;
  NUI_SURFACE_DESC = TNUI_SURFACE_DESC;

  TNUI_SKELETON_POSITION_TRACKING_STATE = (
    NUI_SKELETON_POSITION_NOT_TRACKED	= 0,
    NUI_SKELETON_POSITION_INFERRED	= ( NUI_SKELETON_POSITION_NOT_TRACKED + 1 ) ,
    NUI_SKELETON_POSITION_TRACKED	= ( NUI_SKELETON_POSITION_INFERRED + 1 )
  );
  PNUI_SKELETON_POSITION_TRACKING_STATE = ^NUI_SKELETON_POSITION_TRACKING_STATE;
  NUI_SKELETON_POSITION_TRACKING_STATE = TNUI_SKELETON_POSITION_TRACKING_STATE;

  TNUI_SKELETON_TRACKING_STATE = (
    NUI_SKELETON_NOT_TRACKED	= 0,
    NUI_SKELETON_POSITION_ONLY	= ( NUI_SKELETON_NOT_TRACKED + 1 ) ,
    NUI_SKELETON_TRACKED	= ( NUI_SKELETON_POSITION_ONLY + 1 )
  );
  PNUI_SKELETON_TRACKING_STATE = ^NUI_SKELETON_TRACKING_STATE;
  NUI_SKELETON_TRACKING_STATE = TNUI_SKELETON_TRACKING_STATE;

  TNUI_SKELETON_DATA = record
    eTrackingState : NUI_SKELETON_TRACKING_STATE;
    dwTrackingID : DWORD;
    dwEnrollmentIndex : DWORD;
    dwUserIndex : DWORD;
    Position : Vector4;
    SkeletonPositions : array[0..19] of Vector4;
    eSkeletonPositionTrackingState : array[0..19] of NUI_SKELETON_POSITION_TRACKING_STATE;
    dwQualityFlags : DWORD;
  end;
  PNUI_SKELETON_DATA = ^NUI_SKELETON_DATA;
  NUI_SKELETON_DATA = TNUI_SKELETON_DATA;

  (** sure about the alignment, default should be Align 8?!
  *)
  TNUI_SKELETON_FRAME = record
    liTimeStamp : int64;
    dwFrameNumber : DWORD;
    dwFlags : DWORD;
    vFloorClipPlane : Vector4;
    vNormalToGravity : Vector4;
    SkeletonData : array[0..NUI_SKELETON_COUNT - 1] of NUI_SKELETON_DATA;
  end;
  PNUI_SKELETON_FRAME = ^NUI_SKELETON_FRAME;
  NUI_SKELETON_FRAME = TNUI_SKELETON_FRAME;

  TNUI_SKELETON_BONE_ROTATION = record
    rotationMatrix : Matrix4;
    rotationQuaternion : Vector4;
  end;
  PNUI_SKELETON_BONE_ROTATION = ^NUI_SKELETON_BONE_ROTATION;
  NUI_SKELETON_BONE_ROTATION = TNUI_SKELETON_BONE_ROTATION;

  TNUI_SKELETON_BONE_ORIENTATION = record
    endJoint : NUI_SKELETON_POSITION_INDEX;
    startJoint : NUI_SKELETON_POSITION_INDEX;
    hierarchicalRotation : NUI_SKELETON_BONE_ROTATION;
    absoluteRotation : NUI_SKELETON_BONE_ROTATION;
  end;
  PNUI_SKELETON_BONE_ORIENTATION = ^NUI_SKELETON_BONE_ORIENTATION;
  NUI_SKELETON_BONE_ORIENTATION = TNUI_SKELETON_BONE_ORIENTATION;

  TNUI_LOCKED_RECT = record
    Pitch : integer;
    size : integer;
    pBits : pointer;
  end;
  PNUI_LOCKED_RECT = ^NUI_LOCKED_RECT;
  NUI_LOCKED_RECT = TNUI_LOCKED_RECT;

const
  MICARRAY_ADAPTIVE_BEAM = $1100;

type
  INuiAudioBeam = interface(IUnknown)
  ['{8c3cebfa-a35d-497e-bc9a-e9752a8155e0}']
    function GetBeam(out angle : double) : HRESULT; stdcall;
    function SetBeam(angle : double) : HRESULT; stdcall;

    function GetPosition(out angle, confidence : double) : HRESULT; stdcall;
  end;

  INuiFrameTexture = interface(IUnknown)
  ['{13ea17f5-ff2e-4670-9ee5-1297a6e880d1}']
        function BufferLen : integer; stdcall;
        function Pitch : integer; stdcall;

        function LockRect(Level : UINT; pLockedRect : PNUI_LOCKED_RECT;
            pRect : PRECT; Flags : DWORD ) : HRESULT; stdcall;

        function GetLevelDesc(Level : UINT; out desc : NUI_SURFACE_DESC) : HRESULT; stdcall;

        function UnlockRect(Level: UINT) : HRESULT; stdcall;

  end;

  TNUI_IMAGE_FRAME = record
    liTimeStamp : int64;
    dwFrameNumber : DWORD;
    eImageType : NUI_IMAGE_TYPE;
    eResolution : NUI_IMAGE_RESOLUTION;
    pFrameTexture : INuiFrameTexture;
    dwFrameFlags : DWORD;
    ViewArea : NUI_IMAGE_VIEW_AREA;
  end;
  PNUI_IMAGE_FRAME = ^NUI_IMAGE_FRAME;
  NUI_IMAGE_FRAME = TNUI_IMAGE_FRAME;

  INuiSensor = interface(IUnknown)
  ['{1f5e088c-a8c7-41d3-9957-209677a13e85}']
    function NuiInitialize(dwFlags : DWORD) : HRESULT; stdcall;

    procedure NuiShutdown; stdcall;

    function NuiSetFrameEndEvent(
        hEvent : THandle;
        dwFrameEventFlag : DWORD) : HRESULT; stdcall;

    function NuiImageStreamOpen(
        (* [in] *) eImageType : NUI_IMAGE_TYPE;
        (* [in] *) eResolution : NUI_IMAGE_RESOLUTION;
        (* [in] *) dwImageFrameFlags : DWORD;
        (* [in] *) dwFrameLimit : DWORD;
        (* [in] *) hNextFrameEvent : THandle;
        (* [out] *) out phStreamHandle : THandle) : HRESULT; stdcall;

    function NuiImageStreamSetImageFrameFlags(
        (* [in] *) hStream : THandle;
        (* [in] *) dwImageFrameFlags : DWORD) : HRESULT; stdcall;

    function NuiImageStreamGetImageFrameFlags(
        (* [in] *) hStream : THandle;
        (* [retval][out] *) pdwImageFrameFlags : PDWORD) : HRESULT; stdcall;

    function NuiImageStreamGetNextFrame(
        (* [in] *) hStream : THandle;
        (* [in] *) dwMillisecondsToWait : DWORD;
        (* [retval][out] *) pImageFrame : PNUI_IMAGE_FRAME) : HRESULT; stdcall;

    function NuiImageStreamReleaseFrame(
        (* [in] *) hStream : THandle;
        (* [in] *) pImageFrame : PNUI_IMAGE_FRAME) : HRESULT; stdcall;

    function NuiImageGetColorPixelCoordinatesFromDepthPixel(
        (* [in] *) eColorResolution : NUI_IMAGE_RESOLUTION;
        (* [in] *) const pcViewArea : PNUI_IMAGE_VIEW_AREA;
        (* [in] *) lDepthX : longint;
        (* [in] *) lDepthY : longint;
        (* [in] *) usDepthValue : word;
        (* [out] *) plColorX : PLongint;
        (* [out] *) plColorY : PLongint) : HRESULT; stdcall;

    function NuiImageGetColorPixelCoordinatesFromDepthPixelAtResolution(
        (* [in] *) eColorResolution : NUI_IMAGE_RESOLUTION;
        (* [in] *) eDepthResolution : NUI_IMAGE_RESOLUTION;
        (* [in] *) const pcViewArea : PNUI_IMAGE_VIEW_AREA;
        (* [in] *) lDepthX : longint;
        (* [in] *) lDepthY : longint;
        (* [in] *) usDepthValue : word;
        (* [out] *) plColorX : PLongint;
        (* [out] *) plColorY : PLongint) : HRESULT; stdcall;

    function NuiImageGetColorPixelCoordinateFrameFromDepthPixelFrameAtResolution(
        (* [in] *) eColorResolution : NUI_IMAGE_RESOLUTION;
        (* [in] *) eDepthResolution : NUI_IMAGE_RESOLUTION;
        (* [in] *) cDepthValues : DWORD;
        (* [size_is][in] *) pDepthValues : PWORD;
        (* [in] *) cColorCoordinates : DWORD;
        (* [size_is][out][in] *) pColorCoordinates : PLongint) : HRESULT; stdcall;

    function NuiCameraElevationSetAngle(
        (* [in] *) lAngleDegrees : longint) : HRESULT; stdcall;

    function NuiCameraElevationGetAngle(
        (* [retval][out] *) plAngleDegrees : PLongint) : HRESULT; stdcall;

    function NuiSkeletonTrackingEnable(
        (* [in] *) hNextFrameEvent : THandle;
        (* [in] *) dwFlags : DWORD ) : HRESULT; stdcall;

    function NuiSkeletonTrackingDisable : HRESULT; stdcall;

    function NuiSkeletonSetTrackedSkeletons(
        (* [size_is][in] *) TrackingIDs : PDWORD) : HRESULT; stdcall;

    function NuiSkeletonGetNextFrame(
        (* [in] *) dwMillisecondsToWait : DWORD;
        (* [out][in] *)pSkeletonFrame : PNUI_SKELETON_FRAME) : HRESULT; stdcall;

    function NuiTransformSmooth(
        pSkeletonFrame : PNUI_SKELETON_FRAME;
        const pSmoothingParams : PNUI_TRANSFORM_SMOOTH_PARAMETERS) : HRESULT; stdcall;

    function NuiGetAudioSource(out ppDmo : INuiAudioBeam) : HRESULT; stdcall;

    function NuiInstanceIndex : integer; stdcall;

    function NuiDeviceConnectionId : PWideString; stdcall;

    function NuiUniqueId : PWideString; stdcall;

    function NuiAudioArrayId : PWideString; stdcall;

    function NuiStatus : HRESULT; stdcall;

    function NuiInitializationFlags : DWORD; stdcall;
  end;

//----------------------------------------------------------------------------
// Copyright (c) Microsoft Corporation. All rights reserved.
//----------------------------------------------------------------------------

  IID_INuiSensor = INuiSensor;
  IID_INuiFrameTexture = INuiFrameTexture;
  IID_INuiAudioBeam = INuiAudioBeam;


/// <summary>
/// Returns the number of Kinect sensors that are connected to the computer.
/// </summary>
/// <param name="pCount">Pointer to an integer which receives the number of Kinect sensors.</param>
/// <returns>
/// <para>Returns S_OK if successful; otherwise, returns one of the following failure codes:
/// <list type="table">
///    <listheader>
///       <term>Error code</term>
///       <description>Description</description>
///    </listheader>
///    <item>
///       <term>E_POINTER</term>
///       <description>The <paramref name="pCount"/> parameter is NULL.</description>
///    </item>
/// </list>
/// </returns>
  TNuiGetSensorCount = function (out count : integer) : HRESULT; stdcall;

/// <summary>
/// Creates an instance of the sensor with the specified index so that an application can open and use it.
/// </summary>
/// <param name="index">
/// The zero-based index of the sensor to open. Valid values range from zero to one less than the
/// value returned by the NuiGetSensorCount function.
/// </param>
/// <param name="ppNuiSensor">A pointer that receives a reference to the created INuiSensor interface. This must not be NULL.</param>
/// <returns>
/// Returns S_OK if successful; otherwise, returns one of the following error codes:
/// <list type="table">
///    <listheader>
///       <term>Error code</term>
///       <description>Description</description>
///    </listheader>
///    <item>
///       <term>E_INVALIDARG</term>
///       <description>The <paramref name="index"/> parameter is negative.</description>
///    </item>
///    <item>
///       <term>E_NUI_BADINDEX</term>
///       <description>The <paramref name="index"/> parameter is out of range.</description>
///    </item>
///    <item>
///       <term>E_POINTER</term>
///       <description>The <paramref name="ppNuiSensor"/> parameter is NULL.</description>
///    </item>
/// </list>
/// </returns>
/// <remarks>
/// <para>Applications that support more than one Kinect sensor call this function to access the
/// second and subsequent devices. This function returns a pointer to an INuiSensor interface,
/// which provides functions that are identical to those in NuiApi.h</para>
///
/// <para>Repeated calls to this method with the same ID may return the same interface pointer.</para>
/// </remarks>
  TNuiCreateSensorByIndex = function (index : integer; out ppNuiSensor : INuiSensor) : HRESULT; stdcall;

/// <summary>
/// Creates an instance of the sensor with the specified ID so that an application can open and use it.
/// </summary>
/// <param name="strInstanceId">A pointer to the ID of the Kinect sensor to open. This must not be NULL.</param>
/// <param name="ppNuiSensor">A pointer that receives a reference to the created INuiSensor interface. This must not be NULL.</param>
/// <returns>
/// Returns S_OK if successful; otherwise, returns one of the following error codes:
/// <list type="table">
///    <listheader>
///       <term>Error code</term>
///       <description>Description</description>
///    </listheader>
///    <item>
///       <term>E_INVALIDARG</term>
///       <description>The <paramref name="strInstanceId"/> parameter is NULL.</description>
///    </item>
///    <item>
///       <term>E_NUI_BADINDEX</term>
///       <description>The <paramref name="strInstanceId"/> parameter does not match any attached device.</description>
///    </item>
///    <item>
///       <term>E_POINTER</term>
///       <description>The <paramref name="ppNuiSensor"/> parameter is NULL.</description>
///    </item>
/// </list>
/// </returns>
/// <remarks>
/// <para>Applications that support more than one Kinect sensor call this function to access the
/// second and subsequent devices. This function returns a pointer to an INuiSensor interface,
/// which provides functions that are identical to those in NuiApi.h</para>
///
/// <para>Repeated calls to this method with the same ID may return the same interface pointer.</para>
/// </remarks>
  TNuiCreateSensorById = function (const strInstanceId : PWideString; out ppNuiSensor : INuiSensor) : HRESULT; stdcall;

  TNuiGetAudioSource = function (out ppDmo : INuiAudioBeam) : HRESULT; stdcall;

  TNuiStatusProc = procedure (hrStatus : HRESULT; const instanceName : PWideString;
    const uniqueDeviceName : PWideString; pUserData : pointer); stdcall;

/// <summary>
/// Sets a callback function that gets notified when the sensor connection status changes.
/// </summary>
/// <param name="callback">Function pointer to the callback.</param>
/// <param name="pUserData">Pointer to optional context data that will be passed to the callback.</param>
/// <remarks>
/// Use this method to handle the case of a user connecting or disconnecting a sensor.
/// </remarks>
  TNuiSetDeviceStatusCallback = procedure (callback : TNuiStatusProc; pUserData : pointer); stdcall;

/// <summary>
/// Determines whether the sensor instance has a skeletal engine.
/// </summary>
/// <param name="pNuiSensor">Pointer to an instance of the sensor.</param>
/// <returns>
/// Returns true if the sensor instance has a skeletal engine; otherwise, returns false.
/// </returns>
function HasSkeletalEngine(pNuiSensor : INuiSensor) : boolean;

const
  MAX_DEV_STR_LEN = 512;

type
  TNUI_MICROPHONE_ARRAY_DEVICE = record
    szDeviceName : array[0..MAX_DEV_STR_LEN - 1] of WideChar;
    szDeviceID : array[0..MAX_DEV_STR_LEN - 1] of WideChar;
    iDeviceIndex : integer;
  end;
  PNUI_MICROPHONE_ARRAY_DEVICE = ^NUI_MICROPHONE_ARRAY_DEVICE;
  NUI_MICROPHONE_ARRAY_DEVICE = TNUI_MICROPHONE_ARRAY_DEVICE;

/// <summary>
/// Gets device information for the connected Kinect sensors.
/// </summary>
/// <param name="pDeviceInfo">
/// Pointer to an array of NUI_MICROPHONE_ARRAY_DEVICE structures, allocated by the caller, each of
/// which receives the device information for a single connected Kinect sensor. If you set this
/// parameter to NULL, the <paramref name="piDeviceCount"/> parameter will still receive the number
/// of connected Kinect sensors.
/// </param>
/// <param name="size">
/// Size of the array pointed to by the <paramref name="pDeviceInfo"/> parameter.
/// </param>
/// <param name="piDeviceCount">
/// Receives the number of connected Kinect sensors. When this function returns, this parameter is
/// set to the number of structures in the array pointed to by the <paramref name="pDeviceInfo"/>
/// parameter that contain valid information.
/// </param>
/// <returns>
/// Returns S_OK if successful; otherwise returns a failure code.
/// </returns>
  TNuiGetMicrophoneArrayDevices = function (pDeviceInfo : PNUI_MICROPHONE_ARRAY_DEVICE;
    size : integer; out piDeviceCount : integer) : HRESULT; stdcall;

  TNUI_SPEAKER_DEVICE = record
    szDeviceName : array[0..MAX_DEV_STR_LEN - 1] of widechar;
    iDeviceIndex : integer;
    fDefault : boolean;
  end;
  PNUI_SPEAKER_DEVICE = ^NUI_SPEAKER_DEVICE;
  NUI_SPEAKER_DEVICE = TNUI_SPEAKER_DEVICE;

/// <summary>
/// Gets the active speaker devices found on the system.
/// </summary>
/// <param name="pDeviceInfo">
/// Pointer to an array of NUI_SPEAKER_DEVICE structures, allocated by the caller, each of which
/// receives the device information for a single connected speaker device. If you set this
/// parameter to NULL, the <paramref name="piDeviceCount"/> parameter will still receive the number
/// of connected speaker devices.
/// </param>
/// <param name="size">
/// Size of the array pointed to by the <paramref name="pDeviceInfo"/> parameter.
/// </param>
/// <param name="piDeviceCount">
/// Receives the number of connected speaker devices. When this function returns, this parameter is
/// set to the number of structures in the array pointed to by the <paramref name="pDeviceInfo"/>
/// parameter that contain valid information.
/// </param>
/// <returns>
/// Returns S_OK if successful; otherwise returns a failure code.
/// </returns>
  TNuiGetSpeakerDevices = function (pDeviceInfo : PNUI_SPEAKER_DEVICE;
    size : integer; out piDeviceCount : integer) : HRESULT; stdcall;

implementation

uses NuiApi;

function HasSkeletalEngine(pNuiSensor : INuiSensor) : boolean;
begin
  result := false;
  if not assigned(pNuiSensor) then exit;

  result := ((pNuiSensor.NuiInitializationFlags() and NUI_INITIALIZE_FLAG_USES_SKELETON) <> 0) or
            ((pNuiSensor.NuiInitializationFlags() and NUI_INITIALIZE_FLAG_USES_DEPTH_AND_PLAYER_INDEX) <> 0);
end;

end.
