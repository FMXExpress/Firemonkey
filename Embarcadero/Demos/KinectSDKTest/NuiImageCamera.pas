(************************************************************************
*                                                                       *
*   NuiImageCamera.h -- This module defines the APIs for the Natural    *
*                       User Interface(NUI) image and camera services.  *
*                                                                       *
*   Copyright (c) Microsoft Corp. All rights reserved.                  *
*                                                                       *
************************************************************************)
{$Z4}
unit NuiImageCamera;

interface

uses Windows, NuiSensor;

/// <summary>
/// Converts the resolution to a size.
/// </summary>
/// <param name="res">The image resolution.</param>
/// <param name="refWidth">The width of the image.</param>
/// <param name="refHeight">The height of the image.</param>
procedure NuiImageResolutionToSize(res : NUI_IMAGE_RESOLUTION;
  out refWidth, refHeight : DWORD);

const
  NUI_IMAGE_PLAYER_INDEX_SHIFT          = 3;
  NUI_IMAGE_PLAYER_INDEX_MASK           = ((1 shl NUI_IMAGE_PLAYER_INDEX_SHIFT)-1);
  NUI_IMAGE_DEPTH_MAXIMUM               = ((4000 shl NUI_IMAGE_PLAYER_INDEX_SHIFT) or NUI_IMAGE_PLAYER_INDEX_MASK);
  NUI_IMAGE_DEPTH_MINIMUM               = (800 shl NUI_IMAGE_PLAYER_INDEX_SHIFT);
  NUI_IMAGE_DEPTH_MAXIMUM_NEAR_MODE     = ((3000 shl NUI_IMAGE_PLAYER_INDEX_SHIFT) or NUI_IMAGE_PLAYER_INDEX_MASK);
  NUI_IMAGE_DEPTH_MINIMUM_NEAR_MODE     = (400 shl NUI_IMAGE_PLAYER_INDEX_SHIFT);
  NUI_IMAGE_DEPTH_NO_VALUE              = 0;
  NUI_IMAGE_DEPTH_TOO_FAR_VALUE         = ($0fff shl NUI_IMAGE_PLAYER_INDEX_SHIFT);
  NUI_DEPTH_DEPTH_UNKNOWN_VALUE         = ($1fff shl NUI_IMAGE_PLAYER_INDEX_SHIFT);

  NUI_CAMERA_DEPTH_NOMINAL_FOCAL_LENGTH_IN_PIXELS         = (285.63);   // Based on 320x240 pixel size.
  NUI_CAMERA_DEPTH_NOMINAL_INVERSE_FOCAL_LENGTH_IN_PIXELS = (3.501e-3); // (1/NUI_CAMERA_DEPTH_NOMINAL_FOCAL_LENGTH_IN_PIXELS)
  NUI_CAMERA_DEPTH_NOMINAL_DIAGONAL_FOV                   = (70.0);
  NUI_CAMERA_DEPTH_NOMINAL_HORIZONTAL_FOV                 = (58.5);
  NUI_CAMERA_DEPTH_NOMINAL_VERTICAL_FOV                   = (45.6);

  NUI_CAMERA_COLOR_NOMINAL_FOCAL_LENGTH_IN_PIXELS         = (531.15);   // Based on 640x480 pixel size.
  NUI_CAMERA_COLOR_NOMINAL_INVERSE_FOCAL_LENGTH_IN_PIXELS = (1.83e-3);  // (1/NUI_CAMERA_COLOR_NOMINAL_FOCAL_LENGTH_IN_PIXELS)
  NUI_CAMERA_COLOR_NOMINAL_DIAGONAL_FOV                   = ( 73.9);
  NUI_CAMERA_COLOR_NOMINAL_HORIZONTAL_FOV                 = ( 62.0);
  NUI_CAMERA_COLOR_NOMINAL_VERTICAL_FOV                   = ( 48.6);


//
// Unpacks the depth value from the packed pixel format
//
function NuiDepthPixelToDepth(packedPixel: WORD) : WORD;

//
// Unpacks the player index value from the packed pixel format
//
function NuiDepthPixelToPlayerIndex(packedPixel : WORD) : WORD;

type
  TNUI_IMAGE_DIGITALZOOM = (
    NUI_IMAGE_DIGITAL_ZOOM_1X = 0
  );
  NUI_IMAGE_DIGITALZOOM = TNUI_IMAGE_DIGITALZOOM;

  TNUI_IMAGE_VIEW_AREA = record
    eDigitalZoom : NUI_IMAGE_DIGITALZOOM;
    lCenterX : Longint;
    lCenterY : Longint;
  end;
  PNUI_IMAGE_VIEW_AREA = ^NUI_IMAGE_VIEW_AREA;
  NUI_IMAGE_VIEW_AREA = TNUI_IMAGE_VIEW_AREA;

const
  NUI_IMAGE_FRAME_FLAG_NONE              = $00000000;
  NUI_IMAGE_FRAME_FLAG_VIEW_AREA_UNKNOWN = $00000001;

type

// Note:  NOT identical to D3DSURFACE_DESC
  TNUI_SURFACE_DESC = record
    Width : UINT;
    Height : UINT;
  end;
  NUI_SURFACE_DESC = TNUI_SURFACE_DESC;

const
// return S_FALSE instead of E_NUI_FRAME_NO_DATA if NuiImageStreamGetNextFrame( ) doesn't have a frame ready and a timeout != INFINITE is used
  NUI_IMAGE_STREAM_FLAG_SUPPRESS_NO_FRAME_DATA              = $00010000;
// Set the depth stream to near mode
  NUI_IMAGE_STREAM_FLAG_ENABLE_NEAR_MODE                    = $00020000;
// Use distinct values for depth values that are either too close, too far or unknown
  NUI_IMAGE_STREAM_FLAG_DISTINCT_OVERFLOW_DEPTH_VALUES      = $00040000;

// the max # of NUI output frames you can hold w/o releasing
  NUI_IMAGE_STREAM_FRAME_LIMIT_MAXIMUM = 4;

/// <summary>
/// Sets the image frame flags for the specified stream.
/// </summary>
/// <param name="hStream">A handle to the stream.</param>
/// <param name="dwImageFrameFlags">The image frame flags, as a bitwise-OR combination of the NUI_IMAGE_STREAM_FLAG constants.</param>
/// <returns>
/// Returns S_OK if successful; otherwise, returns one of the following failure codes:
/// <list type="table">
///    <listheader>
///       <term>Error code</term>
///       <description>Description</description>
///    </listheader>
///    <item>
///       <term>E_INVALIDARG</term>
///       <description>The <paramref name="hStream"/> parameter is NULL.</description>
///    </item>
///    <item>
///       <term>E_NUI_DEVICE_NOT_READY</term>
///       <description>The device is uninitialized. <see cref="NuiInitialize"/>.</description>
///    </item>
/// </list>
/// </returns>
/// <remarks>
/// The maximum number of output frames you can set is defined by NUI_IMAGE_STREAM_FRAME_LIMIT_MAXIMUM.
/// </remarks>

type
  TNuiImageStreamSetImageFrameFlags = function (
    hStream : THANDLE;
    dwImageFrameFlags : DWORD
  ) : HRESULT; stdcall;

/// <summary>
/// Gets the image frame flags for the specified stream.
/// </summary>
/// <param name="hStream">A handle to the stream.</param>
/// <param name="pdwImageFrameFlags">A pointer to a DWORD that receives the image frame flags.</param>
/// <returns>
/// Returns S_OK if successful; otherwise, returns one of the following failure codes:
/// <list type="table">
///    <listheader>
///       <term>Error code</term>
///       <description>Description</description>
///    </listheader>
///    <item>
///       <term>E_INVALIDARG</term>
///       <description>The <paramref name="hStream"/> parameter is NULL.</description>
///    </item>
///    <item>
///       <term>E_NUI_DEVICE_NOT_READY</term>
///       <description>The device is uninitialized. <see cref="NuiInitialize"/>.</description>
///    </item>
///    <item>
///       <term>E_POINTER</term>
///       <description>The <paramref name="pdwImageFrameFlags"/> parameter is NULL.</description>
///    </item>
/// </list>
/// </returns>
  TNuiImageStreamGetImageFrameFlags = function (
    hStream : THANDLE;
    pdwImageFrameFlags : PDWORD
  ) : HRESULT; stdcall;

/// <summary>
/// Sets the event that signals the last frame.
/// </summary>
/// <param name="hEvent">A handle to the event.</param>
/// <param name="dwFrameEventFlag">
/// The frame event options, as a bitwise-OR combination of the NUI_IMAGE_STREAM_FLAG constants.
/// </param>
/// <returns>
/// Returns S_OK if successful; otherwise, returns one of the following error codes:
/// <list type="table">
///    <listheader>
///       <term>Error code</term>
///       <description>Description</description>
///    </listheader>
///    <item>
///       <term>E_INVALIDARG</term>
///       <description>The <paramref name="hEvent"/> parameter is an invalid handle.</description>
///    </item>
/// </list>
/// </returns>
/// <remarks>
/// <para>If the frame does not process any data, the event is still signaled unless you specify
/// NUI_FRAME_EVENT_FLAG_SUPPRESS_NO_FRAME_DATA.</para>
///
/// <para>The event provided is signaled after the NUI runtime is finished processing all data
/// associated with a frame. When you use NuiImageStreamGetNextFrame and NuiSkeletonGetNextFrame,
/// all stream data generated in that frame is available before the event is signaled.</para>
///
/// <para>This event is never reset by the NUI runtime because there is not a well-defined time to
/// do so. This is unlike the events provided to NuiImageStreamOpen and NuiSkeletonTrackingEnable.
/// Similarly, proper operation requires an auto-reset event, instead of a manual reset event.</para>
///
/// <para>When pumping multiple streams with a single thread, you should use a NuiSetFrameEndEvent
/// event over separate per-stream events.</para>
///
/// <para>The flag NUI_IMAGE_STREAM_FLAG_SUPPRESS_NO_FRAME_DATA still applies on a per-stream basis
/// when you call NuiImageStreamGetNextFrame provided you specify that flag for the stream in
/// NuiImageStreamOpen.</para>
///
/// <para>The flag NUI_SKELETON_TRACKING_FLAG_SUPPRESS_NO_FRAME_DATA still applies on a per-stream
/// basis when you call NuiSkeletonGetNextFrame provided you specify that flag for the stream in
/// NuiSkeletonTrackingEnable.</para>
///
/// <para>When processing multiple streams on multiple threads, it's more efficient to create a
/// separate event for each stream instead of using NuiSetFrameEndEvent. Using separate events
/// ensures that the app doesn't have to wait until processing for skeleton tracking is done before
/// it can get color and depth data.</para>
///
/// <para>For signaling when skeleton data is available, use an event in NuiSkeletonTrackingEnable.
/// For signaling when an image stream is available, use an event in NuiImageStreamOpen.</para>
/// </remarks>
  TNuiSetFrameEndEvent = function (
    hEvent : THANDLE;
    dwFrameEventFlag : DWORD
  ) : HRESULT; stdcall;

/// <summary>
/// Opens an image stream.
/// </summary>
/// <param name="eImageType">
/// A NUI_IMAGE_TYPE value that specifies which image stream to open. The valid values for this
/// parameter depend on the flags passed to the NuiInitialize method; for more information see
/// remarks.
/// </param>
/// <param name="eResolution">
/// A NUI_IMAGE_RESOLUTION value that specifies which resolution to use for the image stream. The
/// valid values for this parameter depend on the flags passed to the NuiInitialize method; for
/// more information, see remarks.
/// </param>
/// <param name="dwImageFrameFlags">
/// The stream options, as a bitwise-OR combination of the NUI_IMAGE_STREAM_FLAG constants.
/// </param>
/// <param name="dwFrameLimit">
/// The number of frames that the NUI runtime should buffer. The maximum value is
/// NUI_IMAGE_STREAM_FRAME_LIMIT_MAXIMUM. Most applications should use a frame limit of two.
/// </param>
/// <param name="hNextFrameEvent">
/// A handle to a manual reset event that will be fired when the next frame in the stream is
/// available.
/// </param>
/// <param name="phStreamHandle">
/// A pointer that receives a handle to the opened stream. This must not be NULL.
/// </param>
/// <returns>
/// Returns S_OK if successful; otherwise, returns one of the following failure codes:
/// <list type="table">
///    <listheader>
///       <term>Error code</term>
///       <description>Description</description>
///    </listheader>
///    <item>
///       <term>E_INVALIDARG</term>
///       <description>The <paramref name="dwFrameLimit"/> parameter is out of range.</description>
///    </item>
///    <item>
///       <term>E_NUI_DEVICE_NOT_READY</term>
///       <description>The device has not been initialized. <see cref="NuiInitialize"/>.</description>
///    </item>
///    <item>
///       <term>E_OUTOFMEMORY</term>
///       <description>The <paramref name="hEvent"/> parameter is an invalid handle.</description>
///    </item>
///    <item>
///       <term>E_POINTER</term>
///       <description>The <paramref name="hEvent"/> parameter is an invalid handle.</description>
///    </item>
///    <item>
///       <term>E_FAIL</term>
///       <description>An unspecified error occurred.</description>
///    </item>
/// </list>
/// </returns>
/// <remarks>
/// <para>The NUI runtime buffers the number of frames specified by the dwFrameLimit parameter. If
/// the application does not retrieve and release a frame before the buffer is full, the runtime
/// replaces the oldest frame in the buffer with an incoming frame. As a result, frames can
/// occasionally be dropped.</para>
///
/// <para>The valid values for the eImageType and eResolution parameters depend on the NUI
/// initialization flags passed to the NuiInitialize method in the dwFlags parameter. The following
/// tables summarize the combinations that are currently valid.</para>
///
/// <para>If <paramref name="dwFlags"/> includes NUI_INITIALIZE_FLAG_USES_DEPTH:
/// <list>
///    <listheader>
///       <term>NUI_IMAGE_TYPE value</term>
///       <description>NUI_IMAGE_RESOLUTION value</description>
///    </listheader>
///    <item>
///       <term>NUI_IMAGE_TYPE_DEPTH</term>
///       <description>NUI_IMAGE_RESOLUTION_640x480</description>
///    </item>
///    <item>
///       <term>NUI_IMAGE_TYPE_DEPTH</term>
///       <description>NUI_IMAGE_RESOLUTION_320x240</description>
///    </item>
///    <item>
///       <term>NUI_IMAGE_TYPE_DEPTH</term>
///       <description>NUI_IMAGE_RESOLUTION_80x60</description>
///    </item>
/// </list></para>
///
/// <para>If <paramref name="dwFlags"/> includes NUI_INITIALIZE_FLAG_USES_DEPTH_AND_PLAYER_INDEX:
/// <list>
///    <listheader>
///       <term>NUI_IMAGE_TYPE value</term>
///       <description>NUI_IMAGE_RESOLUTION value</description>
///    </listheader>
///    <item>
///       <term>NUI_IMAGE_TYPE_DEPTH_AND_PLAYER_INDEX</term>
///       <description>NUI_IMAGE_RESOLUTION_320x240</description>
///    </item>
///    <item>
///       <term>NUI_IMAGE_TYPE_DEPTH_AND_PLAYER_INDEX</term>
///       <description>NUI_IMAGE_RESOLUTION_80x60</description>
///    </item>
/// </list></para>
///
/// <para>If <paramref name="dwFlags"/> includes NUI_INITIALIZE_FLAG_USES_COLOR:
/// <list>
///    <listheader>
///       <term>NUI_IMAGE_TYPE value</term>
///       <description>NUI_IMAGE_RESOLUTION value</description>
///    </listheader>
///    <item>
///       <term>NUI_IMAGE_TYPE_COLOR</term>
///       <description>NUI_IMAGE_RESOLUTION_1280x960</description>
///    </item>
///    <item>
///       <term>NUI_IMAGE_TYPE_COLOR</term>
///       <description>NUI_IMAGE_RESOLUTION_640x480</description>
///    </item>
///    <item>
///       <term>NUI_IMAGE_TYPE_COLOR_YUV</term>
///       <description>NUI_IMAGE_RESOLUTION_640x480</description>
///    </item>
///    <item>
///       <term>NUI_IMAGE_TYPE_COLOR_RAW_YUV</term>
///       <description>NUI_IMAGE_RESOLUTION_640x480</description>
///    </item>
/// </list></para>
/// </remarks>
  TNuiImageStreamOpen = function (
    eImageType : NUI_IMAGE_TYPE;
    eResolution : NUI_IMAGE_RESOLUTION;
    dwImageFrameFlags : DWORD;
    dwFrameLimit : DWORD;
    hNextFrameEvent : THANDLE;
    out phStreamHandle : THANDLE
  ) : HRESULT; stdcall;


/// <summary>
/// Gets the next frame of data from the specified image stream.
/// </summary>
/// <param name="hStream">
/// A handle to the image stream. This stream must have been opened by a call to the
/// NuiImageStreamOpen method.
/// </param>
/// <param name="dwMillisecondsToWait">
/// The timeout (in milliseconds) before returning without a new frame.
/// </param>
/// <param name="pImageFrame">
/// A pointer to a NUI_IMAGE_FRAME structure that receives the next image frame in the specified
/// stream. The pFrameTexture member of the structure points to an INuiFrameTexture instance that
/// contains the frame data.
/// </param>
/// <returns>
/// Returns S_OK if successful; otherwise, returns one of the following failure codes: , .</para>
/// <list type="table">
///    <listheader>
///       <term>Error code</term>
///       <description>Description</description>
///    </listheader>
///    <item>
///       <term>S_FALSE</term>
///       <description>The wait timeout expired before a new frame was available and <paramref name="hStream"/> was opened with the NUI_IMAGE_STREAM_FLAG_SUPPRESS_NO_FRAME_DATA flag.</description>
///    </item>
///    <item>
///       <term>E_INVALIDARG</term>
///       <description>The <paramref name="hStream"/> parameter is NULL.</description>
///    </item>
///    <item>
///       <term>E_NUI_FRAME_NO_DATA</term>
///       <description>The wait timeout expired before a new frame was available.</description>
///    </item>
///    <item>
///       <term>E_POINTER</term>
///       <description>The <paramref name="pImageFrame"/> parameter is NULL.</description>
///    </item>
/// </list>
/// </returns>

  TNuiImageStreamGetNextFrame = function (
    hStream : THANDLE;
    dwMillisecondsToWait : DWORD;
    out ppcImageFrame : PNUI_IMAGE_FRAME
  ) : HRESULT; stdcall;

/// <summary>
/// Releases the specified frame of data from the specified stream.
/// </summary>
/// <param name="hStream">
/// A handle to the image stream. This stream must have been opened by a call to the
/// NuiImageStreamOpen method.
/// </param>
/// <param name="pImageFrame">A pointer to the frame to release.</param>
/// <returns>
/// Returns S_OK if successful; otherwise, returns one of the following failure codes:
/// <list type="table">
///    <listheader>
///       <term>Error code</term>
///       <description>Description</description>
///    </listheader>
///    <item>
///       <term>E_INVALIDARG</term>
///       <description>The <paramref name="hStream"/> or <paramref name="pImageFrame"/> parameter is NULL.</description>
///    </item>
///    <item>
///       <term>E_NOINTERFACE</term>
///       <description>The <paramref name="pImageFrame"/> parameter's <c>pFrameTexture</c> member is NULL.</description>
///    </item>
///    <item>
///       <term>E_NUI_DEVICE_NOT_READY</term>
///       <description>The device is uninitialized. <see cref="NuiInitialize"/>.</description>
///    </item>
/// </list>
/// </returns>
/// <remarks>
/// Before you call this function, reset the notification event for the stream.
/// </remarks>
  TNuiImageStreamReleaseFrame = function (
    hStream : THANDLE;
    CONST pImageFrame : PNUI_IMAGE_FRAME
  ) : HRESULT; stdcall;


/// <summary>
/// Gets the pixel coordinates in color space that correspond to the specified pixel coordinates in
/// depth space.
/// </summary>
/// <param name="eColorResolution">
/// The resolution of the color image, as a NUI_IMAGE_RESOLUTION enumeration constant.
/// </param>
/// <param name="pcViewArea">
/// The optional zoom and pan settings of the color image, as a pointer to a NUI_IMAGE_VIEW_AREA
/// structure. To ensure that the settings are valid, use the ViewArea member of the NUI_IMAGE_FRAME
/// that you are registering pixels against. Do not instantiate and populate this structure manually.
/// </param>
/// <param name="lDepthX">The X coordinate in depth image space.</param>
/// <param name="lDepthY">The Y coordinate in depth image space.</param>
/// <param name="usDepthValue">
/// The depth value in depth image space. This value is constrained between NUI_IMAGE_DEPTH_MINIMUM
/// and NUI_IMAGE_DEPTH_MAXIMUM.
/// </param>
/// <param name="plColorX">
/// Pointer to a LONG value that receives the X coordinate of the pixel in color image space. This
/// pointer must be non-NULL when you call this function. If this method does not return S_OK, this
/// data is invalid. This value can be outside of the bounds of the color image.
/// </param>
/// <param name="plColorY">
/// Pointer to a LONG value that receives the Y coordinate of the pixel in color image space. This
/// pointer must be non-NULL when you call this function. If this method does not return S_OK, this
/// data is invalid. This value can be outside of the bounds of the color image.
/// </param>
/// <returns>
/// Returns S_OK if successful; otherwise, returns one of the following failure codes:
/// <list type="table">
///    <listheader>
///       <term>Error code</term>
///       <description>Description</description>
///    </listheader>
///    <item>
///       <term>E_INVALIDARG</term>
///       <description>The <paramref name="eColorResolution"/> parameter does not specify a valid resolution, or pcViewArea is provided but does not describe the full frame.</description>
///    </item>
///    <item>
///       <term>E_NUI_DEVICE_NOT_READY</term>
///       <description>The device is uninitialized. <see cref="NuiInitialize"/>.</description>
///    </item>
///    <item>
///       <term>E_POINTER</term>
///       <description>The <paramref name="plColorX"/> or <paramref name="plColorY"/> parameter is NULL.</description>
///    </item>
/// </list>
/// </returns>
/// <remarks>
/// <para>Because depth image data and color image data come from separate sensors, pixels in the
/// two images may not always line up exactly. The two sensors may have different fields of view,
/// or may not be aimed precisely in the same direction. This means that a point near the edge of
/// the depth image may correspond to a pixel just beyond the edge of the color image, or vice
/// versa.</para>
///
/// <para>This function accepts coordinates outside the bounds of the depth image. It may return
/// pixels outside the color image. This means that you can use data from the two images in
/// combination, even when the two images do not line up completely. You must verify that the
/// coordinates that are returned lie within the color image before using the coordinates to
/// reference pixels in that color image.</para>
///
/// <para>The depth image coordinates you specify are not required to be within the bounds of the
/// depth frame image, but they should not be too far outside the depth frame image bounds. If the
/// coordinates are far outside the depth frame image, they are unlikely to map to coordinates
/// inside the bounds of the color image. This function will then return color image coordinates
/// that are unlikely to be useful.</para>
/// </remarks>
/// <seealso cref="NuiImageGetColorPixelCoordinatesFromDepthPixelAtResolution"/>
  TNuiImageGetColorPixelCoordinatesFromDepthPixel = function (
    eColorResolution : NUI_IMAGE_RESOLUTION;
    CONST pcViewArea : PNUI_IMAGE_VIEW_AREA;
    lDepthX : LONGINT;
    lDepthY : LONGINT;
    usDepthValue : WORD;
    out plColorX : LONGINT;
    out plColorY : LONGINT
  ) : HRESULT; stdcall;

/// <summary>
/// Gets the pixel coordinates in color space that correspond to the specified pixel coordinates in
/// depth space, using the specified depth resolution.
/// </summary>
/// <param name="eColorResolution">
/// The resolution of the color image, as a NUI_IMAGE_RESOLUTION enumeration constant.
/// </param>
/// <param name="eDepthResolution">
/// The resolution of the depth image, as a NUI_IMAGE_RESOLUTION enumeration constant.
/// </param>
/// <param name="pcViewArea">
/// The optional zoom and pan settings of the color image, as a pointer to a NUI_IMAGE_VIEW_AREA
/// structure. To ensure that the settings are valid, use the ViewArea member of the NUI_IMAGE_FRAME
/// that you are registering pixels against. Do not instantiate and populate this structure manually.
/// </param>
/// <param name="lDepthX">The X coordinate in depth image space.</param>
/// <param name="lDepthY">The Y coordinate in depth image space.</param>
/// <param name="usDepthValue">
/// The depth value in depth image space. This value is constrained between NUI_IMAGE_DEPTH_MINIMUM
/// and NUI_IMAGE_DEPTH_MAXIMUM.
/// </param>
/// <param name="plColorX">
/// Pointer to a LONG value that receives the X coordinate of the pixel in color image space. This
/// pointer must be non-NULL when you call this function. If this method does not return S_OK, this
/// data is invalid. This value can be outside of the bounds of the color image.
/// </param>
/// <param name="plColorY">
/// Pointer to a LONG value that receives the Y coordinate of the pixel in color image space. This
/// pointer must be non-NULL when you call this function. If this method does not return S_OK, this
/// data is invalid. This value can be outside of the bounds of the color image.
/// </param>
/// <returns>
/// Returns S_OK if successful; otherwise, returns one of the following failure codes:
/// <list type="table">
///    <listheader>
///       <term>Error code</term>
///       <description>Description</description>
///    </listheader>
///    <item>
///       <term>E_INVALIDARG</term>
///       <description>The <paramref name="eColorResolution"/> or <paramref name="eDepthResolution"/> parameter does not specify a valid resolution, or pcViewArea is provided but does not describe the full frame.</description>
///    </item>
///    <item>
///       <term>E_NUI_DEVICE_NOT_READY</term>
///       <description>The device is uninitialized. <see cref="NuiInitialize"/>.</description>
///    </item>
///    <item>
///       <term>E_POINTER</term>
///       <description>The <paramref name="plColorX"/> or <paramref name="plColorY"/> parameter is NULL.</description>
///    </item>
/// </list>
/// </returns>
/// <remarks>
/// <para>Because depth image data and color image data come from separate sensors, pixels in the
/// two images may not always line up exactly. The two sensors may have different fields of view,
/// or may not be aimed precisely in the same direction. This means that a point near the edge of
/// the depth image may correspond to a pixel just beyond the edge of the color image, or vice
/// versa.</para>
///
/// <para>This function accepts coordinates outside the bounds of the depth image. It may return
/// pixels outside the color image. This means that you can use data from the two images in
/// combination, even when the two images do not line up completely. You must verify that the
/// coordinates that are returned lie within the color image before using the coordinates to
/// reference pixels in that color image.</para>
///
/// <para>The depth image coordinates you specify are not required to be within the bounds of the
/// depth frame image, but they should not be too far outside the depth frame image bounds. If the
/// coordinates are far outside the depth frame image, they are unlikely to map to coordinates
/// inside the bounds of the color image. This function will then return color image coordinates
/// that are unlikely to be useful.</para>
/// </remarks>
/// <seealso cref="NuiImageGetColorPixelCoordinatesFromDepthPixel"/>
  TNuiImageGetColorPixelCoordinatesFromDepthPixelAtResolution = function (
    eColorResolution : NUI_IMAGE_RESOLUTION;
    eDepthResolution : NUI_IMAGE_RESOLUTION;
    CONST pcViewArea : PNUI_IMAGE_VIEW_AREA;
    lDepthX : LONGINT;
    lDepthY : LONGINT;
    usDepthValue : WORD;
    out plColorX : LONGINT;
    out plColorY : LONGINT
  ) : HRESULT; stdcall;

const
  NUI_CAMERA_ELEVATION_MAXIMUM  = 27;
  NUI_CAMERA_ELEVATION_MINIMUM  = (-27);

/// <summary>
/// Gets the elevation angle of the Kinect sensor.
/// </summary>
/// <param name="plAngleDegrees">Pointer to a LONG which receives the angle of the sensor in degrees.</param>
/// <returns>
/// Returs S_OK if successful; otherwise, returns one of the following failure codes:
/// <list type="table">
///    <listheader>
///       <term>Error code</term>
///       <description>Description</description>
///    </listheader>
///    <item>
///       <term>E_NUI_DEVICE_NOT_READY</term>
///       <description>The device is uninitialized. <see cref="NuiInitialize"/>.</description>
///    </item>
///    <item>
///       <term>E_POINTER</term>
///       <description>The <paramref name="plAngleDegrees"/> parameter is NULL.</description>
///    </item>
/// </list>
/// </returns>
/// <remarks>
/// <para>An elevation angle of zero indicates that the Kinect sensor is pointing perpendicular to
/// gravity. The tilt is relative to gravity rather than relative to the sensor base. The angle is
/// subject to the physical limitations of the sensor. If the sensor base is resting on a tilted
/// surface, the middle of the sensor's tilt range will not correspond to a tilt angle of zero, and
/// the sensor may not be physically capable of reaching the outer limits of the range allowed by
/// the API. If the sensor is moved so that the base is at a different angle relative to gravity,
/// or if the sensor is tilted manually, the angle reported by the API will change, even if the
/// tilt angle has not been changed programmatically.</para>
/// </remarks>
/// <seealso cref="NuiCameraElevationSetAngle"/>
type
  TNuiCameraElevationGetAngle = function (
    out plAngleDegrees : LONGINT
  ) : HRESULT; stdcall;

/// <summary>
/// Sets the elevation angle of the Kinect sensor.
/// </summary>
/// <param name="lAngleDegrees">
/// The elevation angle relative to gravity, in degrees. A value of zero indicates that the sensor
/// array should point exactly horizontally. Positive values indicate that the sensor array should
/// point above the horizon, and negative values indicate that the sensor array should point below
/// the horizon. This value is constrained between NUI_CAMERA_ELEVATION_MINIMUM and
/// NUI_CAMERA_ELEVATION_MAXIMUM.
/// </param>
/// <returns>
/// Returns S_OK if successful; otherwise, returns one of the following failure codes:
/// <list type="table">
///    <listheader>
///       <term>Error code</term>
///       <description>Description</description>
///    </listheader>
///    <item>
///       <term>__HRESULT_FROM_WIN32(ERROR_TOO_MANY_CMDS)</term>
///       <description>There were too many calls to <c>NuiCameraElevationSetAngle</c> within a given timespan.</description>
///    </item>
///    <item>
///       <term>__HRESULT_FROM_WIN32(ERROR_RETRY)</term>
///       <description>There was too little time between subsequent <c>NuiCameraElevationSetAngle</c> calls.</description>
///    </item>
///    <item>
///       <term>E_NUI_DEVICE_NOT_READY</term>
///       <description>The device is uninitialized. <see cref="NuiInitialize"/>.</description>
///    </item>
/// </list>
/// </returns>
/// <remarks>
/// <para>You should tilt the Kinect sensor as few times as possible, to minimize wear on the
/// sensor and to minimize tilting time. The tilt motor is not designed for constant or repetitive
/// movement, and attempts to use it that way can cause degradation of motor function. To reduce
/// wear on the Kinect sensor's tilt motor, your application can change the elevation angle no more
/// than once per second. In addition, you must allow at least 20 seconds of rest after 15
/// consecutive changes. If your application exceeds these limits, additional attempts to set the
/// elevation angle during the lockout period will result in an error code.</para>
/// 
/// <para>An elevation angle of zero indicates that the Kinect sensor is pointing perpendicular to
/// gravity. The tilt is relative to gravity rather than relative to the sensor base. The angle is
/// subject to the physical limitations of the sensor. If the sensor base is resting on a tilted
/// surface, the middle of the sensor's tilt range will not correspond to a tilt angle of zero, and
/// the sensor may not be physically capable of reaching the outer limits of the range allowed by
/// the API. If the sensor is moved so that the base is at a different angle relative to gravity,
/// or if the sensor is tilted manually, the angle reported by the API will change, even if the
/// tilt angle has not been changed programmatically.</para>
/// </remarks>
/// <seealso cref="NuiCameraElevationGetAngle"/>
  TNuiCameraElevationSetAngle = function (
    lAngleDegrees : LONGINT
  ) : HRESULT; stdcall;

implementation

procedure NuiImageResolutionToSize(res : NUI_IMAGE_RESOLUTION; out refWidth, refHeight : DWORD);
begin
  case res of
    NUI_IMAGE_RESOLUTION_80x60: begin
      refWidth := 80;
      refHeight := 60;
    end;
    NUI_IMAGE_RESOLUTION_320x240: begin
      refWidth := 320;
      refHeight := 240;
    end;
    NUI_IMAGE_RESOLUTION_640x480: begin
      refWidth := 640;
      refHeight := 480;
    end;
    NUI_IMAGE_RESOLUTION_1280x960: begin
      refWidth := 1280;
      refHeight := 960;
    end;
  else
    refWidth := 0;
    refHeight := 0;
  end;
end;

function NuiDepthPixelToDepth(packedPixel: WORD) : WORD;
begin
    result := packedPixel shr NUI_IMAGE_PLAYER_INDEX_SHIFT;
end;

function NuiDepthPixelToPlayerIndex(packedPixel : WORD) : WORD;
begin
    result := packedPixel and NUI_IMAGE_PLAYER_INDEX_MASK;
end;

end.
