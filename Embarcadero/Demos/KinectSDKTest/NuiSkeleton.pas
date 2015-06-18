(************************************************************************
*                                                                       *
*   NuiSkeleton.h -- This module defines the APIs for the Natural       *
*                    User Interface(NUI) skeleton services.             *
*                                                                       *
*   Copyright (c) Microsoft Corp. All rights reserved.                  *
*                                                                       *
************************************************************************)
{$Z4}
unit NuiSkeleton;

interface

uses Windows, NuiSensor, NuiImageCamera;

const
  FLT_EPSILON    = 1.192092896e-07;       (* smallest such that 1.0+FLT_EPSILON != 1.0 *)

  NUI_SKELETON_MAX_TRACKED_COUNT = 2;

//
//  Tracking IDs start at 1
//
  NUI_SKELETON_INVALID_TRACKING_ID = 0;

  NUI_SKELETON_QUALITY_CLIPPED_RIGHT  = $00000001;
  NUI_SKELETON_QUALITY_CLIPPED_LEFT   = $00000002;
  NUI_SKELETON_QUALITY_CLIPPED_TOP    = $00000004;
  NUI_SKELETON_QUALITY_CLIPPED_BOTTOM = $00000008;

// Flags returned by NuiSkeletonGetNextFrame in the dwFlags field of the NUI_SKELETON_FRAME structure
  NUI_SKELETON_FRAME_FLAG_SEATED_SUPPORT_ENABLED  = $00000008;

// Flags for use with the NuiSkeletonTrackingEnable function
  NUI_SKELETON_TRACKING_FLAG_SUPPRESS_NO_FRAME_DATA       = $00000001;
  NUI_SKELETON_TRACKING_FLAG_TITLE_SETS_TRACKED_SKELETONS = $00000002;
  NUI_SKELETON_TRACKING_FLAG_ENABLE_SEATED_SUPPORT        = $00000004;
  NUI_SKELETON_TRACKING_FLAG_ENABLE_IN_NEAR_RANGE         = $00000008;

type
/// <summary>
/// Enables skeleton tracking.
/// </summary>
/// <param name="hNextFrameEvent">
/// A handle to an application-allocated, manual reset event that will be set whenever a new frame
/// of skeleton data is available, and will be reset whenever the latest frame data is returned.
/// This can be NULL.
/// </param>
/// <param name="dwFlags">
/// Flags that control skeleton tracking, as a bitwise-OR combination NUI_SKELETON_TRACKING values.
/// </param>
/// <returns>
/// Returns S_OK if successful; otherwise, returns one of the following failure codes:
/// <list type="table">
///    <listheader>
///       <term>Error code</term>
///       <description>Description</description>
///    </listheader>
///    <item>
///       <term>__HRESULT_FROM_WIN32(ERROR_INVALID_OPERATION)</term>
///       <description>The device is uninitialized or the sensor is not initialized with the NUI_INITIALIZE_FLAG_USES_SKELETON flag. <see cref="NuiInitialize"/>.</description>
///    </item>
///    <item>
///       <term>E_INVALIDARG</term>
///       <description>The <paramref name="dwFlags"/> parameter includes invalid flags.</description>
///    </item>
///    <item>
///       <term>E_OUTOFMEMORY</term>
///       <description>Allocation failed.</description>
///    </item>
/// </list>
/// </returns>
/// <remarks>
/// The following flags are supported:
/// <list type="table">
///    <item>
///       <term>NUI_SKELETON_TRACKING_FLAG_SUPPRESS_NO_FRAME_DATA</term>
///       <description>Prevents NuiSkeletonGetNextFrame from returning E_NUI_FRAME_NO_DATA errors. Instead, calls to NuiSkeletonGetNextFrame block until data is available or the timeout period passes.</description>
///    </item>
///    <item>
///       <term>NUI_SKELETON_TRACKING_FLAG_TITLE_SETS_TRACKED_SKELETONS</term>
///       <description>Disables the default player selection mode and enables the title to manage which players have tracked skeletons.</description>
///    </item>
///    <item>
///       <term>NUI_SKELETON_TRACKING_FLAG_ENABLE_SEATED_SUPPORT</term>
///       <description>Uses seated skeleton tracking mode. The 10 lower-body joints of each skeleton will not be tracked.</description>
///    </item>
/// </list>
/// </remarks>
  TNuiSkeletonTrackingEnable = function (
    hNextFrameEvent : THandle;
    dwFlags : DWORD
  ) : HRESULT; stdcall;

/// <summary>
/// Disables skeleton tracking.
/// </summary>
/// <returns>
/// Returns S_OK if successful; otherwise, returns one of the following error codes:
/// <list type="table">
///    <listheader>
///       <term>Error code</term>
///       <description>Description</description>
///    </listheader>
///    <item>
///       <term>__HRESULT_FROM_WIN32(ERROR_INVALID_OPERATION)</term>
///       <description>The device is uninitialized or the sensor is not initialized with the NUI_INITIALIZE_FLAG_USES_SKELETON flag. <see cref="NuiInitialize"/>.</description>
///    </item>
/// </list>
/// </returns>
  TNuiSkeletonTrackingDisable = function : HRESULT; stdcall;

/// <summary>
/// Gets the next frame of data from the skeleton stream.
/// </summary>
/// <param name="dwMillisecondsToWait">The timeout (in milliseconds) before returning without a new frame.</param>
/// <param name="pSkeletonFrame">
/// A pointer to a NUI_SKELETON_FRAME structure that receives the next image frame in the skeleton
/// stream. This must not be NULL.
/// </param>
/// <returns>
/// Returns S_OK if successful; otherwise, returns one of the following error codes:
/// <list type="table">
///    <listheader>
///       <term>Error code</term>
///       <description>Description</description>
///    </listheader>
///    <item>
///       <term>S_FALSE</term>
///       <description>The wait timeout expired before a new frame was available and NuiSkeletonTrackingEnable was passed the NUI_SKELETON_TRACKING_FLAG_TITLE_SETS_TRACKED_SKELETONS flag.</description>
///    </item>
///    <item>
///       <term>E_NUI_FRAME_NO_DATA</term>
///       <description>The wait timeout expired before a new frame was available.</description>
///    </item>
///    <item>
///       <term>E_POINTER</term>
///       <description>The <paramref name="pSkeletonFrame"/> parameter is NULL.</description>
///    </item>
/// </list>
/// </returns>
/// <seealso cref="NuiSkeletonTrackingEnable"/>
  TNuiSkeletonGetNextFrame = function (
    dwMillisecondsToWait : DWORD;
    pSkeletonFrame : PNUI_SKELETON_FRAME
  ) : HRESULT; stdcall;

/// <summary>
/// Sets an array of IDs for skeletal tracking.
/// </summary>
/// <param name="TrackingIDs">A pointer to the first DWORD in an array of tracking IDs, one for each skeleton.</param>
/// <returns>
/// <para>Returns S_OK if successful; otherwise, returns one of the following error codes:
/// <list type="table">
///    <listheader>
///       <term>Error code</term>
///       <description>Description</description>
///    </listheader>
///    <item>
///       <term>E_INVALIDARG</term>
///       <description>The <paramref name="TrackingIDs"/> parameter is NULL.</description>
///    </item>
///    <item>
///       <term>E_NUI_STREAM_NOT_ENABLED</term>
///       <description>The device is uninitialized or the sensor is not initialized with the NUI_INITIALIZE_FLAG_USES_SKELETON flag. <see cref="NuiInitialize"/>.</description>
///    </item>
///    <item>
///       <term>E_NUI_FEATURE_NOT_INITIALIZED</term>
///       <description>Skeletal tracking is not enabled with the NUI_SKELETON_TRACKING_FLAG_TITLE_SETS_TRACKED_SKELETONS flag.</description>
///    </item>
/// </list>
/// </returns>
  TTrackingIDsArray = array[0..NUI_SKELETON_MAX_TRACKED_COUNT - 1] of DWORD;

  TNuiSkeletonSetTrackedSkeletons = function (
    TrackingIDs : TTrackingIDsArray
  ) : HRESULT; stdcall;

const
// Assuming a pixel resolution of 320x240
// x_meters = (x_pixelcoord - 160) * NUI_CAMERA_DEPTH_IMAGE_TO_SKELETON_MULTIPLIER_320x240 * z_meters;
// y_meters = (y_pixelcoord - 120) * NUI_CAMERA_DEPTH_IMAGE_TO_SKELETON_MULTIPLIER_320x240 * z_meters;
  NUI_CAMERA_DEPTH_IMAGE_TO_SKELETON_MULTIPLIER_320x240 = (NUI_CAMERA_DEPTH_NOMINAL_INVERSE_FOCAL_LENGTH_IN_PIXELS);

// Assuming a pixel resolution of 320x240
// x_pixelcoord = (x_meters) * NUI_CAMERA_SKELETON_TO_DEPTH_IMAGE_MULTIPLIER_320x240 / z_meters + 160;
// y_pixelcoord = (y_meters) * NUI_CAMERA_SKELETON_TO_DEPTH_IMAGE_MULTIPLIER_320x240 / z_meters + 120;
  NUI_CAMERA_SKELETON_TO_DEPTH_IMAGE_MULTIPLIER_320x240 = (NUI_CAMERA_DEPTH_NOMINAL_FOCAL_LENGTH_IN_PIXELS);

/// <summary>
/// Returns the depth space coordinates of the specified point in skeleton space.
/// </summary>
/// <param name="vPoint">
/// The point to transform, in skeleton space coordinates. This point must have a valid depth value.
/// </param>
/// <param name="pfDepthX">
/// A pointer to a FLOAT that receives the X coordinate of the point in depth space. This pointer
/// must be non-NULL when you call the function. If this is a NULL pointer, this function returns
/// without doing anything.
/// </param>
/// <param name="pfDepthY">
/// A pointer to a FLOAT that receives the Y coordinate of the point in depth space. This pointer
/// must be non-NULL when you call the function. If this is a NULL pointer, this function returns
/// without doing anything.
/// </param>
/// <param name="pusDepthValue">
/// <para>A pointer to a USHORT that receives the depth value of the point in depth space. This
/// pointer must be non-NULL when you call the function. If this is a NULL pointer, this function
/// returns without doing anything.</para>
///
/// <para>The format of this value is the same as that for points in other depth images: the least
/// significant three bits contain a player index that is always zero, and the remaining bits are
/// the depth value, in millimeters. This value is suitable for comparison with values from an
/// image from any of the depth streams, provided that the player index information is masked
/// appropriately from the other image.</para>
/// </param>
/// <param name="eResolution">
/// A NUI_IMAGE_RESOLUTION value that specifies the resolution of the depth image.
/// </param>
/// <remarks>
/// You can use this function to place virtual objects into the color image with depth-based
/// occlusion. Call this function to determine which skeleton-space vertices of a virtual object
/// are in front of, or behind, real objects that are visible in the depth map image. After
/// occluded parts of the virtual objects have been culled, use the
/// NuiImageGetColorPixelCoordinatesFromDepthPixel function to overlay the virtual object's
/// vertices into color image pixel coordinates for rendering.
/// </remarks>
procedure NuiTransformSkeletonToDepthImage(
    vPoint : Vector4;
    out lDepthX : longint;
    out lDepthY : longint;
    out usDepthValue : word;
    eResolution : NUI_IMAGE_RESOLUTION); overload;

/// <summary>
/// Returns the depth space coordinates of the specified point in skeleton space.
/// </summary>
/// <param name="vPoint">
/// The point to transform, in skeleton space coordinates. This point must have a valid depth value.
/// </param>
/// <param name="pfDepthX">
/// A pointer to a FLOAT that receives the X coordinate of the point in depth space. This pointer
/// must be non-NULL when you call the function. If this is a NULL pointer, this function returns
/// without doing anything.
/// </param>
/// <param name="pfDepthY">
/// A pointer to a FLOAT that receives the Y coordinate of the point in depth space. This pointer
/// must be non-NULL when you call the function. If this is a NULL pointer, this function returns
/// without doing anything.
/// </param>
/// <param name="pusDepthValue">
/// <para>A pointer to a USHORT that receives the depth value of the point in depth space. This
/// pointer must be non-NULL when you call the function. If this is a NULL pointer, this function
/// returns without doing anything.</para>
///
/// <para>The format of this value is the same as that for points in other depth images: the least
/// significant three bits contain a player index that is always zero, and the remaining bits are
/// the depth value, in millimeters. This value is suitable for comparison with values from an
/// image from any of the depth streams, provided that the player index information is masked
/// appropriately from the other image.</para>
/// </param>
/// <remarks>
/// You can use this function to place virtual objects into the color image with depth-based
/// occlusion. Call this function to determine which skeleton-space vertices of a virtual object
/// are in front of, or behind, real objects that are visible in the depth map image. After
/// occluded parts of the virtual objects have been culled, use the
/// NuiImageGetColorPixelCoordinatesFromDepthPixel function to overlay the virtual object's
/// vertices into color image pixel coordinates for rendering.
/// </remarks>
procedure NuiTransformSkeletonToDepthImage(
    vPoint : Vector4;
    out lDepthX : longint;
    out lDepthY : longint;
    out usDepthValue : word); overload;

/// <summary>
/// Returns the depth space coordinates of the specified point in skeleton space.
/// </summary>
/// <param name="vPoint">
/// The point to transform, in skeleton space coordinates. This point must have a valid depth value.
/// </param>
/// <param name="pfDepthX">
/// A pointer to a FLOAT that receives the X coordinate of the point in depth space. This pointer
/// must be non-NULL when you call the function. If this is a NULL pointer, this function returns
/// without doing anything.
/// </param>
/// <param name="pfDepthY">
/// A pointer to a FLOAT that receives the Y coordinate of the point in depth space. This pointer
/// must be non-NULL when you call the function. If this is a NULL pointer, this function returns
/// without doing anything.
/// </param>
/// <param name="eResolution">
/// A NUI_IMAGE_RESOLUTION value that specifies the resolution of the depth image.
/// </param>
/// <remarks>
/// You can use this function to place virtual objects into the color image with depth-based
/// occlusion. Call this function to determine which skeleton-space vertices of a virtual object
/// are in front of, or behind, real objects that are visible in the depth map image. After
/// occluded parts of the virtual objects have been culled, use the
/// NuiImageGetColorPixelCoordinatesFromDepthPixel function to overlay the virtual object's
/// vertices into color image pixel coordinates for rendering.
/// </remarks>
procedure NuiTransformSkeletonToDepthImage(
    vPoint : Vector4;
    out fDepthX : single;
    out fDepthY : single;
    eResolution : NUI_IMAGE_RESOLUTION); overload;

/// <summary>
/// Returns the depth space coordinates of the specified point in skeleton space.
/// </summary>
/// <param name="vPoint">
/// The point to transform, in skeleton space coordinates. This point must have a valid depth value.
/// </param>
/// <param name="pfDepthX">
/// A pointer to a FLOAT that receives the X coordinate of the point in depth space. This pointer
/// must be non-NULL when you call the function. If this is a NULL pointer, this function returns
/// without doing anything.
/// </param>
/// <param name="pfDepthY">
/// A pointer to a FLOAT that receives the Y coordinate of the point in depth space. This pointer
/// must be non-NULL when you call the function. If this is a NULL pointer, this function returns
/// without doing anything.
/// </param>
/// <remarks>
/// You can use this function to place virtual objects into the color image with depth-based
/// occlusion. Call this function to determine which skeleton-space vertices of a virtual object
/// are in front of, or behind, real objects that are visible in the depth map image. After
/// occluded parts of the virtual objects have been culled, use the
/// NuiImageGetColorPixelCoordinatesFromDepthPixel function to overlay the virtual object's
/// vertices into color image pixel coordinates for rendering.
/// </remarks>
procedure NuiTransformSkeletonToDepthImage(
    vPoint : Vector4;
    out fDepthX : single;
    out fDepthY : single); overload;

/// <summary>
/// Returns the depth space coordinates of the specified point in skeleton space.
/// </summary>
/// <param name="lDepthX">The X coordinate of the depth pixel.</param>
/// <param name="lDepthY">The Y coordinate of the depth pixel.</param>
/// <param name="usDepthValue">
/// The depth value (in millimeters) of the depth image pixel, shifted left by three bits. The left
/// shift enables you to pass the value from the depth image directly into this function.
/// </param>
/// <param name="eResolution">
/// A NUI_IMAGE_RESOLUTION value that specifies the resolution of the depth image.
/// </param>
/// <returns>
/// Returns the skeleton space coordinates of the given depth image pixel (in meters).
/// </returns>
function NuiTransformDepthImageToSkeleton(
    lDepthX : longint;
    lDepthY : longint;
    usDepthValue : word;
    eResolution : NUI_IMAGE_RESOLUTION) : Vector4; overload;

/// <summary>
/// Returns the depth space coordinates of the specified point in skeleton space.
/// </summary>
/// <param name="lDepthX">The X coordinate of the depth pixel.</param>
/// <param name="lDepthY">The Y coordinate of the depth pixel.</param>
/// <param name="usDepthValue">
/// The depth value (in millimeters) of the depth image pixel, shifted left by three bits. The left
/// shift enables you to pass the value from the depth image directly into this function.
/// </param>
/// <returns>
/// Returns the skeleton space coordinates of the given depth image pixel (in meters).
/// </returns>
function NuiTransformDepthImageToSkeleton(
    lDepthX,
    lDepthY : longint;
    usDepthValue : word
    ) : Vector4; overload;

type
/// <summary>
/// Filters skeleton positions to reduce jitter between frames.
/// </summary>
/// <param name="pSkeletonFrame">
/// On entry, points to a NUI_SKELETON_FRAME structure that contains the skeleton data to be
/// smoothed. On exit, the skeleton data in the structure has been replaced by smoothed data. If
/// this function does not return S_OK, this data is unchanged.
/// </param>
/// <param name="pSmoothingParams">
/// The parameters for the smoothing function.  See the NUI_TRANSFORM_SMOOTH_PARAMETERS structure
/// for a description of the parameters.
/// </param>
/// <returns>
/// Returns S_OK if successful; otherwise, returns one of the following error codes:
/// <list type="table">
///    <listheader>
///       <term>Error code</term>
///       <description>Description</description>
///    </listheader>
///    <item>
///       <term>E_OUTOFMEMORY</term>
///       <description>Failed allocation.</description>
///    </item>
///    <item>
///       <term>E_POINTER</term>
///       <description>The <paramref name="pSkeletonFrame"/> parameter is NULL.</description>
///    </item>
/// </list>
/// </returns>
/// <remarks>
/// The filter used is based on the Holt Double Exponential Smoothing method used for statistical
/// analysis of economic data, which provides smoothing with less latency than other smoothing
/// filter algorithms. A single call of this function updates all currently-tracked skeletons.
/// </remarks>
 TNuiTransformSmooth = function (
   pSkeletonFrame : PNUI_SKELETON_FRAME;
   CONST pSmoothingParams : PNUI_TRANSFORM_SMOOTH_PARAMETERS) : HRESULT; stdcall;

/// <summary>
/// Calculate bone orientations for a skeleton.
/// </summary>
/// <param name="pSkeletonData">
/// Pointer to the skeleton data to calculate joint angles for.
/// </param>
/// <param name="pBoneOrientations">
/// Pointer to an array of NUI_SKELETON_BONE_ORIENTATION of dimension NUI_SKELETON_POSITION_COUNT (20).
/// This array must be allocated by the user before calling this function.
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
///       <description>The <paramref name="pSkeletonData"/> parameter is NULL.</description>
///    </item>
///    <item>
///       <term>E_POINTER</term>
///       <description>The <paramref name="pBoneOrientations"/> parameter is NULL.</description>
///    </item>
/// </list>
///    <item>
///       <term>S_FALSE</term>
///       <description>The joints required to calculate the skeleton root are not visible. Hierarchical bone rotations are set to Identity.</description>
///    </item>
/// </list>
/// </returns>
/// <remarks>
/// The function calculates hierarchical and absolute joint angles for the skeleton, which can
/// be used in animating an avatar (Avateering). The HipCenter joint is the root of the hierarchy,
/// and describes an absolute rotation in the right-hand camera coordinate system. All other
/// joints describe rotations relative to their parent joint orientation. The angles are returned
/// in the same order as the joints are defined.
/// </remarks>
  TNuiSkeletonCalculateBoneOrientations = function (
    const pSkeletonData : PNUI_SKELETON_DATA;
    pBoneOrientations : PNUI_SKELETON_BONE_ORIENTATION) : longint; stdcall;

implementation

procedure NuiTransformSkeletonToDepthImage(
    vPoint : Vector4;
    out lDepthX : longint;
    out lDepthY : longint;
    out usDepthValue : word;
    eResolution : NUI_IMAGE_RESOLUTION);
var
  width,
  height : DWORD;
begin
  //
  // Requires a valid depth value.
  //
  if vPoint.z > FLT_EPSILON then begin
    NuiImageResolutionToSize(eResolution, width, height);

    //
    // Center of depth sensor is at (0,0,0) in skeleton space, and
    // and (width/2,height/2) in depth image coordinates.  Note that positive Y
    // is up in skeleton space and down in image coordinates.
    //
    // The 0.5f is to correct for casting to int truncating, not rounding

    lDepthX := round(width / 2 + vPoint.x * (width/320.0) * NUI_CAMERA_SKELETON_TO_DEPTH_IMAGE_MULTIPLIER_320x240 / vPoint.z + 0.5);
    lDepthY := round(height / 2 - vPoint.y * (height/240.0) * NUI_CAMERA_SKELETON_TO_DEPTH_IMAGE_MULTIPLIER_320x240 / vPoint.z + 0.5);

    //
    //  Depth is in meters in skeleton space.
    //  The depth image pixel format has depth in millimeters shifted left by 3.
    //

    usDepthValue := round(vPoint.z *1000) shl 3;
  end else begin
    lDepthX := 0;
    lDepthY := 0;
    usDepthValue := 0;
  end;
end;

procedure NuiTransformSkeletonToDepthImage(
    vPoint : Vector4;
    out lDepthX : longint;
    out lDepthY : longint;
    out usDepthValue : word);
begin
  NuiTransformSkeletonToDepthImage(vPoint, lDepthX, lDepthY, usDepthValue,
    NUI_IMAGE_RESOLUTION_320x240);
end;

procedure NuiTransformSkeletonToDepthImage(
    vPoint : Vector4;
    out fDepthX : single;
    out fDepthY : single;
    eResolution : NUI_IMAGE_RESOLUTION);
var
  width,
  height : DWORD;
begin
  //
  // Requires a valid depth value.
  //
  if vPoint.z > FLT_EPSILON then begin
    NuiImageResolutionToSize(eResolution, width, height);

    //
    // Center of depth sensor is at (0,0,0) in skeleton space, and
    // and (width/2,height/2) in depth image coordinates.  Note that positive Y
    // is up in skeleton space and down in image coordinates.
    //
    fDepthX := width / 2 + vPoint.x * (width/320.0) * NUI_CAMERA_SKELETON_TO_DEPTH_IMAGE_MULTIPLIER_320x240 / vPoint.z;
    fDepthY := height / 2 - vPoint.y * (height/240.0) * NUI_CAMERA_SKELETON_TO_DEPTH_IMAGE_MULTIPLIER_320x240 / vPoint.z;

  end else begin
    fDepthX := 0.0;
    fDepthY := 0.0;
  end;
end;

procedure NuiTransformSkeletonToDepthImage(
    vPoint : Vector4;
    out fDepthX : single;
    out fDepthY : single);
begin
  NuiTransformSkeletonToDepthImage(vPoint, fDepthX, fDepthY,
    NUI_IMAGE_RESOLUTION_320x240);
end;

function NuiTransformDepthImageToSkeleton(
    lDepthX : longint;
    lDepthY : longint;
    usDepthValue : word;
    eResolution : NUI_IMAGE_RESOLUTION) : Vector4;
var
  width,
  height : DWORD;

  fSkeletonZ,
  fSkeletonX,
  fSkeletonY : single;
begin
  NuiImageResolutionToSize(eResolution, width, height);

  //
  //  Depth is in meters in skeleton space.
  //  The depth image pixel format has depth in millimeters shifted left by 3.
  //
  fSkeletonZ := (usDepthValue shr 3) / 1000.0;

  //
  // Center of depth sensor is at (0,0,0) in skeleton space, and
  // and (width/2,height/2) in depth image coordinates.  Note that positive Y
  // is up in skeleton space and down in image coordinates.
  //
  fSkeletonX := (lDepthX - width/2.0) * (320.0/width) *
    NUI_CAMERA_DEPTH_IMAGE_TO_SKELETON_MULTIPLIER_320x240 * fSkeletonZ;
  fSkeletonY := -(lDepthY - height/2.0) * (240.0/height) *
    NUI_CAMERA_DEPTH_IMAGE_TO_SKELETON_MULTIPLIER_320x240 * fSkeletonZ;

  //
  // Return the result as a vector.
  //
  result.x := fSkeletonX;
  result.y := fSkeletonY;
  result.z := fSkeletonZ;
  result.w := 1.0;
end;

function NuiTransformDepthImageToSkeleton(
    lDepthX,
    lDepthY : longint;
    usDepthValue : word
    ) : Vector4;
begin
  result := NuiTransformDepthImageToSkeleton(lDepthX, lDepthY, usDepthValue,
    NUI_IMAGE_RESOLUTION_320x240);
end;

end.
