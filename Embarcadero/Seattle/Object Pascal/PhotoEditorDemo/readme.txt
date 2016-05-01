{******************************************************************************}
{                                                                              }
{                         Delphi FireMonkey Platform                           }
{                                                                              }
{                            Photo Editor Sample                               }
{                                                                              }
{                Copyright(c) 2013 Embarcadero Technologies, Inc.              }
{                                                                              }
{******************************************************************************}

This sample demonstrates ways of getting images on a device and their use by
third-party services of an iPhone. This sample is a simple photo editor with
following use cases:

  1. Getting an image from iPhone Camera
  2. Getting an image from iPhone Photo Library
  3. Applying simple filters
  4. Sharing the changed image with other iOS Services (SMS, Save to The Photo
     Library, Setting as wallpaper, copying into the buffer,etc.)

I. Getting image

For getting image FireMonkey has two standard actions:

  - TTakePhotoFromLibraryAction - take photo from the Photo Library
  - TTakePhotoFromCameraAction - take photo from the Photo Camera

For getting the image you need to link one of the actions to a control. The Actions
return an image through the parameter of the OnDidFinishTaking(Image: TBitmap) event.

II. Editing image

For image correction we use FireMonkey filters. There is some distinction between
filters and effects:

  - Filter is a low level object, which works with bitmaps. Filters make output images.
  - Effects is a high level object, which works with FireMonkey controls and
    uses Filters. Effects don't create an output image.

We use filters to retrieve the image after applying effects.

III. Send to other services

For sending an image to another application (Sharing), FireMonkey has the standard action
TShowShareSheetAction, which enables you to:

  1. Save an image to a Photo Library
  2. Set an image as a photo contact in an Address Book
  3. Print an image
  4. Copy an image to the clipboard.
  5. Send an image via EMail.

The list of actions depends on the list of installed applications on your device.
TShowShareSheetAction does not allow you to change the available activities.
 
The list of activities depends on data which you try to send. You can send
only text or image using TShowShareSheetAction.TextMessage and TShowShareSheetAction.Bitmap.

For transferring your data to TShowShareSheetAction, action has a special event
TShowShareSheetAction.OnBeforeExecute.

IV. General way to apply effects on the received image

TFilterManager stores full meta information about each filter in FireMonkey. It contains:

  - Name of filter
  - Short description
  - Set of filter's parameters:
    * Parameter's Name
    * Min, Max and Default values;
    * Description
    * and other

We can use this information for a common method to get any filter by the filter's name,  then creating this filter
and applying it to the image.



