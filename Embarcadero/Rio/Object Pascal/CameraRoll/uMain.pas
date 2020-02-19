//---------------------------------------------------------------------------

// This software is Copyright (c) 2015-2019 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Permissions,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, System.Sensors, FMX.Media,
  FMX.Objects, FMX.MediaLibrary.Actions, System.Actions, FMX.ActnList, FMX.Graphics,
  FMX.StdActns, FMX.Controls.Presentation;

type
  TCameraRollForm = class(TForm)
  private
    procedure DisplayRationale(Sender: TObject; const APermissions: TArray<string>; const APostRationaleProc: TProc);
    procedure LoadPicturePermissionRequestResult(Sender: TObject; const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>);
  published var
    btnPhotoLibrary: TButton;
    imgPhotoLibraryImage: TImage;
    alGetCameraRoll: TActionList;
    TakePhotoFromLibraryAction1: TTakePhotoFromLibraryAction;
    ToolBar1: TToolBar;
    Label1: TLabel;
  published
    procedure TakePhotoFromLibraryAction1DidFinishTaking(Image: TBitmap);
    procedure btnPhotoLibraryClick(Sender: TObject);
  end;

implementation

uses
  FMX.DialogService;

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

procedure TCameraRollForm.btnPhotoLibraryClick(Sender: TObject);
const
  PermissionReadExternalStorage = 'android.permission.READ_EXTERNAL_STORAGE';
begin
  PermissionsService.RequestPermissions([PermissionReadExternalStorage], LoadPicturePermissionRequestResult, DisplayRationale)
end;

// Optional rationale display routine to display permission requirement rationale to the user
procedure TCameraRollForm.DisplayRationale(Sender: TObject; const APermissions: TArray<string>; const APostRationaleProc: TProc);
begin
  // Show an explanation to the user *asynchronously* - don't block this thread waiting for the user's response!
  // After the user sees the explanation, invoke the post-rationale routine to request the permissions
  TDialogService.ShowMessage('The app needs to read a photo file from your device to show it to you',
    procedure(const AResult: TModalResult)
    begin
      APostRationaleProc;
    end)
end;

procedure TCameraRollForm.LoadPicturePermissionRequestResult(Sender: TObject; const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>);
begin
  // 1 permission involved: READ_EXTERNAL_STORAGE
  if (Length(AGrantResults) = 1) and (AGrantResults[0] = TPermissionStatus.Granted) then
    TakePhotoFromLibraryAction1.Execute
  else
    TDialogService.ShowMessage('Cannot load the photo because the required permission is not granted')
end;

procedure TCameraRollForm.TakePhotoFromLibraryAction1DidFinishTaking(Image: TBitmap);
begin
  { Assign the image retrieved from the Photo Library to the TImage component. }
  imgPhotoLibraryImage.Bitmap.Assign(Image);
end;

end.
