//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, System.Actions,
  FMX.ActnList, FMX.StdActns, FMX.MediaLibrary.Actions, FMX.Objects, FMX.Graphics,
  FMX.Controls.Presentation;

type
  TAccessCameraAppForm = class(TForm)
    alGetFromCamera: TActionList;
    TakePhotoFromCameraAction1: TTakePhotoFromCameraAction;
    imgCameraImage: TImage;
    ToolBar1: TToolBar;
    btnTakePhoto: TButton;
    Label1: TLabel;
    procedure TakePhotoFromCameraAction1DidFinishTaking(Image: TBitmap);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AccessCameraAppForm: TAccessCameraAppForm;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

procedure TAccessCameraAppForm.TakePhotoFromCameraAction1DidFinishTaking(Image: TBitmap);
begin
  { Assign the image retrieved from the Camera to the TImage component. }
  imgCameraImage.Bitmap.Assign(Image);
end;

end.
