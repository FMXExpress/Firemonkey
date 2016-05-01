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
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.StdCtrls,
  System.Actions, FMX.ActnList, FMX.StdActns, FMX.MediaLibrary.Actions, FMX.Graphics,
  FMX.Controls.Presentation;

type
  TShareSheetForm = class(TForm)
    ActionList1: TActionList;
    ShowShareSheetAction1: TShowShareSheetAction;
    Action1: TAction;
    TakePhotoFromCameraAction1: TTakePhotoFromCameraAction;
    TopToolbar: TToolBar;
    Label1: TLabel;
    BottomToolbar: TToolBar;
    btnShare: TButton;
    btnTakePhoto: TButton;
    imgCameraPicture: TImage;
    procedure ShowShareSheetAction1BeforeExecute(Sender: TObject);
    procedure TakePhotoFromCameraAction1DidFinishTaking(Image: TBitmap);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ShareSheetForm: TShareSheetForm;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

procedure TShareSheetForm.ShowShareSheetAction1BeforeExecute(Sender: TObject);
begin
  { show the share sheet }
  ShowShareSheetAction1.Bitmap.Assign(imgCameraPicture.Bitmap);
end;

procedure TShareSheetForm.TakePhotoFromCameraAction1DidFinishTaking(Image: TBitmap);
begin
  { display the picture taken from the camera to the TImage control }
  imgCameraPicture.Bitmap.Assign(Image);
end;

end.
