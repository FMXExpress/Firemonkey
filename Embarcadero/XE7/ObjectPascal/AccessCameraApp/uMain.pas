unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, System.Actions,
  FMX.ActnList, FMX.StdActns, FMX.MediaLibrary.Actions, FMX.Objects, FMX.Graphics;

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