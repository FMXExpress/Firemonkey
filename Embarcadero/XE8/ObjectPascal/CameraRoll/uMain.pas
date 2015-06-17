unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, System.Sensors, FMX.Media,
  FMX.Objects, FMX.MediaLibrary.Actions, System.Actions, FMX.ActnList, FMX.Graphics,
  FMX.StdActns;

type
  TCameraRollForm = class(TForm)
    btnPhotoLibrary: TButton;
    imgPhotoLibraryImage: TImage;
    alGetCameraRoll: TActionList;
    TakePhotoFromLibraryAction1: TTakePhotoFromLibraryAction;
    ToolBar1: TToolBar;
    Label1: TLabel;
    procedure TakePhotoFromLibraryAction1DidFinishTaking(Image: TBitmap);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CameraRollForm: TCameraRollForm;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

procedure TCameraRollForm.TakePhotoFromLibraryAction1DidFinishTaking(Image: TBitmap);
begin
  { Assign the image retrieved from the Photo Library to the TImage component. }
  imgPhotoLibraryImage.Bitmap.Assign(Image);
end;

end.
