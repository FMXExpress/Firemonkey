unit MainFrm_Phone;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.ActnList, FMX.MediaLibrary.Actions, System.Actions, FMX.StdActns, FMX.Objects,
  MainFrm, FMX.Effects, FMX.Layouts, FMX.Ani;

type
  TPhoneMainForm = class(TBaseMainForm)
    TextToSelectImage: TText;
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure ImageContainerClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PhoneMainForm: TPhoneMainForm;

implementation

{$R *.fmx}

procedure TPhoneMainForm.ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
begin
  inherited;
  TextToSelectImage.Visible := FRawBitmap.IsEmpty;
end;

procedure TPhoneMainForm.ImageContainerClick(Sender: TObject);
begin
  if FRawBitmap.IsEmpty then
    ActionTakePhotoFromLibrary.ExecuteTarget(ButtonTakePhotoFromLibrary);
end;

end.
