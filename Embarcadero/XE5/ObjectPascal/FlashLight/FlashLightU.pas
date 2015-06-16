unit FlashLightU;

interface

uses
  System.TypInfo,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Effects,
  FMX.Objects, FMX.Layouts, FMX.Media, FMX.MobilePreview;

type
  TFlashLightForm = class(TForm)
    FlashLight: TImage;
    ImageOn: TImage;
    FlashLightShadow: TShadowEffect;
    Light: TImage;
    ImageOff: TImage;
    ContainerLayout: TLayout;
    Camera: TCameraComponent;
    GlowEffect1: TGlowEffect;
    LayoutButtons: TLayout;
    procedure FormCreate(Sender: TObject);
    procedure ImageOffClick(Sender: TObject);
    procedure ImageOnClick(Sender: TObject);
  private
    procedure SetFlashlightState(Active : Boolean);
  public
    { Public declarations }
  end;

var
  FlashLightForm: TFlashLightForm;

implementation

{$R *.fmx}

procedure TFlashLightForm.SetFlashlightState(Active : Boolean);
begin
  if Active then
  begin
    Camera.TorchMode := TTorchMode.tmModeOn;
  end else
    Camera.TorchMode := TTorchMode.tmModeOff;
end;

procedure TFlashLightForm.FormCreate(Sender: TObject);
begin
  ImageOff.Enabled := Camera.HasFlash;
  Camera.Active := True;
end;

procedure TFlashLightForm.ImageOffClick(Sender: TObject);
begin
  ImageOff.Visible := False;
  ImageOn.Visible := True;
  SetFlashlightState(True);
  Light.Visible := True;
end;

procedure TFlashLightForm.ImageOnClick(Sender: TObject);
begin
  ImageOff.Visible := True;
  ImageOn.Visible := False;
  SetFlashlightState(False);
  Light.Visible := False;
end;

end.
