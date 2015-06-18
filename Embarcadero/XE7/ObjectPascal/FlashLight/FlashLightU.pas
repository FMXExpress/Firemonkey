//---------------------------------------------------------------------------

// This software is Copyright (c) 2013 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit FlashLightU;

interface

uses
  System.TypInfo,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Effects,
  FMX.Objects, FMX.Layouts, FMX.Media;

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
{$R *.LgXhdpiPh.fmx ANDROID}

procedure TFlashLightForm.SetFlashlightState(Active : Boolean);
begin
  if Active then
  begin
    Camera.TorchMode := TTorchMode.ModeOn;
  end else
    Camera.TorchMode := TTorchMode.ModeOff;
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
