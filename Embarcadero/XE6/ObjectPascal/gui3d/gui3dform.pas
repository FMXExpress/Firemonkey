
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit gui3dform;

interface

uses
  System.SysUtils, System.Types, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Types3D, FMX.Layers3D, FMX.Ani,
  FMX.Effects, FMX.Filter.Effects, FMX.Objects, FMX.Styles, FMX.StdCtrls, FMX.Forms3D, 
  FMX.Controls3D;

type
  TForm29 = class(TForm3D)
    Layer3D1: TLayer3D;
    Button1: TButton;
    AniIndicator1: TAniIndicator;
    Panel1: TPanel;
    Label1: TLabel;
    Resources1: TStyleBook;
    RectAnimation1: TRectAnimation;
    Ellipse1: TEllipse;
    GlowEffect1: TGlowEffect;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form29: TForm29;

implementation

{$R *.fmx}

procedure TForm29.Button1Click(Sender: TObject);
begin
  if ClientWidth > ClientHeight then
    Layer3D1.AnimateFloat('RotationAngle.X', 360, 2, TAnimationType.InOut, TInterpolationType.Back)
  else
    Layer3D1.AnimateFloat('RotationAngle.Y', 360, 2, TAnimationType.InOut, TInterpolationType.Back);
  Layer3D1.AnimateFloat('Position.Z', 500, 1);
  Layer3D1.AnimateFloatDelay('Position.Z', 0, 1, 1);
end;

end.
