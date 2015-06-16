
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit anidemofrm;

interface

uses
  System.SysUtils, System.Types, System.Variants, System.UITypes,
  System.Classes, FMX.Types, FMX.Graphics, FMX.Dialogs, FMX.Types3D, FMX.Forms,
  FMX.Forms3D, FMX.Controls3D, System.Math.Vectors, FMX.Objects, FMX.Controls,
  FMX.Ani, FMX.Layouts, FMX.Objects3D, FMX.Layers3D, FMX.MaterialSources;

type
  TfrmAniDemo = class(TForm3D)
    TextureLighting: TLightMaterialSource;
    ColorLighting: TLightMaterialSource;
    Light1: TLight;
    Light2: TLight;
    ObjectLayer1: TLayer3D;
    Plane1: TPlane;
    RoundCube1: TRoundCube;
    Sphere1: TSphere;
    Sphere2: TSphere;
    StrokeCube1: TStrokeCube;
    Text3: TTextLayer3D;
    Text3D1: TText3D;
    Path1: TPath;
    RoundRect1: TRoundRect;
    Background1: TLayout;
    Text4: TTextLayer3D;
    Text2: TTextLayer3D;
    Text1: TTextLayer3D;
    FloatAnimation1: TFloatAnimation;
    FloatAnimation2: TFloatAnimation;
    FloatAnimation3: TFloatAnimation;
    FloatAnimation4: TFloatAnimation;
    PathAnimation1: TPathAnimation;
    Text5: TText;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmAniDemo: TfrmAniDemo;

implementation

{$R *.fmx}

end.
