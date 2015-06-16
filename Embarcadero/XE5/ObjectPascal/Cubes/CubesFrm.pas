
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit CubesFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layers3D, FMX.Objects,
  FMX.Types3D, FMX.Platform, FMX.Ani, FMX.Objects3D, FMX.MaterialSources,
  FMX.Forms3D, FMX.Controls3D;

type
  TForm268 = class(TForm3D)
    FloatAnimation1: TFloatAnimation;
    FloatAnimation2: TFloatAnimation;
    Light1: TLight;
    Timer2: TTimer;
    Dummy1: TDummy;
    Material1: TLightMaterialSource;
    Material2: TLightMaterialSource;
    Light2: TLight;
    procedure Form3DCreate(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form268: TForm268;

implementation

{$R *.fmx}

procedure TForm268.Form3DCreate(Sender: TObject);
const 
  W = 10;
  H = 10;
  Z = 10;
  S = 1.5;
var
  I, J, K: Integer;
  Img: TControl3D;
begin
  BeginUpdate;
  for I := 1 to W do
    for J := 1 to H do
    for K := 1 to Z do
    begin
      Img := TCube.Create(nil);
      Img.Parent := Dummy1;
      if random < 0.5 then
        TCube(Img).MaterialSource := Material1
      else
        TCube(Img).MaterialSource := Material2;
      Img.SetSize(0.95, 0.95, 0.95);
      Img.Position.Point := Point3D(-W/2*S + I * S, 1 + H/2*S - J * S, K * S);
      with TFloatAnimation.Create(nil) do
      begin
        Parent := Img;
        PropertyName := 'RotationAngle.X';
        StopValue := 180;
        AutoReverse := True;
        Duration := 1 + random * 2;
        Loop := True;
        Enabled := True;
        Interpolation := TInterpolationType.itBack;
      end;
    end;
  EndUpdate;  
end;

procedure TForm268.Timer2Timer(Sender: TObject);
begin
  Caption := '1000 Cubes FPS:' + FloatToStr(Context.FPS) + ' Shader switch: ' + IntToStr(Context.ChangeShaderCount);
end;

initialization
  AniFrameRate := 60;
end.
