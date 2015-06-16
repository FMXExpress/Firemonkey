unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms3D, FMX.Forms, FMX.Dialogs, System.Sensors,
  FMX.Controls3D, FMX.Objects3D, FMX.StdCtrls, FMX.Layers3D,
  FMX.MaterialSources, FMX.Types3D;

type
  TGyroscopeForm = class(TForm3D)
    Rectangle3D1: TRectangle3D;
    Timer1: TTimer;
    LightMaterialSource1: TLightMaterialSource;
    Light1: TLight;
    Layer3D1: TLayer3D;
    Label1: TLabel;
    procedure Timer1Timer(Sender: TObject);
    procedure Form3DCreate(Sender: TObject);
  private
    { Private declarations }
    FSensors: TSensorArray;
    FSensorManager: TSensorManager;
  public
    { Public declarations }
  end;

var
  GyroscopeForm: TGyroscopeForm;

implementation

{$R *.fmx}

procedure TGyroscopeForm.Form3DCreate(Sender: TObject);
begin
  { attempt to get and activate the sensor manager }
  FSensorManager := TSensorManager.Current;
  FSensorManager.Activate;

  { attempt to get an orientation sensor }
  FSensors := TSensorManager.Current.GetSensorsByCategory(TSensorCategory.Orientation);

  if Length(FSensors) = 0 then
  begin
    Label1.Text := 'Gyro not found';
    Exit; { no sensors available }
  end;

  if not (FSensors[0] is TCustomOrientationSensor) then
    Exit; { no orientation sensor is available }

  { start the sensor if it is not started }
  if not TCustomOrientationSensor(FSensors[0]).Started then
  begin
    TCustomOrientationSensor(FSensors[0]).Start;
    Timer1.Enabled := True;
  end;
end;

procedure TGyroscopeForm.Timer1Timer(Sender: TObject);
begin
  { check for sensor assignment }
  if Length(FSensors) > 0 then
    if Assigned(FSensors[0]) then
    begin
      { and rotate the cube }
      Rectangle3D1.RotationAngle.X := TCustomOrientationSensor(FSensors[0]).TiltX;
      Rectangle3D1.RotationAngle.Y := TCustomOrientationSensor(FSensors[0]).TiltY;
      Rectangle3D1.RotationAngle.Z := TCustomOrientationSensor(FSensors[0]).TiltZ;
      Label1.Text := Format('Gyro: %3.1f %3.1f %3.1f',[Rectangle3D1.RotationAngle.X,
        Rectangle3D1.RotationAngle.Y,
        Rectangle3D1.RotationAngle.Z]);
    end;
end;

end.
