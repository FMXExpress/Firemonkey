unit GlassVoiceLaunchUnit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Sensors,
  FMX.StdCtrls, FMX.Sensors, FMX.Objects, FMX.Ani, Generics.Collections;

type
  TForm2 = class(TForm)
    StyleBook1: TStyleBook;
    MotionSensor1: TMotionSensor;
    LineActive: TLine;
    LabelX: TLabel;
    LabelZ: TLabel;
    LineReference: TLine;
    FloatAnimationX: TFloatAnimation;
    FloatAnimationZ: TFloatAnimation;
    procedure MotionSensor1DataChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FloatAnimationXFinish(Sender: TObject);
    procedure FloatAnimationZFinish(Sender: TObject);
  private
    { Private declarations }
    FXSample: TList<Single>;
    FZSample: TList<Single>;
    const SampleCount = 20;
    procedure UpdateZ;
    procedure UpdateX;

  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

function Average(collection: TList<Single>): Single;
var
  sample: Single;
begin
  result := 0;
  for sample in collection do
  begin
    result := result + sample;
  end;
  result := result / collection.Count;
end;

procedure TForm2.FloatAnimationXFinish(Sender: TObject);
begin
 UpdateX;
end;

procedure TForm2.FloatAnimationZFinish(Sender: TObject);
begin
  UpdateZ;
end;

procedure TForm2.FormCreate(Sender: TObject);
var
  Test: TList<Integer>;
begin
  FXSample := TList<Single>.Create;
  FZSample := TList<Single>.Create;
end;

procedure TForm2.UpdateX();
var
  x: Single;
begin
  if not FloatAnimationX.Running then
  begin
    x := Average(FXSample);
    FXSample.Clear;
    LabelX.Text := Format('L/R: %6.2f ', [X]);
    FloatAnimationX.Enabled := False;
    FloatAnimationX.StopValue := X * 9;
    FloatAnimationX.Enabled := True;
  end;
end;

procedure TForm2.UpdateZ;
var
  z: Single;
begin
  if not FloatAnimationZ.Running then
  begin
    z := Average(FZSample);
    FZSample.Clear;
    LabelZ.Text := Format('U/D: %6.2f ', [Z]);
    FloatAnimationZ.Enabled := False;
    FloatAnimationZ.StopValue := LineReference.Position.Y + (Z * (Height div -10));
    FloatAnimationZ.Enabled := True;
  end;
end;

procedure TForm2.MotionSensor1DataChanged(Sender: TObject);
var
  LProp: TCustomMotionSensor.TProperty;
  x, z: Single;
begin
  for LProp in MotionSensor1.Sensor.AvailableProperties do
  begin
    { get the data from the sensor }
    case LProp of
      TCustomMotionSensor.TProperty.AccelerationX:
      begin
        x := MotionSensor1.Sensor.AccelerationX;
      end;
      TCustomMotionSensor.TProperty.AngleAccelX:
      begin
        x := MotionSensor1.Sensor.AngleAccelX;
      end;
      TCustomMotionSensor.TProperty.AccelerationZ:
      begin
        z := MotionSensor1.Sensor.AccelerationZ;
      end;
      TCustomMotionSensor.TProperty.AngleAccelZ:
      begin
        z := MotionSensor1.Sensor.AngleAccelZ;
      end;
    end;
  end;


  FXSample.Add(X);
  FZSample.Add(Z);
end;

end.
