unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, System.Sensors, FMX.StdCtrls,
  FMX.ListBox, FMX.Layouts, System.Sensors.Components, FMX.Objects, System.Math, FMX.Platform;

type
  TAccelerometerForm = class(TForm)
    swAccelerometerSensorActive: TSwitch;
    Timer1: TTimer;
    ToolBar1: TToolBar;
    Label1: TLabel;
    ListBox1: TListBox;
    lbAccelerometerSensor: TListBoxItem;
    lbAccelerationX: TListBoxItem;
    lbAccelerationY: TListBoxItem;
    lbAccelerationZ: TListBoxItem;
    lbAngleAccelX: TListBoxItem;
    lbAngleAccelY: TListBoxItem;
    lbAngleAccelZ: TListBoxItem;
    lbMotion: TListBoxItem;
    lbSpeed: TListBoxItem;
    MotionSensor1: TMotionSensor;
    Layout1: TLayout;
    Circle1: TCircle;
    Circle2: TCircle;
    Circle3: TCircle;
    Left: TCircle;
    Circle5: TCircle;
    Right: TCircle;
    Circle4: TCircle;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure swAccelerometerSensorActiveSwitch(Sender: TObject);
  private
    { Private declarations }
    LastGoodX,LastGoodY: Single;
    function GetScreenOrientation: TScreenOrientation;
  public
    { Public declarations }
  end;

var
  AccelerometerForm: TAccelerometerForm;

implementation

{$R *.fmx}

function TAccelerometerForm.GetScreenOrientation: TScreenOrientation;
begin
  Result := IFMXScreenService(TPlatformServices.Current.GetPlatformService(IFMXScreenService)).GetScreenOrientation;
end;

procedure TAccelerometerForm.FormCreate(Sender: TObject);
begin
    // start with no sensors

  lbAccelerationX.Visible := False;
  lbAccelerationY.Visible := False;
  lbAccelerationZ.Visible := False;
  lbAngleAccelX.Visible := False;
  lbAngleAccelY.Visible := False;
  lbAngleAccelZ.Visible := False;
  lbMotion.Visible := False;
  lbSpeed.Visible := False;

{$ifdef IOS}
  {$ifndef CPUARM}
    lbAccelerometerSensor.Text := 'Simulator - no sensors';
    swAccelerometerSensorActive.Enabled := False;
  {$endif}
{$endif}

end;

procedure TAccelerometerForm.swAccelerometerSensorActiveSwitch(Sender: TObject);
begin
  { activate or deactivate the reading of the accelerometer sensor }
  MotionSensor1.Active := swAccelerometerSensorActive.IsChecked;
  Timer1.Enabled := swAccelerometerSensorActive.IsChecked;
end;

procedure TAccelerometerForm.Timer1Timer(Sender: TObject);
var
  LProp: TCustomMotionSensor.TProperty;
  TmpX1, TmpY1, TmpX2, TmpY2: Single;
  Diameter, Distance: Single;
  X, Y: Single;
begin
  for LProp in MotionSensor1.Sensor.AvailableProperties do
  begin
    { get the data from the sensor }
    case LProp of
      TCustomMotionSensor.TProperty.AccelerationX:
      begin
        lbAccelerationX.Visible := True;
        lbAccelerationX.Text := Format('Acceleration X: %6.2f', [MotionSensor1.Sensor.AccelerationX]);
      end;
      TCustomMotionSensor.TProperty.AccelerationY:
      begin
        lbAccelerationY.Visible := True;
        lbAccelerationY.Text := Format('Acceleration Y: %6.2f', [MotionSensor1.Sensor.AccelerationY]);
      end;
      TCustomMotionSensor.TProperty.AccelerationZ:
      begin
        lbAccelerationZ.Visible := True;
        lbAccelerationZ.Text := Format('Acceleration Z: %6.2f', [MotionSensor1.Sensor.AccelerationZ]);
      end;
      TCustomMotionSensor.TProperty.AngleAccelX:
      begin
        lbAngleAccelX.Visible := True;
        lbAngleAccelX.Text := Format('Angle X: %6.2f', [MotionSensor1.Sensor.AngleAccelX]);
      end;
      TCustomMotionSensor.TProperty.AngleAccelY:
      begin
        lbAngleAccelY.Visible := True;
        lbAngleAccelY.Text := Format('Angle Y: %6.2f', [MotionSensor1.Sensor.AngleAccelY]);
      end;
      TCustomMotionSensor.TProperty.AngleAccelZ:
      begin
        lbAngleAccelZ.Visible := True;
        lbAngleAccelZ.Text := Format('Angle Z: %6.2f', [MotionSensor1.Sensor.AngleAccelZ]);
      end;
      TCustomMotionSensor.TProperty.Motion:
      begin
        lbMotion.Visible := True;
        lbMotion.Text := Format('Motion: %6.2f', [MotionSensor1.Sensor.Motion]);
      end;
      TCustomMotionSensor.TProperty.Speed:
      begin
        lbSpeed.Visible := True;
        lbSpeed.Text := Format('Speed: %6.2f', [MotionSensor1.Sensor.Speed]);
      end;
    end;
  end;

  Diameter := Circle2.Width/2 - Circle1.Width/2;

  TmpX1 := Circle3.Position.X+(Circle3.Width/2);
  TmpY1 := Circle3.Position.Y+(Circle3.Height/2);

  TmpX2 := Circle1.Position.X+(Circle1.Width/2);
  TmpY2 := Circle1.Position.Y+(Circle1.Height/2);


  Distance := Sqrt(Sqr(TmpX2-TmpX1)+Sqr(TmpY2-TmpY1));


  if Distance<Diameter then
   begin

    if (GetScreenOrientation=TScreenOrientation.Portrait) OR (GetScreenOrientation=TScreenOrientation.InvertedPortrait) then
     begin
      X := (MotionSensor1.Sensor.AccelerationX*10);
      Y := (MotionSensor1.Sensor.AccelerationY*10);
     end
    else
     begin
      Y := (MotionSensor1.Sensor.AccelerationX*10);
      X := (MotionSensor1.Sensor.AccelerationY*10);
     end;

    LastGoodX := Circle1.Position.X;
    LastGoodY := Circle1.Position.Y;
    Circle1.position.x := Circle1.position.x - X;
    Circle1.position.y := Circle1.position.y + Y;

    if (Y>-7) then
     begin
      Circle4.Visible := True;
     end
    else
     begin
      Circle4.Visible := False;
     end;


    if (GetScreenOrientation=TScreenOrientation.Portrait) OR (GetScreenOrientation=TScreenOrientation.InvertedPortrait) then
     begin

       // right
      if (X>2) then
       begin
        Right.Visible := True;
       end
      else
       begin
        Right.Visible := False;
       end;

       // left
      if (X<-2) then
       begin
        Left.Visible := True;
       end
      else
       begin
        Left.Visible := False;
       end;

     end
    else
     begin

       // left
      if (X>2) then
       begin
        Left.Visible := True;
       end
      else
       begin
        Left.Visible := False;
       end;

       // right
      if (X<-2) then
       begin
        Right.Visible := True;
       end
      else
       begin
        Right.Visible := False;
       end;

     end;


   end
   else
    begin
      Circle1.position.x := LastGoodX;
      Circle1.position.y := LastGoodY;
    end;

end;

end.
