unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.IniFiles,
  System.Messaging, System.IOUtils, FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Layouts, System.Math, FMX.StdCtrls, FMX.Ani, FMX.Media, FMX.Platform,
  FMX.Filter.Effects, FMX.Effects, System.Sensors, System.Sensors.Components,
  FMX.ListBox;

type
  TOrientationSensorForm = class(TForm)
    OrientationSensor1: TOrientationSensor;
    swOrientationSensorActive: TSwitch;
    ToolBar1: TToolBar;
    Label1: TLabel;
    ListBox1: TListBox;
    lbOrientationSensor: TListBoxItem;
    lbTiltX: TListBoxItem;
    lbTiltY: TListBoxItem;
    lbTiltZ: TListBoxItem;
    lbHeadingX: TListBoxItem;
    lbHeadingY: TListBoxItem;
    lbHeadingZ: TListBoxItem;
    Layout1: TLayout;
    TiltButton: TSpeedButton;
    HeadingButton: TSpeedButton;
    Timer1: TTimer;
    Ship: TRectangle;
    Thruster: TRectangle;
    GlowEffect1: TGlowEffect;
    Button1: TButton;
    Layout2: TLayout;
    procedure swOrientationSensorActiveSwitch(Sender: TObject);
    procedure OrientationSensor1SensorChoosing(Sender: TObject;
      const Sensors: TSensorArray; var ChoseSensorIndex: Integer);
    procedure TiltButtonClick(Sender: TObject);
    procedure HeadingButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    DefaultX,DefaultY,DefaultZ: Single;
    PlayerDataVerticalVelocity, PlayerDataHorizontalVelocity: Single;
    ScreenOrientation: TScreenOrientation;
    OrientationChangedId: Integer;
    procedure OrientationChanged(const Sender: TObject; const Msg: TMessage);
    function GetScreenOrientation: TScreenOrientation;
  public
    { Public declarations }
  end;

var
  OrientationSensorForm: TOrientationSensorForm;

implementation

{$R *.fmx}

procedure TOrientationSensorForm.OrientationSensor1SensorChoosing(
  Sender: TObject; const Sensors: TSensorArray; var ChoseSensorIndex: Integer);
var
  I: Integer;
  Found: Integer;
begin
  Found := -1;
  for I := 0 to High(Sensors) do
  begin
    if TiltButton.IsPressed and (TCustomOrientationSensor.TProperty.TiltX in TCustomOrientationSensor(Sensors[I]).AvailableProperties) then
    begin
        Found := I;
        Break;
    end
    else if HeadingButton.IsPressed and (TCustomOrientationSensor.TProperty.HeadingX in TCustomOrientationSensor(Sensors[I]).AvailableProperties) then
    begin
      Found := I;
      Break;
    end;
  end;

  if Found < 0 then
  begin
    Found := 0;
    TiltButton.IsPressed := True;
    HeadingButton.IsPressed := False;
    ShowMessage('Compass not available');
  end;

  ChoseSensorIndex := Found;
end;

procedure TOrientationSensorForm.TiltButtonClick(Sender: TObject);
begin
  OrientationSensor1.Active := False;
  HeadingButton.IsPressed := False;
  TiltButton.IsPressed := True;
  OrientationSensor1.Active := swOrientationSensorActive.IsChecked;
end;

procedure TOrientationSensorForm.Timer1Timer(Sender: TObject);
var
ShipAngle: Single;
HeadingValue,DefaultValue: Single;
ScreenOrienation: TScreenOrientation;
Thrust: Single;
begin
  { get the data from the sensor }
  lbTiltX.Text := Format('Tilt X: %f', [OrientationSensor1.Sensor.TiltX]);
  lbTiltY.Text := Format('Tilt Y: %f', [OrientationSensor1.Sensor.TiltY]);
  lbTiltZ.Text := Format('Tilt Z: %f', [OrientationSensor1.Sensor.TiltZ]);
  lbHeadingX.Text := Format('Heading X: %f', [OrientationSensor1.Sensor.HeadingX]);
  lbHeadingY.Text := Format('Heading Y: %f', [OrientationSensor1.Sensor.HeadingY]);
  lbHeadingZ.Text := Format('Heading Z: %f', [OrientationSensor1.Sensor.HeadingZ]);
  if DefaultX=0 then
   DefaultX := OrientationSensor1.Sensor.HeadingX;
  if DefaultY=0 then
   DefaultY := OrientationSensor1.Sensor.HeadingY;
  if DefaultZ=0 then
   DefaultZ := OrientationSensor1.Sensor.HeadingZ;

  ScreenOrienation := GetScreenOrientation;

  if (ScreenOrientation=TScreenOrientation.Landscape) OR (ScreenOrientation=TScreenOrientation.InvertedLandscape) then
   begin
    HeadingValue := OrientationSensor1.Sensor.HeadingX;
    DefaultValue := DefaultX;
   end
  else
   begin
    HeadingValue := OrientationSensor1.Sensor.HeadingY;
    DefaultValue := DefaultY;
   end;

  if (HeadingValue+10)>DefaultValue then
   begin
     Ship.RotationAngle := Ship.RotationAngle + 15;
   end
  else
  if (HeadingValue+5)>DefaultValue then
   begin
     Ship.RotationAngle := Ship.RotationAngle + 5;
   end
  else
  if (HeadingValue+1)>DefaultValue then
   begin
     Ship.RotationAngle := Ship.RotationAngle + 1;
   end;

  if (HeadingValue-10)<DefaultValue then
   begin
     Ship.RotationAngle := Ship.RotationAngle - 15;
   end
  else
  if (HeadingValue-5)<DefaultValue then
   begin
     Ship.RotationAngle := Ship.RotationAngle - 5;
   end
  else
  if (HeadingValue-1)<DefaultValue then
   begin
     Ship.RotationAngle := Ship.RotationAngle - 1;
   end;

  //

  if (OrientationSensor1.Sensor.HeadingZ+20)>DefaultZ then
   begin
     Thrust := 15*0.01;
   end
  else
  if (OrientationSensor1.Sensor.HeadingZ+10)>DefaultZ then
   begin
     Thrust := 5*0.01;
   end
  else
  if (OrientationSensor1.Sensor.HeadingZ+5)>DefaultZ then
   begin
     Thrust := 1*0.01;
   end;

{  if (OrientationSensor1.Sensor.HeadingZ-20)<DefaultZ then
   begin
     Thrust := -15*0.01;
   end
  else
  if (OrientationSensor1.Sensor.HeadingZ-10)<DefaultZ then
   begin
     Thrust := -5*0.01;
   end
  else
  if (OrientationSensor1.Sensor.HeadingZ-5)<DefaultZ then
   begin
     Thrust := -1*0.01;
   end;     }

    PlayerDataVerticalVelocity := PlayerDataVerticalVelocity - Thrust;
    PlayerDataHorizontalVelocity := PlayerDataHorizontalVelocity + Thrust;

    //PlayerDataVerticalVelocity := PlayerDataVerticalVelocity + 0.1;
   // PlayerDataHorizontalVelocity := Max(0, PlayerDataHorizontalVelocity - 0.1);

    ShipAngle := (Ship.RotationAngle) * (PI / 180);
    Ship.Position.X := Ship.Position.X + PlayerDataHorizontalVelocity * Sin(ShipAngle);
    Ship.Position.Y := Ship.Position.Y + PlayerDataVerticalVelocity;

end;

procedure TOrientationSensorForm.Button1Click(Sender: TObject);
begin
ship.Position.X := Layout2.Width/2;
ship.Position.Y := Layout2.Height/2;
  DefaultX := 0;
  DefaultY := 0;
  DefaultZ := 0;
  PlayerDataVerticalVelocity := 0;
  PlayerDataHorizontalVelocity := 0;
  Ship.RotationAngle := 0;
end;

procedure TOrientationSensorForm.FormActivate(Sender: TObject);
begin
{$ifdef IOS}
  {$ifndef CPUARM}
    lbOrientationSensor.Text := 'Simulator - no sensors';
    swOrientationSensorActive.Enabled := False;
  {$endif}
{$endif}
end;

procedure TOrientationSensorForm.HeadingButtonClick(Sender: TObject);
begin
  OrientationSensor1.Active := False;
  TiltButton.IsPressed := False;
  HeadingButton.IsPressed := True;
  OrientationSensor1.Active := swOrientationSensorActive.IsChecked;
end;

procedure TOrientationSensorForm.swOrientationSensorActiveSwitch(
  Sender: TObject);
begin
  { activate or deactivate the orientation sensor }
  OrientationSensor1.Active := swOrientationSensorActive.IsChecked;
  Timer1.Enabled := swOrientationSensorActive.IsChecked;
  DefaultX := 0;
  DefaultY := 0;
  DefaultZ := 0;
end;

procedure TOrientationSensorForm.OrientationChanged(const Sender: TObject; const Msg: TMessage);
var
 NewScreenOrientation: TScreenOrientation;
begin
   NewScreenOrientation := GetScreenOrientation;

   if (NewScreenOrientation=TScreenOrientation.Portrait) AND (ScreenOrientation=TScreenOrientation.LandScape) then
    begin
    end
   else
   if (NewScreenOrientation=TScreenOrientation.LandScape) AND (ScreenOrientation=TScreenOrientation.Portrait) then
    begin
    end;
end;

procedure TOrientationSensorForm.FormCreate(Sender: TObject);
begin
  ScreenOrientation := GetScreenOrientation;
  OrientationChangedId := TMessageManager.DefaultManager.SubscribeToMessage(TOrientationChangedMessage, OrientationChanged);
end;

function TOrientationSensorForm.GetScreenOrientation: TScreenOrientation;
begin
  Result := IFMXScreenService(TPlatformServices.Current.GetPlatformService(IFMXScreenService)).GetScreenOrientation;
end;


end.
