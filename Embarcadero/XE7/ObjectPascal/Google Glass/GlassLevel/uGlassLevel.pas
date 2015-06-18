// This software is Copyright (c) 2014 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of one of Embarcadero's developer tools products.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

unit uGlassLevel;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Sensors,
  FMX.StdCtrls, FMX.Objects, FMX.Ani, Generics.Collections,
  FMX.Platform, System.Sensors.Components

{$IFDEF Android}
, Androidapi.JNI.PowerManager
{$ENDIF}
;

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
    Timer1: TTimer;
    procedure MotionSensor1DataChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FloatAnimationXFinish(Sender: TObject);
    procedure FloatAnimationZFinish(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FXSample: TList<Single>;
    FZSample: TList<Single>;
    const SampleCount = 20;
    procedure UpdateZ;
    procedure UpdateX;

  public
    { Public declarations }
    function HandleAppEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}
{$R *.GGlass.fmx ANDROID}

function Average(collection: TList<Single>): Single;
var
  sample: Single;
begin
  result := 0;
  if collection.Count = 0 then exit;
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
  aFMXApplicationEventService: IFMXApplicationEventService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXApplicationEventService, IInterface(aFMXApplicationEventService)) then
    aFMXApplicationEventService.SetApplicationEventHandler(HandleAppEvent);

  FXSample := TList<Single>.Create;
  FZSample := TList<Single>.Create;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
{$IFDEF Android}
  ReleaseWakeLock;
{$ENDIF}
end;

function TForm2.HandleAppEvent(AAppEvent: TApplicationEvent;
  AContext: TObject): Boolean;
begin
{$IFDEF Android}
  Result := True;
  case AAppEvent of
    TApplicationEvent.FinishedLaunching: AcquireWakeLock;
    TApplicationEvent.BecameActive: AcquireWakeLock;
    TApplicationEvent.WillBecomeInactive: ReleaseWakeLock;
    TApplicationEvent.EnteredBackground: ReleaseWakeLock;
    TApplicationEvent.WillBecomeForeground: AcquireWakeLock;
    TApplicationEvent.WillTerminate: ReleaseWakeLock;
  else
    Result := False;
  end;
{$ENDIF}
end;

procedure TForm2.UpdateX();
var
  x: Single;
begin
  if not FloatAnimationX.Running then
  begin
    x := Average(FXSample) * -90;
    FXSample.Clear;
    FloatAnimationX.Enabled := False;
    LabelX.Text := Format('L/R: %6.2f ', [X]);
    FloatAnimationX.StopValue := X;
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
    LabelZ.Text := Format('U/D: %6.2f ', [Z * 100]);
    FloatAnimationZ.Enabled := False;
    FloatAnimationZ.StopValue := LineReference.Position.Y + (Z * (Height));
    if FloatAnimationZ.StopValue < 0 then
      FloatAnimationZ.StopValue := 0
    else if FloatAnimationZ.StopValue > Height - 5 then
         FloatAnimationZ.StopValue := Height - 5;
    FloatAnimationZ.Enabled := True;
  end;
end;

procedure TForm2.MotionSensor1DataChanged(Sender: TObject);
var
  LProp: TCustomMotionSensor.TProperty;
  x, z: Single;
begin
  x := 0;
  z := 0;

  if Assigned(MotionSensor1.Sensor) then
  begin
    for LProp in MotionSensor1.Sensor.AvailableProperties do
    begin
      { get the data from the sensor }
      case LProp of
        TCustomMotionSensor.TProperty.AccelerationX:
        begin
          x := MotionSensor1.Sensor.AccelerationX;
        end;
        TCustomMotionSensor.TProperty.AccelerationZ:
        begin
          z := MotionSensor1.Sensor.AccelerationZ;
        end;
      end;
    end;
  end;

  FXSample.Add(X);
  FZSample.Add(Z);
end;

end.
