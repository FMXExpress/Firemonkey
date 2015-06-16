
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects;

type
  TForm7 = class(TForm)
    Image1: TImage;
    FPSTimer: TTimer;
    RotationTimer: TTimer;
    procedure FPSTimerTimer(Sender: TObject);
    procedure RotationTimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure AppIdle(Sender: TObject; var Done: Boolean);
  public
    { Public declarations }
  end;

var
  Form7: TForm7;

implementation

{$R *.fmx}

var
  Angle : Single = 0;
  FPS : Integer = 0;
  PrevTime : Double = 0;
  CurrTime : Double;
  DT, V : Double;

procedure TForm7.AppIdle(Sender: TObject; var Done: Boolean);
begin
  CurrTime := GetTime;
  if PrevTime <> 0 then
    DT := CurrTime-PrevTime;
  PrevTime := CurrTime;

  V := 4000000.0 * DT;

  Angle := Angle+0.5*V;
  if Angle >= 360 then
    Angle := 0;
  Image1.RotationAngle := Angle;
  Inc(FPS);
end;

procedure TForm7.FormCreate(Sender: TObject);
begin
  Application.OnIdle := AppIdle;
end;

procedure TForm7.FPSTimerTimer(Sender: TObject);
begin
  Caption := IntToStr(Round(FPS/FPSTimer.Interval*1000))+' fps';
  FPS := 0;
end;

procedure TForm7.RotationTimerTimer(Sender: TObject);
var
  Done : Boolean;
begin
  AppIdle(Self,Done); // MacOSX
end;

end.
