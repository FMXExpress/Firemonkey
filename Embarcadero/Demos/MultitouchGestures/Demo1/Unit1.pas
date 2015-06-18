unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects;

type
  TForm1 = class(TForm)
    Image1: TImage;
    procedure Image1Gesture(Sender: TObject; const EventInfo: TGestureEventInfo;
      var Handled: Boolean);
  private
    { Déclarations privées }
    LastAngle:Double;
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Image1Gesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  if  EventInfo.GestureID=igiRotate then
  begin
    if (TInteractiveGestureFlag.gfBegin in EventInfo.Flags) then
      LastAngle:=Image1.RotationAngle
    else
    begin
      if EventInfo.Angle<>0 then
      begin
        Image1.RotationAngle:=LastAngle - (EventInfo.Angle*180) / Pi;
      end;
    end;
  end;
end;

end.
