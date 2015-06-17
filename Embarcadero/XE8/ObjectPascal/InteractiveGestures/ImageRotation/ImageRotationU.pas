unit ImageRotationU;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.StdCtrls,
  FMX.Gestures;

type
  TImageRotationForm = class(TForm)
    Image1: TImage;
    GestureManager1: TGestureManager;
    ToolBar1: TToolBar;
    Label1: TLabel;
    procedure FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo;
      var Handled: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FLastAngle: Double;
  ImageRotationForm: TImageRotationForm;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

procedure TImageRotationForm.FormGesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
var
  LObj: IControl;
  LImage: TImage;

begin
  if EventInfo.GestureID = igiRotate then
  begin
    LObj := Self.ObjectAtPoint(ClientToScreen(EventInfo.Location));
    if LObj is TImage then
    begin
      { rotate the image }
      LImage := TImage(LObj.GetObject);
      if (TInteractiveGestureFlag.gfBegin in EventInfo.Flags) then
        FLastAngle := LImage.RotationAngle
      else if EventInfo.Angle <> 0 then
        LImage.RotationAngle := FLastAngle - (EventInfo.Angle * 180) / Pi;
    end;
  end;
end;

end.
