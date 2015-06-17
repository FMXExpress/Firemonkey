unit TapHoldForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Gestures, FMX.Objects,
  FMX.StdCtrls;

type
  TTapHold = class(TForm)
    GestureManager1: TGestureManager;
    Image1: TImage;
    ToolBar1: TToolBar;
    Title: TLabel;
    procedure FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo;
      var Handled: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  TapHold: TTapHold;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

procedure TTapHold.FormGesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  { if a long tap gesture is detected }
  if EventInfo.GestureID = System.UITypes.igiLongTap then
    { show a message }
    Title.Text := Format('LongTap at %d, %d seen %s',
               [Round(EventInfo.Location.X), Round(EventInfo.Location.Y),
                FormatDateTime('nn:ss',Now)]);
end;

end.
