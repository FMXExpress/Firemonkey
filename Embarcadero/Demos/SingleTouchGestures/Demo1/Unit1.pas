unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Actions,
  FMX.ActnList, FMX.Objects, FMX.Gestures;

type
  TForm1 = class(TForm)
    GestureManager1: TGestureManager;
    ActionList1: TActionList;
    Rectangle1: TRectangle;
    Action1: TAction;
    procedure Action1Execute(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Action1Execute(Sender: TObject);
begin
  showmessage('Left Gesture');
end;

end.
