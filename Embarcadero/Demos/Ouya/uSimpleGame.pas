unit uSimpleGame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Ani, FMX.Layouts;

type
  TForm5 = class(TForm)
    ship: TLabel;
    target: TLabel;
    Label1: TLabel;
    ShipX: TFloatAnimation;
    ShipY: TFloatAnimation;
    Label2: TLabel;
    TargetX: TFloatAnimation;
    TargetY: TFloatAnimation;
    StyleBook1: TStyleBook;
    lblScore: TLabel;
    playArea: TLayout;
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure ShipXFinish(Sender: TObject);
  private
    { Private declarations }
    procedure MoveLeft;
    procedure MoveRight;
    procedure MoveUp;
    procedure MoveDown;
    procedure HitCheck;
    const fMove = 20;
    var fScore: Integer;
  public
    { Public declarations }
  end;

var
  Form5: TForm5;

implementation

{$R *.fmx}

procedure TForm5.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  case Key of
    37: MoveLeft;
    38: MoveUp;
    39: MoveRight;
    40: MoveDown;
  end;

  label1.Text := 'Key: ' + Key.ToString;
end;

procedure TForm5.HitCheck;
var
  shipPt, targetPt: TPointF;
  distance: Integer;
begin
  shipPt := ship.Position.Point;
  targetPt := target.Position.Point;
  distance := round(shipPt.Distance(targetPt));

  label2.Text := 'Dist: ' + round(distance).ToString;

  if distance < fMove then
  begin
    targetX.StartValue := target.Position.X;
    targetY.StartValue := target.Position.Y;
    targetX.StopValue := Random(round(playArea.width));
    targetY.StopValue := Random(round(playArea.height));
    targetX.Start;
    targetY.Start;
    label2.Text := 'Hit!';
    fScore := fScore + 1;
    lblScore.Text := 'Score: ' + fScore.ToString;
  end;
end;

procedure TForm5.MoveDown;
begin
  ship.Text := '\/';
  if not ShipY.Running then
  begin
    ShipY.StopValue := ShipY.StopValue + fMove;
    ShipY.Start;
  end;
end;

procedure TForm5.MoveLeft;
begin
  ship.Text := '<';
  if not Shipx.Running then
  begin
    shipX.StopValue := Shipx.StopValue - fMove;
    shipX.Start;
  end;
end;

procedure TForm5.MoveRight;
begin
  ship.Text := '>';
  if not Shipx.Running then
  begin
    shipX.StopValue := Shipx.StopValue + fMove;
    shipX.Start;
  end;
end;

procedure TForm5.MoveUp;
begin
  ship.Text := '/\';
  if not ShipY.Running then
  begin
    ShipY.StopValue := ShipY.StopValue - fMove;
    ShipY.Start;
  end;

end;

procedure TForm5.ShipXFinish(Sender: TObject);
begin
  HitCheck;
end;

end.
