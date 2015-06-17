unit LifeUnitFmx;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Layouts, FMX.StdCtrls, LifeEngine;

const
  BoardSize: TSize = (cx: 500; cy: 500);

type
  TLifeForm = class(TForm)
    OpenDialog1: TOpenDialog;
    PaintBox1: TPaintBox;
    HorzScrollBar: TScrollBar;
    VertScrollBar: TScrollBar;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
    procedure VertScrollBarChange(Sender: TObject);
    procedure HorzScrollBarChange(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
  private
    var
      FLifeEngine: TLifeEngine;
      FLifeBoard: TLifeBoard;
      FGensPerSecond, FMaxGensPerSecond: Double;
      FViewOffset, FViewSize: TPoint;
    procedure LifeEngineUpdate(Sender: TObject);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  LifeForm: TLifeForm;

implementation

uses
  System.Math;

{$R *.fmx}

{ TLifeForm }

procedure TLifeForm.Button1Click(Sender: TObject);
begin
  if not FLifeEngine.Running then
  begin
    FLifeEngine.Start;
    Button1.Text := 'Stop';
  end else
  begin
    FLifeEngine.Stop;
    Button1.Text := 'Start';
  end;
end;

procedure TLifeForm.Button2Click(Sender: TObject);
begin
  if not FLifeEngine.Running then
  begin
    FLifeEngine.Clear;
    FLifeBoard := FLifeEngine.LifeBoard;
    FormResize(Sender);
    PaintBox1.InvalidateRect(PaintBox1.BoundsRect);
  end;
end;

procedure TLifeForm.Button3Click(Sender: TObject);
begin
  if not FLifeEngine.Running and OpenDialog1.Execute then
  begin
    FLifeEngine.LoadPattern(OpenDialog1.FileName);
  end;
end;

procedure TLifeForm.Button4Click(Sender: TObject);
begin
  HorzScrollBar.Value := (Length(FLifeBoard) - FViewSize.X) div 2;
  VertScrollBar.Value := (Length(FLifeBoard[0]) - FViewSize.Y) div 2;
end;

procedure TLifeForm.CheckBox1Change(Sender: TObject);
begin
  if FLifeEngine <> nil then
    FLifeEngine.Parallel := CheckBox1.IsChecked;
end;

procedure TLifeForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if FLifeEngine.Running then
    Button1Click(Sender);
  CanClose := not FLifeEngine.Running;
end;

procedure TLifeForm.FormCreate(Sender: TObject);
begin
  FLifeEngine := TLifeEngine.Create(BoardSize);
  FLifeEngine.OnUpdate := LifeEngineUpdate;
  FLifeBoard := FLifeEngine.LifeBoard;
  FLifeEngine.UpdateRate := 30;
end;

procedure TLifeForm.FormDestroy(Sender: TObject);
begin
  FLifeEngine.Free;
end;

procedure TLifeForm.FormResize(Sender: TObject);
begin
  FViewSize := Point((Trunc(PaintBox1.Width) - 10) div 10, (Trunc(PaintBox1.Height) - 10) div 10);
  HorzScrollBar.Max := Length(FLifeBoard){ - FViewSize.X};
  VertScrollBar.Max := Length(FLifeBoard[0]){ - FViewSize.Y};
end;

procedure TLifeForm.HorzScrollBarChange(Sender: TObject);
begin
  FViewOffset.X := Trunc(HorzScrollBar.Value);
  PaintBox1.InvalidateRect(PaintBox1.BoundsRect);
end;

procedure TLifeForm.LifeEngineUpdate(Sender: TObject);
begin
  FLifeBoard := FLifeEngine.LifeBoard;
  FGensPerSecond := FLifeEngine.GensPerSecond;
  FMaxGensPerSecond := FLifeEngine.MaxGensPerSecond;
  Label2.Text := Format('%f Generations Per Second', [FGensPerSecond]);
  Label3.Text := Format('%f Max Generations Per Second', [FMaxGensPerSecond]);
  PaintBox1.InvalidateRect(PaintBox1.BoundsRect);
end;

procedure TLifeForm.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  Row, Column: Integer;
begin
  if not FLifeEngine.Running and (Button = TMouseButton.mbLeft) then
  begin
    Row := Trunc(Y) div 10;
    Column := Trunc(X) div 10;
    if (Row >= 0) and (Row <= FViewSize.Y) and
      (Column >= 0) and (Column <= FViewSize.X) then
    begin
      FLifeBoard[FViewOffset.X + Column, FViewOffset.Y + Row] :=
        FLifeBoard[FViewOffset.X + Column, FViewOffset.Y + Row] xor 1;
        PaintBox1.InvalidateRect(TRectF.Create(Column + 10, Row * 10, Column * 10 + 11, Row * 10 + 11));
    end;
    Label1.Text := Format('%d, %d', [FViewOffset.X + Column, FViewOffset.Y + Row]);
  end;
end;

procedure TLifeForm.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
    Label1.Text := Format('%d, %d', [FViewOffset.X + Trunc(X) div 10,
      FViewOffset.Y + Trunc(Y) div 10]);
end;

procedure TLifeForm.PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
var
  I, J: Integer;
  Gens, Max: Integer;
  ScaleTemp: Integer;
begin
  PaintBox1.Canvas.BeginScene();
  PaintBox1.Canvas.Stroke.Kind := TBrushKind.Solid;
  PaintBox1.Canvas.Stroke.Color := TAlphaColorRec.Gray;
  if Length(FLifeBoard) > 0 then
    for I := 0 to FViewSize.X - 1 do
      for J := 0 to FViewSize.Y - 1 do
      begin
        if FLifeBoard[Min(FViewOffset.X + I, High(FLifeBoard)), Min(FViewOffset.Y + J, High(FLifeBoard[0]))] <> 0 then
          with PaintBox1.Canvas do
          begin
            Fill.Color := TAlphaColorRec.Black;
            FillREct(TRectF.Create(I * 10, J * 10, I * 10 + 11, J * 10 + 11), 0, 0, [],100);
          end else
          with PaintBox1.Canvas do
          begin
            Fill.Color := Self.Canvas.Fill.Color;
            DrawRect(TRectF.Create(I * 10, J * 10, I * 10 + 11, J * 10 + 11), 0, 0, [], 100);
          end;
      end;
  with PaintBox1.Canvas do
  begin
    // Calculate the scale for the graph
    ScaleTemp := 1000000;
    while ScaleTemp > 10 do
    begin
      if FMaxGensPerSecond * 10 < ScaleTemp then
        ScaleTemp := ScaleTemp div 10
      else
        break;
    end;
    Gens := Trunc((FGensPerSecond * PaintBox1.Height) / ScaleTemp);
    Max := Trunc((FMaxGensPerSecond * PaintBox1.Height) / ScaleTemp);
    Fill.Color := TAlphaColorRec.Green;
    FillRect(TRectF.Create(PaintBox1.Width - 4, PaintBox1.Height - Gens, PaintBox1.Width,
      PaintBox1.Height), 0,0,[], 100);

    Stroke.Color := TAlphaColorRec.Red;
    DrawLine(TPointF.Create(PaintBox1.Width-4, PaintBox1.Height-Max),
    TPointF.Create(PaintBox1.Width, PaintBox1.Height - Max), 100);
  end;
  PaintBox1.Canvas.EndScene;
end;

procedure TLifeForm.VertScrollBarChange(Sender: TObject);
begin
  FViewOffset.Y := Trunc(VertScrollBar.Value);
//  PaintBox1.Repaint();
  PaintBox1.InvalidateRect(PaintBox1.BoundsRect);
end;

end.
