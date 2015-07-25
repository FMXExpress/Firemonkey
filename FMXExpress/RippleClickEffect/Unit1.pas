unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Ani, FMX.Objects, System.Math, duck,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.ListBox, FMX.Layouts;

type
  TForm1 = class(TForm)
    Rectangle1: TRectangle;
    Circle1: TCircle;
    FloatAnimation1: TFloatAnimation;
    Button1: TButton;
    Panel1: TPanel;
    ListBox1: TListBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    ListBoxItem4: TListBoxItem;
    ListBoxItem5: TListBoxItem;
    SpeedButton1: TSpeedButton;
    Text1: TText;
    procedure FloatAnimation1Process(Sender: TObject);
    procedure FloatAnimation1Finish(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FX,FY:Single;
    procedure GlobalMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FloatAnimation1Finish(Sender: TObject);
begin
Circle1.Visible := False;
end;

procedure TForm1.FloatAnimation1Process(Sender: TObject);
begin
Circle1.Width := Circle1.Height;
Circle1.Position.X := FX-(Circle1.Width/2);;
Circle1.Position.Y := FY-(Circle1.Height/2);;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 Self.duck.all.has('OnMouseDown').each(
    procedure(obj: TObject)
    begin
      TControl(obj).ClipChildren := True;
      TControl(obj).OnMouseDown := GlobalMouseDown;
    end
  );
end;

procedure TForm1.GlobalMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
Circle1.Parent := TControl(Sender);
Circle1.Position.X := X-(Circle1.Width/2);
FX := X;
Circle1.Position.Y := Y-(Circle1.Height/2);
FY := Y;
Circle1.Width := 0;
Circle1.Height := 0;
Circle1.Visible := True;
FloatAnimation1.StopValue := Max(TControl(Sender).Width,TControl(Sender).Height)*2;
end;

end.
