unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Layouts, FMX.Ani;

type
  TForm1 = class(TForm)
    Layout1: TLayout;
    Rectangle1: TRectangle;
    Rectangle2: TRectangle;
    Rectangle3: TRectangle;
    Rectangle4: TRectangle;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Text1: TText;
    procedure FormResize(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Rectangle1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Rectangle1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    pageIndex: Integer;
    tapDownPos: TPointF;
    tapIsDown: Boolean;  // This is needed, since the tap can happend outside control and then moved inside it to release
    procedure ScrollMainView(page: Integer);
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.ScrollMainView(page: Integer);
begin
  pageIndex := page;
  Dec(page);
  TAnimator.AnimateFloat(Layout1, 'Position.X', (ClientWidth * page) * -1, 0.5, TAnimationType.Out, TInterpolationType.Exponential);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ScrollMainView(3);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  ScrollMainView(4);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  ScrollMainView(2);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  ScrollMainView(1);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  pageIndex := 1;
  tapIsDown := false;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  Layout1.Height := ClientHeight;
  Rectangle4.Width := ClientWidth;
  Rectangle3.Width := ClientWidth;
  Rectangle2.Width := ClientWidth;
  Rectangle1.Width := ClientWidth;
end;

procedure TForm1.Rectangle1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  tapDownPos := TPointF.Create(X, Y);
  tapIsDown := true;
end;

procedure TForm1.Rectangle1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if tapIsDown then
  begin
    tapIsDown := false;

    // swipe left
    if (X - tapDownPos.X < -50) and (pageIndex < 4) then
    begin
      Inc(pageIndex);
      //ShowMessage('to right: ' + Format('%f > %d', [X - tapDown.X, pageIndex]));
      ScrollMainView(pageIndex);
    end
    // swipe right
    else if (X - tapDownPos.X > 50) and (pageIndex > 1) then
    begin
      Dec(pageIndex);
      //ShowMessage('to left: ' + Format('%f > %d', [X - tapDown.X, pageIndex]));
      ScrollMainView(pageIndex);
    end;
  end;
end;

end.
