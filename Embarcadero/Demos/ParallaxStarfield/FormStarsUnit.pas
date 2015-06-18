unit FormStarsUnit;

interface

uses
  SysUtils, Types, UITypes, Classes, Variants, FMX_Types, FMX_Controls, FMX_Forms,
  FMX_Dialogs, AllStars;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure FormResize(Sender: TObject);
  private
    FApp: TStarsApp;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FApp := TStarsApp.Create(self.Width, self.Height);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FApp.Free;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Invalidate;
end;

procedure TForm1.FormPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin
  FApp.Update;
  FApp.Draw(Canvas);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  FApp.ResizeView(self.Width, self.Height);
end;

end.
