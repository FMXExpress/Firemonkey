unit AllStars;

interface

uses
  Types, UITypes, FMX_Types, contnrs;

const
  STARS_COUNT = 150;

type
  TStarItem = class
    Loc: TPointF;
    Speed: double;
    procedure Draw(const c: TCanvas);
  end;

  TStarsApp = class
  private
    FWidth,
    FHeight: double;
    FStars: TObjectList;
  public
    constructor Create(const AWidth, AHeight: double);
    destructor Destroy; override;
    procedure Update;
    procedure Draw(const c: TCanvas);
    procedure ResizeView(const AWidth, AHeight: double);
  end;

implementation

uses
  Classes;

{ TStarsApp }

constructor TStarsApp.Create(const AWidth, AHeight: double);
var i: integer; s: TStarItem;
begin
  FWidth := AWidth;
  FHeight := AHeight;
  FStars := TObjectList.Create;
  for i := 0 to STARS_COUNT-1 do
  begin
    s := TStarItem.Create;
    s.Loc := PointF(random(round(FWidth)), random(round(FHeight)));
    s.Speed := 1 + random(2);
    FStars.Add(s);
  end;
end;

destructor TStarsApp.Destroy;
begin
  FStars.Free;
  inherited;
end;

procedure TStarsApp.Draw(const c: TCanvas);
var i: integer; s: TStarItem;
begin
  c.Clear(claBlack);

  for i := 0 to FStars.Count-1 do
  begin
    s := TStarItem(FStars[i]);
    s.Draw(c);
  end;
end;

procedure TStarsApp.ResizeView(const AWidth, AHeight: double);
var i: integer; s: TStarItem; temp: double;
begin
  FWidth := AWidth;
  FHeight := AHeight;
  for i := 0 to FStars.Count-1 do
  begin
    s := TStarItem(FStars[i]);
    temp := s.Loc.X;
    s.Loc.X := s.Loc.Y;
    s.Loc.Y := temp;
  end;
end;

procedure TStarsApp.Update;
var i: integer; s: TStarItem;
begin
  for i := 0 to FStars.Count-1 do
  begin
    s := TStarItem(FStars[i]);
    s.Loc.Y := s.Loc.Y + s.Speed;
    if s.Loc.Y > FHeight then
      s.Loc := PointF(random(round(FWidth)),0);
  end;
end;

{ TStarItem }

procedure TStarItem.Draw(const c: TCanvas);
begin
  if Speed > 2 then
    c.Stroke.Color := MakeColor(255,255,255)
  else if Speed > 1 then
    c.Stroke.Color := MakeColor(190,190,190)
  else
    c.Stroke.Color := MakeColor(100,100,100);

  c.StrokeThickness := 1;
  c.Stroke.Kind := TBrushKind.bkSolid;
  c.DrawLine(PointF(Loc.X-1, Loc.Y), PointF(Loc.X+1, Loc.Y), 1);
  c.DrawLine(PointF(Loc.X, Loc.Y-1), PointF(Loc.X, Loc.Y+1), 1);
end;

initialization
  Randomize;

end.
