unit frmmain;


interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, ExtCtrls, leapdata, wsleap,
  Mask;

type

  { TMainForm }

  TMainForm = class(TForm)
    BStart: TButton;
    Bstop: TButton;
    CBPercentual: TCheckBox;
    Label1: TLabel;
    MEFriction: TMaskEdit;
    SGSongs: TStringGrid;
    Timer1: TTimer;
    procedure BStartClick(Sender: TObject);
    procedure BstopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    FList : TStringList;
    procedure DoFrame(Sender: TObject; AFrame: TFrame);
    procedure DoGridScroll;
    procedure LoadSongs;
    procedure StartSwipe(ASpeed, AFriction : Integer; P : Boolean);
  public
    FFriction,
    FDelta : Integer;
    FPercent : Boolean;
    FController:TWebSocketLeapController;
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation


{$R *.dfm}

{ TMainForm }

procedure TMainForm.BStartClick(Sender: TObject);
begin
  FController.Enabled:=True;
end;

procedure TMainForm.BstopClick(Sender: TObject);
begin
  FController.Enabled:=False;
end;

procedure TMainForm.LoadSongs;

Var
  I,J : Integer;
  S : String;

begin
  SGSongs.RowCount:=FList.Count;
  For I:=0 to Flist.Count-1 do
    begin
    S:=Flist[i];
    J:=Pos('#-#',S);
    SGSongs.Cells[0,I]:=Copy(S,1,J-1);
    Delete(S,1,J+2);
    J:=Pos('#-#',S);
    SGSongs.Cells[1,I]:=Copy(S,1,J-1);
    Delete(S,1,J+2);
    SGSongs.Cells[2,I]:=ExtractFileName(S);
    end;
end;

procedure TMainForm.FormCreate(Sender: TObject);

Var
  FN : String;
  I : Integer;

begin
  FController:=TWebSocketLeapController.Create(Self);
  FController.OnFrame:=DoFrame;
  FController.Enabled:=True;
  FController.EnableGestures:=True;
  FN:=ChangeFileExt(Paramstr(0),'.lst');
  if FileExists(FN) then
    begin
    FList:=TStringList.Create;
    FList.LoadFromFile(FN);
    LoadSongs;
    end;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  DoGridScroll;
end;

procedure TMainForm.DoGridScroll;

begin
  SGSongs.TopRow:=SGSongs.TopRow+FDelta;
  if not FPercent then
    begin
    If Ffriction>Abs(FDelta) then
      FFriction:=Abs(FDelta);
    If FDelta<0 Then FDelta:=FDelta+FFriction else FDelta:=FDelta-FFriction;
    end
  else
    FDelta:=Trunc(FDelta*(1-(FFriction/100)));
  If FDelta=0 then
    Timer1.Enabled:=False;
end;


procedure TMainForm.StartSwipe(ASpeed, AFriction : Integer; P : Boolean);

begin
  Timer1.Enabled:=True;
  FFriction:=AFriction;
  FDelta:=ASpeed;
  FPercent:=P;
  DoGridScroll;
end;

procedure TMainForm.DoFrame(Sender: TObject; AFrame: TFrame);

Var
  I,D : Integer;
  G : TGesture;
  S : TSwipeGesture;

begin
  if (AFrame.Gestures.Count=0) then exit;
  for I:=0 to AFrame.Gestures.Count-1 do
    begin
    G:=AFrame.Gestures[i];
    if (G is TSwipeGesture) then
      begin
      S:=G as TSwipeGesture;
      With S.Direction do
        begin
        If Abs(Y)>Abs(X*5) then
          begin
          D:=Round(Y*S.Speed/10);
          StartSwipe(D,StrToIntDef(Trim(MEFriction.Text),1),CBPercentual.Checked);
          end;
        end;
      end;
    end;
end;

end.

