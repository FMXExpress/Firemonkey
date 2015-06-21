unit uGame3D;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms3D, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Effects, FMX.Objects, FMX.Filter.Effects, FMX.StdCtrls, FMX.Ani,
  FMX.Layouts, FMX.Controls3D, FMX.Layers3D, FMX.Platform, System.IOUtils, System.IniFiles, Math;

type
  TGameForm3D = class(TForm3D)
    BackGroundImage: TImage;
    BigPipe: TLayout;
    TopPipe: TRectangle;
    TopPipeCap: TRectangle;
    BirdSprite: TImage;
    FMonkeyA: TImage;
    FMonkeyB: TImage;
    GameLoop: TTimer;
    GameOverLayout: TLayout;
    GameOverLBL: TLabel;
    GOFloat: TFloatAnimation;
    Rectangle3: TRectangle;
    Label2: TLabel;
    Label4: TLabel;
    GOScoreLBL: TLabel;
    BestScoreLBL: TLabel;
    Layout1: TLayout;
    OKBTN: TButton;
    ReplayBTN: TButton;
    GetReadyLayout: TLayout;
    Image2: TImage;
    MonochromeEffect1: TMonochromeEffect;
    Rectangle1: TRectangle;
    Label1: TLabel;
    Arc1: TArc;
    Arc2: TArc;
    Rectangle2: TRectangle;
    Pie1: TPie;
    GetReadyLBL: TLabel;
    FloatAnimation1: TFloatAnimation;
    GlowEffect3: TGlowEffect;
    Ground: TRectangle;
    GroundA: TImage;
    GroundB: TImage;
    GroundBar: TImage;
    ScoreLBL: TLabel;
    GlowEffect1: TGlowEffect;
    Layer3D: TLayer3D;
    procedure GameLoopTimer(Sender: TObject);
    procedure Form3DDestroy(Sender: TObject);
    procedure Form3DMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure GetReadyLayoutClick(Sender: TObject);
    procedure ReplayBTNClick(Sender: TObject);
    procedure OKBTNClick(Sender: TObject);
    procedure Form3DCreate(Sender: TObject);
    procedure Form3DHide(Sender: TObject);
    procedure Form3DShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    EnemyList: TStringList;
    GameTick: Integer;
    Score: Integer;
    GameLoopActive: Boolean;
    IsGameOver: Boolean;
    MonkeyAni: Boolean;
    GroundAni: Boolean;
    IniFileName: String;
    BestScore: Integer;
    JustOnce: Boolean;
    BG_WRatio: Integer;
    BG_HRatio: Integer;
    procedure ResetGame;
    procedure SetScore(I: Integer);
    procedure GameOver;
    function CheckBoundryCollision( R1, R2 : TRect; OffSetY : LongInt = 4; OffSetX : LongInt = 4): Boolean; {overload;}
    function GetScreenScale: Single;
  end;
  const
    BG_WIDTH=384;
    BG_HEIGHT=576;

var
  GameForm3D: TGameForm3D;

implementation

{$R *.fmx}

uses uMenu;

function TGameForm3D.GetScreenScale: Single;
var
  ScreenSvc: IFMXScreenService;
begin
  Result := 1;
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, IInterface(ScreenSvc)) then
  begin
    Result := ScreenSvc.GetScreenScale;
  end;
end;


procedure TGameForm3D.Form3DCreate(Sender: TObject);
begin
EnemyList := TStringList.Create;
GOFloat.StopValue := 0;
GOFloat.StartValue := GameForm3D.Height;
GameOverLayout.Position.Y := GameForm3D.Height;
IniFileName := System.IOUtils.TPath.GetDocumentsPath + System.SysUtils.PathDelim + 'Scores.dat';
end;

procedure TGameForm3D.Form3DDestroy(Sender: TObject);
begin
EnemyList.Free;

end;

procedure TGameForm3D.Form3DHide(Sender: TObject);
begin
GameLoop.Enabled := False;
MenuForm.Show;
end;

procedure TGameForm3D.Form3DMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
if GameLoopActive=True then
 begin
  if BirdSprite.Position.Y<75 then
    begin
     BirdSprite.Position.Y := 0;
    end
  else
    begin
      BirdSprite.Position.Y := BirdSprite.Position.Y-75;
    end;
    BirdSprite.RotationAngle := 0;
 end;
end;

procedure TGameForm3D.Form3DShow(Sender: TObject);
var
IniFile: TMemIniFile;
begin
if JustOnce=False then
 begin
   try
     IniFile := TMemIniFile.Create(IniFileName);
     BestScore := IniFile.ReadInteger('Settings','Best',0);
     IniFile.Free;
     BestScoreLBL.Text := IntToStr(BestScore);
   except on E: Exception do
    begin
      //E.Message;
    end;
   end;
   JustOnce := True;
 end;

end;

procedure TGameForm3D.ReplayBTNClick(Sender: TObject);
begin
GameOverLayout.Position.Y := GameForm3D.Height;
GameForm3D.ResetGame;
end;

procedure TGameForm3D.ResetGame;
var
I: Integer;
R: TLayout;
begin
  IsGameOver := False;
  MonkeyAni := False;
  GameOverLayout.Visible := False;
  BirdSprite.Position.X := 96;
  BirdSprite.Position.Y := 248;
  BirdSprite.RotationAngle := 0;
  GetReadyLayout.Visible := True;
  SetScore(0);

    for I := EnemyList.Count-1 downto 0 do
      begin
        if Assigned(EnemyList.Objects[I]) then
         begin
            R := TLayout(EnemyList.Objects[I]);
            R.DisposeOf;
            EnemyList.Delete(I);
         end;
      end;

  Application.ProcessMessages;
  GameLoop.Enabled := True;
end;

procedure TGameForm3D.GameOver;
var
IniFile: TMemIniFile;
begin
IsGameOver := True;
GameLoopActive := False;
GameOverLayout.BringToFront;
GameOverLayout.Visible := True;
ScoreLBL.Visible := False;
//GOFloat.Enabled := True;

GOScoreLBL.Text := IntToStr(Score);

if Score>BestScore then
 begin
  BestScore := Score;
  BestScoreLBL.Text := IntToStr(Score);
   try
     IniFile := TMemIniFile.Create(IniFileName);
     IniFile.WriteInteger('Settings','Best',BestScore);
     IniFile.Free;
   except on E: Exception do
    begin
      //E.Message;
    end;
   end;
 end;

end;

procedure TGameForm3D.GetReadyLayoutClick(Sender: TObject);
var
FormScale: Single;
begin
FormScale := GetScreenScale;
BG_WRatio := Trunc(FormScale);
BG_HRatio := Trunc(FormScale);
GetReadyLayout.Visible := False;
ScoreLBL.Visible := True;
GameLoopActive := True;
end;

procedure TGameForm3D.OKBTNClick(Sender: TObject);
begin
if IsGameOver=True then
 begin
   GameOverLayout.Position.Y := GameForm3D.Height;
   GameOverLayout.Visible := False;
   GameForm3D.Close;
   MenuForm.Show;
 end;
end;

procedure TGameForm3D.SetScore(I: Integer);
begin
Score := I;
ScoreLBL.Text := IntToStr(Score);
end;

procedure TGameForm3D.GameLoopTimer(Sender: TObject);
const
SpawnTime = 1.5;
var
R: TLayout;
I: Integer;
POffset,WOffset: Integer;
R1, R2: TRect;
GameOverCheck: Boolean;
begin

//GameForm3D.BeginUpdate;

if GameLoopActive=True then
 begin
    GameOverCheck := False;
    if (BirdSprite.Position.Y+BirdSprite.Height)>Ground.Position.Y then
     begin
       GameOverCheck := True;
     end;


    if GameTick=0 then
     begin
          if (Random<0.5) then
           begin
             WOffset := (200*BG_HRatio);
           end
           else
           begin
             WOffset := (250*BG_HRatio);
           end;

          if (Random<0.5) then
           begin
             POffset := (-125*BG_HRatio);
           end
           else
           begin
             POffset := (-25*BG_HRatio);
           end;


          R := TLayout(BigPipe.Clone(Self));
          EnemyList.AddObject('',R);
          R.Parent := Layer3D;//GameForm3D;
          R.Visible := True;
          R.Position.X := GameForm3D.Width+R.Width;
          R.Position.Y := (0-WOffset)+POffset;
          R.Tag := 1;

          R := TLayout(BigPipe.Clone(Self));
          EnemyList.AddObject('',R);
          R.Parent := Layer3D;//GameForm3D;
          R.Visible := True;
          R.Position.X := GameForm3D.Width+R.Width;
          R.Position.Y := (GameForm3D.Height-R.Height+WOffset)+POffset;
          R.RotationAngle := 180;
          R.Tag := 2;
     end;


   if GameTick>(SpawnTime*30) then
     begin
        GameTick := 0;
     end
   else
     begin
      Inc(GameTick);
     end;

    R1 := Rect(Trunc(BirdSprite.Position.X),Trunc(BirdSprite.Position.Y),Trunc(BirdSprite.Position.X+BirdSprite.Width),Trunc(BirdSprite.Position.Y+BirdSprite.Height));

    for I := EnemyList.Count-1 downto 0 do
      begin
        if Assigned(EnemyList.Objects[I]) then
         begin
            R := TLayout(EnemyList.Objects[I]);
            R.Position.X := R.Position.X-5;

            R2 := Rect(Trunc(R.Position.X),Trunc(R.Position.Y),Trunc(R.Position.X+R.Width),Trunc(R.Position.Y+R.Height));

            if CheckBoundryCollision(R1,R2)=True then
             begin
               GameOverCheck := True;
             end
            else
             begin

              if (((R.Position.X+(R.Width/2))<BirdSprite.Position.X) AND (R.Tag=1) AND (R.TagFloat=0)) then
               begin
                R.TagFloat := 1;
                SetScore(Score+1);
               end;

             end;

            if (R.Position.X<((R.Width*-1)-10)) then
             begin
               R.DisposeOf;
               EnemyList.Delete(I);
             end;

         end;
      end;

    if BirdSprite.RotationAngle<90 then
      BirdSprite.RotationAngle := BirdSprite.RotationAngle+5;

   BirdSprite.Position.Y := BirdSprite.Position.Y+(Max(BirdSprite.RotationAngle,1)/5);

   if MonkeyAni=False then
       begin
         BirdSprite.Bitmap.Assign(FMonkeyB.Bitmap);
         MonkeyAni := True;
       end
     else
       begin
         BirdSprite.Bitmap.Assign(FMonkeyA.Bitmap);
         MonkeyAni := False;
       end;

       if GameOverCheck=True then
         GameOver;
 end;

   Ground.BringToFront;
   ScoreLBL.BringToFront;
   if GroundAni=True then
    begin
     GroundBar.Bitmap.Assign(GroundB.Bitmap);
     GroundAni := False;
     GroundBar.BringToFront;
    end
    else
    begin
     GroundBar.Bitmap.Assign(GroundA.Bitmap);
     GroundAni := True;
     GroundBar.BringToFront;
    end;
   if IsGameOver then
     GameOverLayout.BringToFront;

//  GameForm3D.EndUpdate;

end;


{function CheckCollision(x1, y1, x2, y2, enemy_height, enemy_width: Integer): Boolean;
const
  OFFSET_X = 4;
  OFFSET_Y = 4;
begin
  Result := True;  }

{  if ((
  (y1 + PLAYER_HEIGHT - (OFFSET_Y * 2)
  <
  (y1 + OFFSET_Y  and y2 + ENEMY_HEIGHT - (OFFSET_Y * 2)
  )
  or
     (
     (x1 + PLAYER_WIDTH - (OFFSET_X * 2)
     and
     (x1 + OFFSET_X  and x2 + ENEMY_WIDTH - (OFFSET_X * 2)
     )) then
      Result := False;}
//end;

function TGameForm3D.CheckBoundryCollision( R1, R2 : TRect; OffSetY : LongInt = 4; OffSetX : LongInt = 4): Boolean;
 // Simple collision test based on rectangles.

begin
   // Rectangle R1 can be the rectangle of the character (player)
   // Rectangle R2 can be the rectangle of the enemy

        // Simple collision test based on rectangles. We can also use the
        // API function IntersectRect() but i believe this is much faster.
        Result:=( NOT ((R1.Bottom - (OffSetY * 2) < R2.Top + OffSetY)
          or(R1.Top + OffSetY > R2.Bottom - (OffSetY * 2))
           or( R1.Right - (OffSetX * 2) < R2.Left + OffSetX)
            or( R1.Left + OffSetX > R2.Right - (OffSetX * 2))));
end;


end.
