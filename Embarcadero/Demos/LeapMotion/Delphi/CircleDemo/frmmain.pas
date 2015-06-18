unit frmmain;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics,
  leapdata, wsleap, Dialogs, ExtCtrls, jpeg;

type

  { TForm1 }

  TForm1 = class(TForm)
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    FIndex,
    FLastID : Integer;
    FLastProgress : TFloat;
    FImages : Array [0..3] of TJPegImage;
    FController : TWebSocketLeapController;
    procedure DoFrame(Sender: TObject; AFrame: TFrame);
    procedure RotateImage(Progress: TFloat; Reset: Boolean);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);

Const
   FN : Array[0..3] of String
      = ('image','image-90','image-180','image-270');
Var
  I : Integer;

begin
  For I:=0 to 3 do
    begin
    FImages[I]:=TJpegImage.Create;
    FImages[i].LoadFromFile(FN[i]+'.jpg');
    end;
  Image1.Picture.Graphic:=FImages[0];//Assign(FImages[0]);
  FIndex:=0;
  FController:=TWebSocketLeapController.Create(Self);
  FController.OnFrame:=DoFrame;
  FController.Enabled:=True;
  FController.EnableGestures:=True;
end;

procedure TForm1.FormDestroy(Sender: TObject);
Var
  I : Integer;

begin
  For I:=0 to 3 do
    FImages[I].Free;
end;

procedure TForm1.DoFrame(Sender: TObject; AFrame: TFrame);

Var
  I : Integer;
  C,CID : TCircleGesture;
  P : TFLoat;
  IsNew : Boolean;

begin
  if (AFrame.Gestures.Count=0) then exit;
  C:=nil;
  CID:=Nil;
  for I:=0 to AFrame.Gestures.Count-1 do
    if (AFrame.Gestures[i] is TCircleGesture) then
      begin
      C:=AFrame.Gestures[i] as TCircleGesture;
      if (C.ID=FLastID) then
        CID:=C;
      end;
  If (C=Nil) then exit;
  IsNew:=(CID=Nil);
  if Not IsNew then
    C:=CID;
  FLastID:=C.ID;
  P:=C.Progress;
  If (C.Pointables.Count>0)
     and Assigned(C.Pointables[0]) then
     begin
     if (AngleTo(C.Pointables[0].Direction,C.Normal)<=Pi/4) then
     // Clockwise, invert progress
      P:=-P;
     end;
  RotateImage(P,IsNew);
end;

procedure TForm1.RotateImage(Progress : TFloat; Reset : Boolean);

Var
  J : integer;

begin
  if Reset then
    FLastProgress:=0;
  J:=Trunc((Progress-FLastProgress)*4);
  If (J<>0) then
    begin
    FLastProgress:=Progress;
    FIndex:=Abs((FIndex+J) mod 4);
    Image1.Picture.Graphic:=FImages[FIndex];
    end;
end;

end.

