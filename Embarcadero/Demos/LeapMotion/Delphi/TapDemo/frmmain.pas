unit frmmain;

{$ifdef fpc}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, Forms, ExtCtrls, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, leapdata, wsleap, ComCtrls, Spin, types, jpeg;

type
  TImageState = (isNormal,isHover,isTap,isTap2);
  { TMainForm }

  TMainForm = class(TForm)
    IFinger: TImage;
    Ltitle: TLabel;
    Label2: TLabel;
    SBTap: TStatusBar;
    TTap: TTimer;
    SEDelay: TSpinEdit;
    procedure DoClick(Sender: TObject);
    procedure DoFrame(Sender: TObject; AFrame: TFrame);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TTapTimer(Sender: TObject);
  private
    procedure DoDeleteProduct(Sender: TObject);
    procedure StartTapTimer;
    procedure InitTap;
    { private declarations }
  public
    FH,FW : Integer;
    FController : TWebSocketLeapController;
    FTapState : TImageState;
    { public declarations }
    FLastIndex : Integer;
    FTapImages : Array[0..15,TImageState] of TJpegImage;
    FButtons  : Array[0..15] of TImage;
    FLastButton : TImage;
  end;

var
  MainForm: TMainForm;

implementation

Const
  FProducts : Array[0..15] of String =
    ('Apples','Bananas','Kiwis','Pears','Pineapples','Star fruits','Strawberries','Yuccas',
     'Broccoli','Zucchini','Eggplant','Cabbage','lettuce','mushroom','ognions','tomato');
  FImageFiles : Array[0..15] of String =
    ('apple','banana','kiwi','pear','pineapple','star_fruit','strawberry','yucca',
     'broccoli','courgette','eggplant','cabbage','lettuce','mushroom','ognions','tomato');
  ITop  = 48;
  ILeft = 24;
  BW = 130;
  BH = 150;
  BHS = 24;
  BVS = 14;

{$R *.dfm}

{ TMainForm }

procedure TMainForm.InitTap;

Var
  B : TImage;
  I : Integer;

begin
  Fh:=IFinger.Height div 2;
  FW:=IFinger.Width div 2;
  Width:=ILeft+4*(BW+BHS);
  height:=ITop+4*(BH+BVS);
  self.Color:=clWhite;
  For I:=0 to 15 do
    begin
    B:=TImage.Create(Self);
    FTapImages[I,isNormal]:=TJpegImage.Create;
    FTapImages[I,isHover]:=TJpegImage.Create;
    FTapImages[I,isTap]:=TJpegImage.Create;
    FTapImages[I,isTap2]:=TJpegImage.Create;
    if FileExists(FImageFiles[i]+'_normal.jpeg') then
      begin
      FTapImages[I,isNormal].LoadFromFile(FImageFiles[i]+'_normal.jpeg');
      FTapImages[I,isHover].LoadFromFile(FImageFiles[i]+'_hover.jpeg');
      FTapImages[I,isTap].LoadFromFile(FImageFiles[i]+'_tap.jpeg');
      FTapImages[I,isTap2].LoadFromFile(FImageFiles[i]+'_tap2.jpeg');
      end
    else
      begin
      FTapImages[I,isNormal].LoadFromFile(FImageFiles[i]+'.jpeg');
      FTapImages[I,isHover].LoadFromFile(FImageFiles[i]+'.jpeg');
      FTapImages[I,isTap].LoadFromFile(FImageFiles[i]+'.jpeg');
      FTapImages[I,isTap2].LoadFromFile(FImageFiles[i]+'.jpeg');
      end;
    B.Picture.Assign(FTapImages[I,isNormal]);
    B.Parent:=Self;
    B.Width:=BW;
    B.Height:=BH;
    B.Tag:=I;
    B.Top:=ITop+(I div 4)*(BH+BVS);
    B.Left:=ILeft+(I mod 4)*(BW+BHS);
    B.OnClick:=DoClick;
    B.Transparent:=True;
    FButtons[i]:=B;
    end;
end;

procedure TMainForm.FormCreate(Sender: TObject);

begin
  FController:=TWebSocketLeapController.Create(Self);
  FController.OnFrame:=DoFrame;
  FController.Enabled:=True;
  FController.EnableGestures:=True;
  InitTap;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
Var
  I : Integer;
  S : TImageState;

begin
  For I:=0 to 15 do
    For S:=Low(TImageState) to High(TImageState) do
      FTapImages[I,S].Free;
end;

procedure TMainForm.DoDeleteProduct(Sender: TObject);

begin
  TTap.Enabled:=False;
  LTitle.Caption:='Choose your product';
end;

procedure TMainForm.TTapTimer(Sender: TObject);
begin
  FLastButton.Picture.Assign(FTapImages[FLastIndex,FTapState]);
  Case FTapState of
   isTap2 : FTapState:=isHover;
   isHover : FTapState:=isNormal;
  end;
  if FTapState=isNormal then
    begin
    LTitle.Caption:='You chose for '+FProducts[FlastButton.Tag];
    TTap.Interval:=2000;
    TTap.OnTimer:=DoDeleteProduct;
    end;
end;

procedure TMainForm.DoClick(Sender: TObject);
begin
  SBTap.SimplePanel:=True;
  SBTap.SimpleText:='You chose for '+FProducts[(Sender as TComponent).Tag];
end;

procedure TMainForm.StartTapTimer;

begin
  FLastButton.Picture.Assign(FTapImages[FLastIndex,isTap]);
  DoClick(FLastButton); //.Click;
  TTap.Interval:=Round(SEDelay.Value*100);
  FTapState:=isTap2;
  TTap.OnTimer:=TTapTimer;
  TTap.Enabled:=True;
end;




procedure TMainForm.DoFrame(Sender: TObject; AFrame: TFrame);

Var
  P,P2 : TPointable;
  V : T3DVector;
  Pt : TPoint;
  I : Integer;
  G : TGesture;

begin
  if AFrame.Pointables.Count=0 then exit;
  Caption:='Frame + '+IntToStr(AFrame.ID);
  P:=AFrame.Pointables[0];
  For I:=1 to AFrame.Pointables.Count-1 do
    begin
    P2:=AFrame.Pointables[i];
    If P.TipPosition.Z>P2.tipPosition.Z then
      P:=P2;
    end;
  V:=AFrame.InteractionBox.Normalize(P.tipPosition);
  Pt.X:=Round(ILeft+V.X*(4*(BW+BHS)));
  PT.Y:=Round(ITop+(1-V.Y)*(4*(BH+BVS)));
  IFinger.Visible:=PtInRect(Rect(FW,FH,Width-FW,Height-FH),Pt);
  if IFinger.Visible then
    begin
    IFinger.BringToFront;
    IFinger.SetBounds(Pt.X-FW,PT.Y-FH,FW*2,FH*2);
    end;
  I:=15;
  While (I>=0) and Not (PtInRect(FButtons[i].BoundsRect,Pt)) do
    Dec(I);
//  Writeln(P.tipPosition.X,',',P.tipPosition.Y,'->',V.X:5:2,',',V.Y:5:2,' -> ',pt.x,',',pt.y,' -> ',i);
  if (I>=0) then
    begin
    if Assigned(FLastButton) and (FButtons[i]<>FlastButton) then
      FLastButton.Picture.Assign(FTapImages[FLastIndex,isNormal]);
    FLastIndex:=I;
    if (FLastButton<>FButtons[i]) then
      begin
      FLastButton:=FButtons[i];
      FLastButton.Picture.Assign(FTapImages[i,isHover]);
      end;
    end;
  if FTapState<>isNormal then
     exit;
  if (FLastButton=Nil) or (AFrame.Gestures.Count=0) then exit;
  for I:=0 to AFrame.Gestures.Count-1 do
    begin
    G:=AFrame.Gestures[i];
    if (AFrame.Gestures[i] is TTapGesture) then
      StartTapTimer;
    end;
end;

end.

