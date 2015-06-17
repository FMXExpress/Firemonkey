unit MobileSelPublishTypeFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  uUIFunction,Math,
  uSkinFireMonkeyControl, uSkinFireMonkeyPanel, uSkinFireMonkeyImage,
  uSkinAnimator, uSkinFireMonkeyButton, uSkinMaterial, uSkinButtonType;

type
  TFrameMobileSelPublishType = class(TFrame)
    pnlBottomBar: TSkinFMXPanel;
    imgClose: TSkinFMXImage;
    aniImgCloseRotate: TSkinAnimator;
    imgBack: TSkinFMXImage;
    btnText: TSkinFMXButton;
    btnPhoto: TSkinFMXButton;
    btnCamera: TSkinFMXButton;
    btnCheck: TSkinFMXButton;
    btnReview: TSkinFMXButton;
    btnMore: TSkinFMXButton;
    aniBtnText: TSkinControlMoveAnimator;
    bdmButton: TSkinButtonDefaultMaterial;
    anibtnPhoto: TSkinControlMoveAnimator;
    anibtnCamera: TSkinControlMoveAnimator;
    anibtnCheck: TSkinControlMoveAnimator;
    anibtnReview: TSkinControlMoveAnimator;
    anibtnMore: TSkinControlMoveAnimator;
    procedure aniImgCloseRotateAnimate(Sender: TObject);
    procedure imgCloseClick(Sender: TObject);
    procedure aniImgCloseRotateAnimateEnd(Sender: TObject);
    procedure imgBackClick(Sender: TObject);
    procedure imgBackResize(Sender: TObject);
    procedure btnTextClick(Sender: TObject);
  private
    { Private declarations }
  public
    FrameHistroy:TFrameHistroy;
  public
    procedure ShowEnterAnimation;
    procedure ShowExitAnimation;
    { Public declarations }
  end;


var
  GlobalMobileSelPublishTypeFrame:TFrameMobileSelPublishType;

implementation

{$R *.fmx}

procedure TFrameMobileSelPublishType.aniImgCloseRotateAnimate(Sender: TObject);
begin
  Self.imgClose.Properties.CurrentRotateAngle:=Self.aniImgCloseRotate.Position;
end;

procedure TFrameMobileSelPublishType.aniImgCloseRotateAnimateEnd(Sender: TObject);
begin
  if aniImgCloseRotate.DirectionType=TAnimateDirectionType.adtBackward then
  begin

    //返回

    HideFrame(Self);
    ReturnFrame(FrameHistroy);



  end;
end;

procedure TFrameMobileSelPublishType.btnTextClick(Sender: TObject);
begin
  //发文本微博
  ShowExitAnimation;
end;

procedure TFrameMobileSelPublishType.imgCloseClick(Sender: TObject);
begin
  ShowExitAnimation;
end;

procedure TFrameMobileSelPublishType.ShowEnterAnimation;
begin
  aniImgCloseRotate.DirectionType:=TAnimateDirectionType.adtForward;
  aniImgCloseRotate.Start;



  aniBtnText.DirectionType:=TAnimateDirectionType.adtBackward;
  aniBtnText.Max:=Ceil(Self.Height+aniBtnText.Min);
  aniBtnText.AnimateControl.Position.Y:=aniBtnText.Max;
  aniBtnText.Start;

  aniBtnPhoto.DirectionType:=TAnimateDirectionType.adtBackward;
  aniBtnPhoto.Max:=Ceil(Self.Height+aniBtnPhoto.Min);
  aniBtnPhoto.AnimateControl.Position.Y:=aniBtnPhoto.Max;
  aniBtnPhoto.Start;

  aniBtnCamera.DirectionType:=TAnimateDirectionType.adtBackward;
  aniBtnCamera.Max:=Ceil(Self.Height+aniBtnCamera.Min);
  aniBtnCamera.AnimateControl.Position.Y:=aniBtnCamera.Max;
  aniBtnCamera.Start;


  aniBtnCheck.DirectionType:=TAnimateDirectionType.adtBackward;
  aniBtnCheck.Max:=Ceil(Self.Height+aniBtnCheck.Min);
  aniBtnCheck.AnimateControl.Position.Y:=aniBtnCheck.Max;
  aniBtnCheck.Start;

  aniBtnReview.DirectionType:=TAnimateDirectionType.adtBackward;
  aniBtnReview.Max:=Ceil(Self.Height+aniBtnReview.Min);
  aniBtnReview.AnimateControl.Position.Y:=aniBtnReview.Max;
  aniBtnReview.Start;

  aniBtnMore.DirectionType:=TAnimateDirectionType.adtBackward;
  aniBtnMore.Max:=Ceil(Self.Height+aniBtnMore.Min);
  aniBtnMore.AnimateControl.Position.Y:=aniBtnMore.Max;
  aniBtnMore.Start;


end;

procedure TFrameMobileSelPublishType.ShowExitAnimation;
begin
  aniImgCloseRotate.DirectionType:=TAnimateDirectionType.adtBackward;
  aniImgCloseRotate.Start;


  aniBtnText.DirectionType:=TAnimateDirectionType.adtForward;
  aniBtnText.Start;

  aniBtnPhoto.DirectionType:=TAnimateDirectionType.adtForward;
  aniBtnPhoto.Start;

  aniBtnCamera.DirectionType:=TAnimateDirectionType.adtForward;
  aniBtnCamera.Start;


  aniBtnCheck.DirectionType:=TAnimateDirectionType.adtForward;
  aniBtnCheck.Start;

  aniBtnReview.DirectionType:=TAnimateDirectionType.adtForward;
  aniBtnReview.Start;

  aniBtnMore.DirectionType:=TAnimateDirectionType.adtForward;
  aniBtnMore.Start;


end;

procedure TFrameMobileSelPublishType.imgBackClick(Sender: TObject);
begin
  ShowExitAnimation;
end;

procedure TFrameMobileSelPublishType.imgBackResize(Sender: TObject);
begin
  Self.imgClose.Left:=Ceil(Width-Self.imgClose.WidthInt) div 2;


//  Self.btnText.Width:=(Width) / 3;
//  Self.btnPhoto.Width:=(Width) / 3;
//  Self.btnCamera.Width:=(Width) / 3;

  Self.btnText.Left:=Ceil(Width-btnText.WidthInt*3) div 2;
  Self.btnPhoto.Left:=btnText.Left+btnText.WidthInt;
  Self.btnCamera.Left:=btnPhoto.Left+btnPhoto.WidthInt;



//  Self.btnCheck.Width:=(Width) / 3;
//  Self.btnReview.Width:=(Width) / 3;
//  Self.btnMore.Width:=(Width) / 3;

  Self.btnCheck.Left:=Ceil(Width-btnCheck.WidthInt*3) div 2;
  Self.btnReview.Left:=btnCheck.Left+btnCheck.WidthInt;
  Self.btnMore.Left:=btnReview.Left+btnReview.WidthInt;




end;

end.
