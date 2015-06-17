unit WaitingFrame;

interface

uses
  System.SysUtils,uFuncCommon, System.Types, System.UITypes, System.Classes, System.Variants,
  Math,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, uSkinFireMonkeyImageListPlayer, uSkinFireMonkeyControl,
  uSkinFireMonkeyImage, uSkinImageList, FMX.Objects, uSkinFireMonkeyLabel;

type
  TFrameWaiting = class(TFrame)
    imglistWaiting: TSkinImageList;
    tmrTimeOut: TTimer;
    tmrBringToFont: TTimer;
    BackRectangle: TRectangle;
    imgWaiting: TSkinFMXImageListPlayer;
    BackGround: TRectangle;
    lblWaiting: TSkinFMXLabel;
    procedure FrameResize(Sender: TObject);
    procedure tmrTimeOutTimer(Sender: TObject);
    procedure tmrBringToFontTimer(Sender: TObject);
  private
    ParentOldResize:TNotifyEvent;
    procedure DoParentResize(Sender: TObject);
    { Private declarations }
  public
    constructor Create(AOwner:TComponent);override;
    procedure ShowWaiting(Parent:TFmxObject;Hint:String;TimeOut:Integer=0);
    procedure HideWaiting;
    { Public declarations }
  end;


var
  GlobalWaitingFrame:TFrameWaiting;


procedure ShowWaitingFrame(Parent:TFmxObject;Hint:String;TimeOut:Integer=0);
procedure HideWaitingFrame;

implementation

{$R *.fmx}


procedure ShowWaitingFrame(Parent:TFmxObject;Hint:String;TimeOut:Integer);
begin
  if GlobalWaitingFrame=nil then
  begin
    GlobalWaitingFrame:=TFrameWaiting.Create(Application);
  end;
  GlobalWaitingFrame.ShowWaiting(Parent,Hint,TimeOut);
//  GlobalWaitingFrame.Parent:=Parent;
//
//  GlobalWaitingFrame.DoParentResize(Parent);
//
//  if (Parent<>nil) and (Parent is TControl) then
//  begin
//    GlobalWaitingFrame.ParentOldResize:=TControl(Parent).OnResize;
//    TControl(Parent).OnResize:=GlobalWaitingFrame.DoParentResize;
//  end;
//  if (Parent<>nil) and (Parent is TForm) then
//  begin
//    GlobalWaitingFrame.ParentOldResize:=TForm(Parent).OnResize;
//    TForm(Parent).OnResize:=GlobalWaitingFrame.DoParentResize;
//  end;
//
////  GlobalWaitingFrame.DoParentResize(Parent);
//
//  GlobalWaitingFrame.BringToFront;
//
//  GlobalWaitingFrame.imgWaiting.Properties.ImageListAnimated:=True;
//
//  GlobalWaitingFrame.Visible:=True;
//
//  if TimeOut>0 then
//  begin
//    GlobalWaitingFrame.tmrTimeOut.Interval:=TimeOut;
//    GlobalWaitingFrame.tmrTimeOut.Enabled:=True;
//  end;
end;

procedure HideWaitingFrame;
begin
  if GlobalWaitingFrame<>nil then
  begin
    GlobalWaitingFrame.HideWaiting;
//    if (GlobalWaitingFrame.Parent<>nil) and (GlobalWaitingFrame.Parent is TControl) then
//    begin
//      TControl(GlobalWaitingFrame.Parent).OnResize:=GlobalWaitingFrame.ParentOldResize;
//    end;
//    if (GlobalWaitingFrame.Parent<>nil) and (GlobalWaitingFrame.Parent is TForm) then
//    begin
//      TForm(GlobalWaitingFrame.Parent).OnResize:=GlobalWaitingFrame.ParentOldResize;
//    end;
//
//
//    GlobalWaitingFrame.tmrTimeOut.Enabled:=False;
//    GlobalWaitingFrame.imgWaiting.Properties.ImageListAnimated:=False;
//    GlobalWaitingFrame.Parent:=nil;
//    GlobalWaitingFrame.Visible:=False;
  end;
end;

constructor TFrameWaiting.Create(AOwner: TComponent);
begin
  inherited;

  Self.OnResize:=Self.FrameResize;
end;

procedure TFrameWaiting.DoParentResize(Sender: TObject);
begin
  Self.Left:=0;
  Self.Top:=0;
  if (Parent<>nil) and (Parent is TControl) then
  begin
    Width:=TControl(Parent).Width;
    Height:=TControl(Parent).Height;
  end;
  if (Parent<>nil) and (Parent is TForm) then
  begin
    Width:=TForm(Parent).Width;
    Height:=TForm(Parent).Height;
  end;
  if Assigned(ParentOldResize) then
  begin
    ParentOldResize(Sender);
  end;
end;

procedure TFrameWaiting.FrameResize(Sender: TObject);
begin
  Self.imgWaiting.Left:=Ceil(Self.Width-Self.imgWaiting.Width) div 2;
  Self.imgWaiting.Top:=Ceil(Self.Height-Self.imgWaiting.Height) div 2;

  Self.lblWaiting.Position.X:=Ceil(Self.Width-Self.lblWaiting.Width) div 2;
  Self.lblWaiting.Position.Y:=Self.imgWaiting.Top+Self.imgWaiting.Height+10;

  Self.BackRectangle.Position.X:=Ceil(Self.Width-Self.BackRectangle.Width) div 2;
  Self.BackRectangle.Position.Y:=Ceil(Self.Height-Self.BackRectangle.Height) div 2;
end;

procedure TFrameWaiting.HideWaiting;
begin
    if (Self.Parent<>nil) and (Self.Parent is TControl) then
    begin
      TControl(Self.Parent).OnResize:=Self.ParentOldResize;
    end;
    if (Self.Parent<>nil) and (Self.Parent is TForm) then
    begin
      TForm(Self.Parent).OnResize:=Self.ParentOldResize;
    end;


    Self.tmrTimeOut.Enabled:=False;
    Self.imgWaiting.Properties.ImageListAnimated:=False;
    Self.Parent:=nil;
    Self.Visible:=False;

end;

procedure TFrameWaiting.ShowWaiting(Parent: TFmxObject;Hint:String; TimeOut: Integer);
begin
  HideWaiting;

  Self.Parent:=Parent;

  Self.DoParentResize(Parent);

  if (Parent<>nil) and (Parent is TControl) then
  begin
    Self.ParentOldResize:=TControl(Parent).OnResize;
    TControl(Parent).OnResize:=Self.DoParentResize;
  end;
  if (Parent<>nil) and (Parent is TForm) then
  begin
    Self.ParentOldResize:=TForm(Parent).OnResize;
    TForm(Parent).OnResize:=Self.DoParentResize;
  end;

  Self.lblWaiting.Caption:=Hint;
//  Self.DoParentResize(Parent);

  Self.BringToFront;

  Self.imgWaiting.Properties.ImageListAnimated:=True;

  Self.Visible:=True;

  if TimeOut>0 then
  begin
    Self.tmrTimeOut.Interval:=TimeOut;
    Self.tmrTimeOut.Enabled:=True;
  end;
end;

procedure TFrameWaiting.tmrBringToFontTimer(Sender: TObject);
begin
  Self.BringToFront;
end;

procedure TFrameWaiting.tmrTimeOutTimer(Sender: TObject);
begin
  Self.tmrTimeOut.Enabled:=False;
  HideWaitingFrame;
end;

end.
