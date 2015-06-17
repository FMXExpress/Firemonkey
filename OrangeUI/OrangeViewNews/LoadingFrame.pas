unit LoadingFrame;

interface

uses
  System.SysUtils,uFuncCommon, System.Types, System.UITypes, System.Classes, System.Variants,
  Math,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, uSkinFireMonkeyImageListPlayer, uSkinFireMonkeyControl,
  uSkinFireMonkeyImage, uSkinImageList;

type
  TFrameLoading = class(TFrame)
    imglistLoading: TSkinImageList;
    imgLoading: TSkinFMXImageListPlayer;
    tmrTimeOut: TTimer;
    tmrBringToFont: TTimer;
    procedure FrameResize(Sender: TObject);
    procedure tmrTimeOutTimer(Sender: TObject);
    procedure tmrBringToFontTimer(Sender: TObject);
  private
    ParentOldResize:TNotifyEvent;
    procedure DoParentResize(Sender: TObject);
    { Private declarations }
  public
    constructor Create(AOwner:TComponent);override;
    procedure ShowLoading(Parent:TFmxObject;TimeOut:Integer=0);
    procedure HideLoading;
    { Public declarations }
  end;


var
  GlobalLoadingFrame:TFrameLoading;


procedure ShowLoadingFrame(Parent:TFmxObject;TimeOut:Integer=0);
procedure HideLoadingFrame;

implementation

{$R *.fmx}


procedure ShowLoadingFrame(Parent:TFmxObject;TimeOut:Integer);
begin
  if GlobalLoadingFrame=nil then
  begin
    GlobalLoadingFrame:=TFrameLoading.Create(Application);
  end;
  GlobalLoadingFrame.ShowLoading(Parent,TimeOut);
//  GlobalLoadingFrame.Parent:=Parent;
//
//  GlobalLoadingFrame.DoParentResize(Parent);
//
//  if (Parent<>nil) and (Parent is TControl) then
//  begin
//    GlobalLoadingFrame.ParentOldResize:=TControl(Parent).OnResize;
//    TControl(Parent).OnResize:=GlobalLoadingFrame.DoParentResize;
//  end;
//  if (Parent<>nil) and (Parent is TForm) then
//  begin
//    GlobalLoadingFrame.ParentOldResize:=TForm(Parent).OnResize;
//    TForm(Parent).OnResize:=GlobalLoadingFrame.DoParentResize;
//  end;
//
////  GlobalLoadingFrame.DoParentResize(Parent);
//
//  GlobalLoadingFrame.BringToFront;
//
//  GlobalLoadingFrame.imgLoading.Properties.ImageListAnimated:=True;
//
//  GlobalLoadingFrame.Visible:=True;
//
//  if TimeOut>0 then
//  begin
//    GlobalLoadingFrame.tmrTimeOur.Interval:=TimeOut;
//    GlobalLoadingFrame.tmrTimeOur.Enabled:=True;
//  end;
end;

procedure HideLoadingFrame;
begin
  if GlobalLoadingFrame<>nil then
  begin
    GlobalLoadingFrame.HideLoading;
//    if (GlobalLoadingFrame.Parent<>nil) and (GlobalLoadingFrame.Parent is TControl) then
//    begin
//      TControl(GlobalLoadingFrame.Parent).OnResize:=GlobalLoadingFrame.ParentOldResize;
//    end;
//    if (GlobalLoadingFrame.Parent<>nil) and (GlobalLoadingFrame.Parent is TForm) then
//    begin
//      TForm(GlobalLoadingFrame.Parent).OnResize:=GlobalLoadingFrame.ParentOldResize;
//    end;
//
//
//    GlobalLoadingFrame.tmrTimeOur.Enabled:=False;
//    GlobalLoadingFrame.imgLoading.Properties.ImageListAnimated:=False;
//    GlobalLoadingFrame.Parent:=nil;
//    GlobalLoadingFrame.Visible:=False;
  end;
end;

constructor TFrameLoading.Create(AOwner: TComponent);
begin
  inherited;

  Self.OnResize:=Self.FrameResize;
end;

procedure TFrameLoading.DoParentResize(Sender: TObject);
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

procedure TFrameLoading.FrameResize(Sender: TObject);
begin
  Self.imgLoading.Left:=Ceil(Self.Width-Self.imgLoading.Width) div 2;
  Self.imgLoading.Top:=Ceil(Self.Height-Self.imgLoading.Height) div 2;
end;

procedure TFrameLoading.HideLoading;
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
    Self.imgLoading.Properties.ImageListAnimated:=False;
    Self.Parent:=nil;
    Self.Visible:=False;

end;

procedure TFrameLoading.ShowLoading(Parent: TFmxObject; TimeOut: Integer);
begin
  HideLoading;

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

//  Self.DoParentResize(Parent);

  Self.BringToFront;

  Self.imgLoading.Properties.ImageListAnimated:=True;

  Self.Visible:=True;

  if TimeOut>0 then
  begin
    Self.tmrTimeOut.Interval:=TimeOut;
    Self.tmrTimeOut.Enabled:=True;
  end;
end;

procedure TFrameLoading.tmrBringToFontTimer(Sender: TObject);
begin
  Self.BringToFront;
end;

procedure TFrameLoading.tmrTimeOutTimer(Sender: TObject);
begin
  Self.tmrTimeOut.Enabled:=False;
  HideLoadingFrame;
end;

end.
