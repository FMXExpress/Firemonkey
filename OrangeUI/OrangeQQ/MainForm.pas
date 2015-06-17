unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl,
  LoginFrame,uUIFunction, uSkinFireMonkeyControl, uSkinFireMonkeyFrameImage,
  uSkinFireMonkeyPanel, uSkinFireMonkeyPageControl,
  MessageFrame,StateFrame,SettingFrame,ContactorFrame,
  uSkinFireMonkeyImage, FMX.Edit, uSkinFireMonkeyEdit, FMX.Objects, FMX.Effects,
  FMX.StdCtrls, FMX.Layouts, FMX.ListBox, FMX.ExtCtrls, FMX.Header,
  FMX.Styles.Switch, FMX.Ani, FMX.Styles, FMX.Styles.Objects,
  uBufferBitmap,
  Math,
//  uMobileUtils,
  uDrawTextParam,
  uSkinPageControlType, uSkinFireMonkeyNotifyNumberIcon, uSkinFireMonkeyLabel,
  uSkinFireMonkeySwitchPageListPanel;

type
  TfrmMain = class(TForm)
    pcMain: TSkinFMXPageControl;
    tsMessage: TSkinFMXTabSheet;
    tsContactor: TSkinFMXTabSheet;
    tsState: TSkinFMXTabSheet;
    nniState: TSkinFMXNotifyNumberIcon;
    nniMessage: TSkinFMXNotifyNumberIcon;
    procedure FormCreate(Sender: TObject);
    procedure pcMainResize(Sender: TObject);
    procedure pcMainChange(Sender: TObject);
    procedure FormVirtualKeyboardShown(Sender: TObject;
      KeyboardVisible: Boolean; const Bounds: TRect);
    procedure FormVirtualKeyboardHidden(Sender: TObject;
      KeyboardVisible: Boolean; const Bounds: TRect);
    procedure pcMainChanging(Sender: TObject; NewIndex: Integer;
      var AllowChange: Boolean);
    procedure FormShow(Sender: TObject);
  private
//    FBufferBitmap:TBufferBitmap;
//    FBufferBitmap1:TBufferBitmap;
    { Private declarations }
  public
    procedure OnReturnFrame(FromFrame:TFrame);
    procedure OnReturnFrameAfterLogin(FromFrame:TFrame);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
  Self.Width:=320;
  Self.Height:=480;
  {$ENDIF}


  //显示登录界面
  ShowFrame(TFrame(GlobalLoginFrame),TFrameLogin,frmMain,nil,nil,OnReturnFrameAfterLogin,Application);
  GlobalLoginFrame.FrameHistroy:=CurrentFrameHistroy;


  pcMain.Properties.SwitchPageListControlGestureManager.CanGestureSwitch:=True;
  pcMain.Properties.SwitchPageListControlGestureManager.ControlGestureManager.IsUseDecideFirstMouseMoveKind:=True;




  pcMain.Properties.SwitchPageAnimateSpeed:=2;
  pcMain.Properties.SwitchPageAnimated:=True;
  pcMain.Properties.SwitchPageListControlGestureManager.PageListSwitchingProgressIncement:=30;

  pcMainChange(Self);
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  GetGlobalVirtualKeyboardFixer.StartSync(Self);
end;

procedure TfrmMain.FormVirtualKeyboardHidden(Sender: TObject;
  KeyboardVisible: Boolean; const Bounds: TRect);
begin
  CallSubFrameVirtualKeyboardHidden(Sender,Self,KeyboardVisible,Bounds);
end;

procedure TfrmMain.FormVirtualKeyboardShown(Sender: TObject;
  KeyboardVisible: Boolean; const Bounds: TRect);
begin
  CallSubFrameVirtualKeyboardShown(Sender,Self,KeyboardVisible,Bounds);
end;

procedure TfrmMain.OnReturnFrame(FromFrame: TFrame);
begin
end;

procedure TfrmMain.OnReturnFrameAfterLogin(FromFrame: TFrame);
begin
  Self.pcMain.Properties.ActivePage:=tsMessage;

  //显示消息界面
  ShowFrame(TFrame(GlobalMessageFrame),TFrameMessage,tsMessage,nil,nil,OnReturnFrame,Application,True);
  GlobalMessageFrame.FrameHistroy:=CurrentFrameHistroy;

  //显示联系人界面
  ShowFrame(TFrame(GlobalContactorFrame),TFrameContactor,tsContactor,nil,nil,OnReturnFrame,Application,False);
//  GlobalContactorFrame.FrameHistroy:=CurrentFrameHistroy;

  //显示动态界面
  ShowFrame(TFrame(GlobalStateFrame),TFrameState,tsState,nil,nil,OnReturnFrame,Application,False);
//  GlobalStateFrame.FrameHistroy:=CurrentFrameHistroy;

end;

procedure TfrmMain.pcMainChange(Sender: TObject);
begin
//  nniMessage.Visible:=Self.pcMain.Properties.ActivePage<>tsMessage;
//  nniState.Visible:=Self.pcMain.Properties.ActivePage<>tsState;
end;

procedure TfrmMain.pcMainChanging(Sender: TObject; NewIndex: Integer;
  var AllowChange: Boolean);
begin
//  if NewIndex=1 then
//  begin
//    //显示联系人界面
//    ShowFrame(TFrame(GlobalContactorFrame),TFrameContactor,tsContactor,nil,nil,OnReturnFrame,Application);
//    GlobalContactorFrame.FrameHistroy:=CurrentFrameHistroy;
//  end;
//  if NewIndex=2 then
//  begin
//    //显示动态界面
//    ShowFrame(TFrame(GlobalStateFrame),TFrameState,tsState,nil,nil,OnReturnFrame,Application);
//    GlobalStateFrame.FrameHistroy:=CurrentFrameHistroy;
//  end;

end;

procedure TfrmMain.pcMainResize(Sender: TObject);
begin
//  Self.nniMessage.Left:=(Self.pcMain.WidthInt div 3) * 0
//      +(Self.pcMain.WidthInt div 3) div 2  + 3;
//  Self.nniState.Left:=(Self.pcMain.WidthInt div 3) * 2
//      +(Self.pcMain.WidthInt div 3) div 2  + 8;
//
//  Self.nniMessage.Top:=(Self.pcMain.HeightInt - Self.pcMain.Properties.TabHeaderHeight);
//  Self.nniState.Top:=(Self.pcMain.HeightInt - Self.pcMain.Properties.TabHeaderHeight);

end;

end.
