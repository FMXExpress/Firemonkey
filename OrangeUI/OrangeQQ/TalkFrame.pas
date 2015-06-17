unit TalkFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  uSkinImageList, uSkinFireMonkeyLabel, uSkinFireMonkeyItemDesignerPanel,
  uSkinFireMonkeyScrollControl, uSkinFireMonkeyListBox, uSkinFireMonkeyImage,
  uUIFunction,
  FMX.VirtualKeyboard,
  FMX.Platform,
  uSkinItems,
  uDrawCanvas,
  uSkinPackage,
  uComponentType,
  uBufferBitmap,
//  CommonSkinMaterialModule,
//  MessageHintFrame,
  uSkinListBoxType,
  uSkinFireMonkeyControl, uSkinFireMonkeyPanel, uSkinFireMonkeyButton,
  FMX.Controls.Presentation, FMX.Edit, uSkinFireMonkeyEdit, FMX.Layouts,
  FMX.Memo, uSkinFireMonkeyMemo, uDrawPicture,
  {$IFDEF VER290}
  FMX.ScrollBox,
  {$ENDIF}
  uSkinPageControlType, uSkinFireMonkeyPageControl,
  uSkinFireMonkeySwitchPageListPanel, uSkinMaterial, uSkinButtonType,
  uSkinFireMonkeyFrameImage, uSkinFireMonkeyDirectUIParent,
  uSkinFireMonkeyScrollBoxContent, uSkinFireMonkeyScrollBox,
  uSkinFireMonkeyScrollControlCorner, uSkinFireMonkeyScrollBar;

type
  TFrameTalk = class(TFrame,IFrameVirtualKeyboardEvent,IFrameHistroyVisibleEvent)
    imglistHead: TSkinImageList;
    imglistChatBottom: TSkinImageList;
    tmrAddTestFriendMsg: TTimer;
    pnlToolBar: TSkinFMXPanel;
    imgChatBottom: TSkinFMXImage;
    SkinFMXButton2: TSkinFMXButton;
    SkinFMXButton3: TSkinFMXButton;
    pnlVirtualKeyboard: TSkinFMXPanel;
    bdmMyMsgButton: TSkinButtonDefaultMaterial;
    imgTalkSign: TSkinImageList;
    bdmMyVoiceMsg: TSkinButtonDefaultMaterial;
    bdmYourMsgButton: TSkinButtonDefaultMaterial;
    bdmMyVoiceLen: TSkinButtonDefaultMaterial;
    bdmTextYourMsg: TSkinButtonDefaultMaterial;
    bdmMyTextMsg: TSkinButtonDefaultMaterial;
    sbClient: TSkinFMXScrollBox;
    sbcClient: TSkinFMXScrollBoxContent;
    SkinFMXButton1: TSkinFMXButton;
    SkinFMXPanel2: TSkinFMXPanel;
    SkinFMXButton4: TSkinFMXButton;
    SkinFMXButton6: TSkinFMXButton;
    SkinFMXButton5: TSkinFMXButton;
    SkinFMXPanel3: TSkinFMXPanel;
    SkinFMXButton7: TSkinFMXButton;
    SkinFMXButton8: TSkinFMXButton;
    SkinFMXPanel4: TSkinFMXPanel;
    SkinFMXButton9: TSkinFMXButton;
    pcMessage: TSkinFMXPageControl;
    tsText: TSkinFMXTabSheet;
    tsVoice: TSkinFMXTabSheet;
    edtMyMsg: TSkinFMXMemo;
    SkinFMXButton10: TSkinFMXButton;
    SkinFMXPanel1: TSkinFMXPanel;
    btnReturn: TSkinFMXButton;
    imglistState: TSkinImageList;
    procedure btnReturnClick(Sender: TObject);
    procedure tmrAddTestFriendMsgTimer(Sender: TObject);
    procedure edtMyMsgMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure lbTalkMsgListClick(Sender: TObject);
    procedure SkinFMXButton3Click(Sender: TObject);
    procedure SkinFMXButton10MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure SkinFMXButton10MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
  private
    procedure DoShow;
    procedure DoHide;

    //显示虚拟键盘
    procedure DoVirtualKeyboardShow(KeyboardVisible: Boolean; const Bounds: TRect);
    //隐藏虚拟键盘
    procedure DoVirtualKeyboardHide(KeyboardVisible: Boolean; const Bounds: TRect);
    { Private declarations }
  public
    FrameHistroy:TFrameHistroy;
    procedure OnReturnFrame(FromFrame:TFrame);
    constructor Create(AOwner:TComponent);override;
    { Public declarations }
  end;

var
  GlobalTalkFrame:TFrameTalk;

implementation

{$R *.fmx}

{ TFrameTalk }

constructor TFrameTalk.Create(AOwner: TComponent);
begin
  inherited;
  Self.pnlVirtualKeyboard.Height:=0;
  Self.pcMessage.Properties.TabHeaderHeight:=0;

  SkinFMXButton10MouseUp(SkinFMXButton10,TMouseButton.mbLeft,[],0,0);
  SkinFMXButton10MouseUp(SkinFMXButton10,TMouseButton.mbLeft,[],0,0);
  SkinFMXButton10MouseUp(SkinFMXButton10,TMouseButton.mbLeft,[],0,0);
  SkinFMXButton10MouseUp(SkinFMXButton10,TMouseButton.mbLeft,[],0,0);
  SkinFMXButton10MouseUp(SkinFMXButton10,TMouseButton.mbLeft,[],0,0);
  SkinFMXButton10MouseUp(SkinFMXButton10,TMouseButton.mbLeft,[],0,0);
  SkinFMXButton10MouseUp(SkinFMXButton10,TMouseButton.mbLeft,[],0,0);


  Self.sbClient.VertScrollBar.Properties.Position:=0;
end;

procedure TFrameTalk.DoHide;
begin
end;

procedure TFrameTalk.DoShow;
begin
end;

procedure TFrameTalk.DoVirtualKeyboardHide(KeyboardVisible: Boolean;
  const Bounds: TRect);
begin
  Self.pnlVirtualKeyboard.Height:=0;
end;

procedure TFrameTalk.DoVirtualKeyboardShow(KeyboardVisible: Boolean;
  const Bounds: TRect);
//var
//  AFixTop:Integer;
begin
  if Bounds.Height-GetGlobalVirtualKeyboardFixer.VirtualKeyboardHideHeight>Self.pnlVirtualKeyboard.Height then
  begin
    Self.pnlVirtualKeyboard.Height:=RectHeight(Bounds)-GetGlobalVirtualKeyboardFixer.VirtualKeyboardHideHeight;
//    //计算出合理的位置
//    AFixTop:=Ceil(Self.btnLogin.Top+Self.btnLogin.Height)-Self.pnlVirtualKeyboard.Top;
//    Self.sbClient.VertScrollBar.Properties.Position:=AFixTop;
  end;
end;

procedure TFrameTalk.OnReturnFrame(FromFrame: TFrame);
begin
end;

procedure TFrameTalk.SkinFMXButton10MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  Self.SkinFMXButton10.StaticText:='松开发送';
  Self.SkinFMXButton10.Properties.IsPushed:=True;
end;

procedure TFrameTalk.SkinFMXButton10MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  AButton:TSkinFMXButton;
  APanel:TSkinFMXPanel;
begin
  Self.SkinFMXButton10.StaticText:='按住说话';
  Self.SkinFMXButton10.Properties.IsPushed:=False;


  //发送一段语音消息
  AButton:=TSkinFMXButton.Create(Self);
  AButton.Caption:='OrangeUI意向客户';
  AButton.Detail:=FormatDateTime('YYYY-MM-DD HH:MM',Now);
  AButton.Properties.Icon.SkinImageList:=Self.imglistHead;
  AButton.Properties.Icon.ImageIndex:=0;
  AButton.KeepSelfOwnMaterial:=False;
  AButton.MaterialUseKind:=uComponentType.mukRef;
  AButton.RefMaterial:=Self.bdmMyMsgButton;
  AButton.Height:=63;
  AButton.Align:=TAlignLayout.alTop;
  AButton.Parent:=Self.sbcClient;
  AButton.Top:=sbcClient.HeightInt;
  AButton.ParentMouseEvent:=True;
  sbcClient.Height:=sbcClient.Height+AButton.Height;
  sbClient.VertScrollBar.Properties.Position:=sbClient.VertScrollBar.Properties.Max;

  //Panel
  APanel:=TSkinFMXPanel.Create(Self);
  APanel.Height:=46;
  APanel.Align:=TAlignLayout.alTop;
  APanel.Parent:=Self.sbcClient;
  APanel.Top:=sbcClient.HeightInt;
  APanel.ParentMouseEvent:=True;
  sbcClient.Height:=sbcClient.Height+APanel.Height;
  sbClient.VertScrollBar.Properties.Position:=sbClient.VertScrollBar.Properties.Max;

  AButton:=TSkinFMXButton.Create(Self);
  AButton.Properties.Icon.SkinImageList:=Self.imgTalkSign;
  AButton.Properties.Icon.ImageIndex:=0;
  AButton.KeepSelfOwnMaterial:=False;
  AButton.MaterialUseKind:=uComponentType.mukRef;
  AButton.RefMaterial:=Self.bdmMyVoiceMsg;
  AButton.Left:=68;
  AButton.Width:=100;
  AButton.Height:=42;
  AButton.Parent:=APanel;
  AButton.ParentMouseEvent:=True;


  AButton:=TSkinFMXButton.Create(Self);
  AButton.Caption:='3"';
  AButton.Properties.Icon.SkinImageList:=Self.imgTalkSign;
  AButton.Properties.Icon.ImageIndex:=1;
  AButton.KeepSelfOwnMaterial:=False;
  AButton.MaterialUseKind:=uComponentType.mukRef;
  AButton.RefMaterial:=Self.bdmMyVoiceLen;
  AButton.Left:=200;
  AButton.Top:=13;
  AButton.Width:=46;
  AButton.Height:=25;
  AButton.Parent:=APanel;
  AButton.ParentMouseEvent:=True;


end;

procedure TFrameTalk.SkinFMXButton3Click(Sender: TObject);
begin
//  Self.SkinFMXButton3.Properties.StaticIsPushed:=Not Self.SkinFMXButton3.Properties.IsPushed;
  if Self.SkinFMXButton3.Properties.IsPushed then
  begin
    Self.pcMessage.Properties.ActivePageIndex:=1;
  end
  else
  begin
    Self.pcMessage.Properties.ActivePageIndex:=0;
  end;
end;

procedure TFrameTalk.btnReturnClick(Sender: TObject);
begin
  HideFrame(Self);
  ReturnFrame(FrameHistroy);
end;

procedure TFrameTalk.tmrAddTestFriendMsgTimer(Sender: TObject);
//var
//  FriendMsgListBoxItem:TSkinListBoxItem;
begin
//  tmrAddTestFriendMsg.Enabled:=False;
//
//  //发送
//  Self.lbTalkMsgList.Properties.Items.BeginUpdate;
//  try
//
//    FriendMsgListBoxItem:=Self.lbTalkMsgList.Properties.Items.Add;
//
//    FriendMsgListBoxItem.Caption:='您好，我有事不在，请一会儿再联系！';
//
//    FriendMsgListBoxItem.ItemType:=sitDefault;
//
//    CalcMsgSizeByListBoxItem(FriendMsgListBoxItem);
//
//  finally
//    Self.lbTalkMsgList.Properties.Items.EndUpdate;
//  end;
//
//
//  Self.lbTalkMsgList.VertScrollBar.Properties.Position:=
//    Self.lbTalkMsgList.VertScrollBar.Properties.Max;

end;

procedure TFrameTalk.edtMyMsgMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
//var
//  VKbSvc: IFMXVirtualKeyboardService;
begin
//  {$IFNDEF MSWINDOWS}
//  if TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardService, VKbSvc) then
//  begin
//    VKbSvc.ShowVirtualKeyboard(Self.edtMyMsg);
//  end;
//  {$ENDIF}
//  Self.lbTalkMsgList.VertScrollBar.Properties.Position:=
//    Self.lbTalkMsgList.VertScrollBar.Properties.Max;
end;

procedure TFrameTalk.lbTalkMsgListClick(Sender: TObject);
//var
//  VKbSvc: IFMXVirtualKeyboardService;
begin
//  //关闭虚拟键盘
//  if TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardService, VKbSvc) then
//  begin
//    VKbSvc.HideVirtualKeyboard;
//  end;
end;

end.

