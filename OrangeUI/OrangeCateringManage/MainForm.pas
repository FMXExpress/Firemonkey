unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  uSkinFireMonkeyControl, uSkinFireMonkeyImage, uSkinFireMonkeyLabel,
  uSkinImageList, uSkinFireMonkeyButton, FMX.TabControl,
  uSkinFireMonkeyScrollControl, uSkinFireMonkeyListBox,
  uDrawCanvas,
  Math,uSkinItems,
  uSkinScrollBarType,
  uSkinFireMonkeyItemDesignerPanel, uDrawPicture;

type
  TfrmMain = class(TForm)
    imglistNavigate: TSkinImageList;
    SkinFMXButton1: TSkinFMXButton;
    TabControl1: TTabControl;
    SkinFMXImage1: TSkinFMXImage;
    SkinFMXImage2: TSkinFMXImage;
    SkinFMXLabel1: TSkinFMXLabel;
    SkinFMXButton2: TSkinFMXButton;
    SkinFMXButton3: TSkinFMXButton;
    SkinFMXButton4: TSkinFMXButton;
    SkinFMXButton5: TSkinFMXButton;
    SkinFMXButton6: TSkinFMXButton;
    TabItem1: TTabItem;
    SkinFMXButton7: TSkinFMXButton;
    SkinFMXButton8: TSkinFMXButton;
    SkinFMXButton9: TSkinFMXButton;
    SkinFMXButton10: TSkinFMXButton;
    SkinFMXButton11: TSkinFMXButton;
    SkinFMXButton12: TSkinFMXButton;
    SkinFMXButton13: TSkinFMXButton;
    SkinFMXButton14: TSkinFMXButton;
    SkinFMXButton15: TSkinFMXButton;
    SkinFMXButton16: TSkinFMXButton;
    SkinFMXLabel2: TSkinFMXLabel;
    SkinFMXListBox1: TSkinFMXListBox;
    imglistItemBack: TSkinImageList;
    imglistBillState: TSkinImageList;
    SkinFMXImage5: TSkinFMXImage;
    SkinFMXButton18: TSkinFMXButton;
    SkinFMXButton19: TSkinFMXButton;
    SkinFMXLabel7: TSkinFMXLabel;
    SkinFMXLabel8: TSkinFMXLabel;
    SkinFMXLabel9: TSkinFMXLabel;
    SkinFMXLabel10: TSkinFMXLabel;
    SkinFMXLabel11: TSkinFMXLabel;
    SkinFMXLabel12: TSkinFMXLabel;
    SkinFMXLabel13: TSkinFMXLabel;
    SkinFMXLabel14: TSkinFMXLabel;
    SkinFMXLabel15: TSkinFMXLabel;
    SkinFMXLabel16: TSkinFMXLabel;
    SkinFMXItemDesignerPanel1: TSkinFMXItemDesignerPanel;
    SkinFMXImage3: TSkinFMXImage;
    SkinFMXImage4: TSkinFMXImage;
    SkinFMXLabel3: TSkinFMXLabel;
    SkinFMXLabel5: TSkinFMXLabel;
    SkinFMXLabel4: TSkinFMXLabel;
    SkinFMXLabel6: TSkinFMXLabel;
    SkinFMXButton17: TSkinFMXButton;
    SkinFMXButton20: TSkinFMXButton;
    SkinFMXButton21: TSkinFMXButton;
    SkinFMXButton22: TSkinFMXButton;
    SkinFMXButton23: TSkinFMXButton;
    SkinFMXButton24: TSkinFMXButton;
    SkinFMXButton25: TSkinFMXButton;
    SkinFMXImage6: TSkinFMXImage;
    tmrSync: TTimer;
    procedure SkinFMXButton6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SkinFMXListBox1PrepareDrawItem(Sender: TObject;
      Canvas: TDrawCanvas; ItemDesignerPanel: TSkinFMXItemDesignerPanel;
      Item: TSkinItem; ItemRect: TRect);
    procedure SkinFMXListBox1VertScrollBarMinOverRangePosValueChange(
      Sender: TObject; NextValue:Double;
                                          LastValue:Double;
                                          Step:Double;var NewValue: Double; var CanChange: Boolean);
    procedure tmrSyncTimer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Self.SkinFMXItemDesignerPanel1.Visible:=False;
  Self.TabControl1.TabPosition:=TTabPosition.tpNone;
end;

procedure TfrmMain.SkinFMXButton6Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.SkinFMXListBox1PrepareDrawItem(Sender: TObject;
  Canvas: TDrawCanvas; ItemDesignerPanel: TSkinFMXItemDesignerPanel;
  Item: TSkinItem; ItemRect: TRect);
begin
  //设置每个单据的状态
  Self.SkinFMXImage4.Properties.Picture.ImageIndex:=
    Self.SkinFMXListBox1.Properties.Items.IndexOf(Item) mod 4;
end;

procedure TfrmMain.SkinFMXListBox1VertScrollBarMinOverRangePosValueChange(
  Sender: TObject; NextValue:Double;
                                          LastValue:Double;
                                          Step:Double;var NewValue: Double; var CanChange: Boolean);
begin

  //如果用户拖动越界超过100时，开始刷新
  if (NewValue<10) then
  begin
    Self.SkinFMXImage6.Visible:=False;
  end;
  if (TScrollBarProperties(Sender).ControlGestureManager.IsUserDraging) then
  begin
    if (NewValue<10) then
    begin
      //隐藏
      Self.SkinFMXImage6.Visible:=False;
    end
    else if (Self.SkinFMXListBox1.GetVertScrollBarIntf.ScrollBarProperties.ControlGestureManager.CalcMinOverRangePosValue<=SkinFMXImage6.Properties.Picture.Height) then
    begin
      //慢慢移下来,变大
      Self.SkinFMXImage6.Properties.Rotated:=False;
      Self.SkinFMXImage6.Width:=Self.SkinFMXListBox1.GetVertScrollBarIntf.ScrollBarProperties.ControlGestureManager.CalcMinOverRangePosValue;
      Self.SkinFMXImage6.Height:=Self.SkinFMXListBox1.GetVertScrollBarIntf.ScrollBarProperties.ControlGestureManager.CalcMinOverRangePosValue;
      Self.SkinFMXImage6.Left:=Ceil(Self.SkinFMXListBox1.Width-Self.SkinFMXImage6.Width) div 2;
      if NewValue-20<0 then
      begin
        Self.SkinFMXImage6.Top:=NewValue-20;
      end
      else
      begin
        Self.SkinFMXImage6.Top:=0;
      end;
      Self.SkinFMXImage6.Visible:=True;
    end
    else if (Self.SkinFMXListBox1.GetVertScrollBarIntf.ScrollBarProperties.ControlGestureManager.CalcMinOverRangePosValue>
      SkinFMXImage6.Properties.Picture.Height+20)
    then
    begin
      if Not Self.tmrSync.Enabled then
      begin
        Self.SkinFMXImage6.Properties.Rotated:=True;
        Self.tmrSync.Enabled:=True;
      end;
    end;
  end;


  //在刷新的时候松开鼠标，保持一定的值
  if (NewValue<150)
    and Self.tmrSync.Enabled
  then
  begin
    NewValue:=150;
    //并停止回滚到初始
    TScrollBarProperties(Sender).ControlGestureManager.ScrollingToInitialAnimator.Pause;
  end;

end;

procedure TfrmMain.tmrSyncTimer(Sender: TObject);
begin

  Self.SkinFMXImage6.Properties.Rotated:=False;
  Self.SkinFMXImage6.Visible:=False;

  Self.tmrSync.Enabled:=False;
  Self.SkinFMXListBox1.GetVertScrollBarIntf.ScrollBarProperties.ControlGestureManager.ScrollingToInitialAnimator.Continue;

end;

end.
