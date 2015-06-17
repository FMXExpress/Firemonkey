unit CartFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  uSkinFireMonkeyImage, uSkinFireMonkeyControl, uSkinFireMonkeyPanel,
  uSkinFireMonkeyScrollControl, uSkinFireMonkeyListBox, uSkinMaterial,
  uSkinCheckBoxType, uSkinFireMonkeyCheckBox, uSkinFireMonkeyLabel,
  uSkinFireMonkeyButton, uSkinFireMonkeyItemDesignerPanel, uSkinPageControlType,
  uSkinFireMonkeyPageControl, uSkinFireMonkeySwitchPageListPanel,
  uUIFunction,
  LoginFrame,
  Math,
  uComponentType,
  uSkinListBoxType,
  uSkinButtonType, FMX.Controls.Presentation, FMX.Edit, uSkinFireMonkeyEdit;

type
  TFrameCart = class(TFrame,IFrameVirtualKeyboardEvent)
    pnlToolBar1: TSkinFMXPanel;
    defCheckBoxMaterial: TSkinCheckBoxDefaultMaterial;
    SkinFMXPanel2: TSkinFMXPanel;
    SkinFMXPageControl1: TSkinFMXPageControl;
    TSkinFMXTabSheet1: TSkinFMXTabSheet;
    TSkinFMXTabSheet2: TSkinFMXTabSheet;
    lbShoppingCart: TSkinFMXListBox;
    ItemDefault: TSkinFMXItemDesignerPanel;
    chkDefaultChecked: TSkinFMXCheckBox;
    imgDefaultIcon: TSkinFMXImage;
    lblDefaultCaption: TSkinFMXLabel;
    lblDefaultDetail: TSkinFMXLabel;
    lblDefaultDetail1: TSkinFMXLabel;
    pnlBottomBar: TSkinFMXPanel;
    SkinFMXCheckBox1: TSkinFMXCheckBox;
    SkinFMXLabel1: TSkinFMXLabel;
    SkinFMXLabel2: TSkinFMXLabel;
    SkinFMXButton1: TSkinFMXButton;
    imgHead: TSkinFMXImage;
    SkinFMXLabel3: TSkinFMXLabel;
    SkinFMXButton2: TSkinFMXButton;
    SkinFMXButton3: TSkinFMXButton;
    bdmLoginButton: TSkinButtonDefaultMaterial;
    SkinFMXButton4: TSkinFMXButton;
    SkinFMXButton5: TSkinFMXButton;
    SkinFMXLabel4: TSkinFMXLabel;
    SkinFMXLabel5: TSkinFMXLabel;
    ItemFooter: TSkinFMXItemDesignerPanel;
    lblFooterCaption: TSkinFMXLabel;
    SkinFMXImage1: TSkinFMXImage;
    edtEditPrice: TSkinFMXEdit;
    edtBindingEdit: TSkinFMXEdit;
    pnlVirtualKeyboard: TSkinFMXPanel;
    procedure chkDefaultCheckedClick(Sender: TObject);
    procedure chkHeaderFromCaptionClick(Sender: TObject);
    procedure SkinFMXButton2Click(Sender: TObject);
    procedure edtBindingEditStayClick(Sender: TObject);
    procedure lbShoppingCartStopEditingItem(Sender: TObject;
      Item: TSkinListBoxItem; BindingControl, EditControl: TFmxObject);
    procedure edtEditPriceKeyUp(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
    procedure edtEditPriceEnter(Sender: TObject);
  private
    //ÏÔÊ¾ÐéÄâ¼üÅÌ
    procedure DoVirtualKeyboardShow(KeyboardVisible: Boolean; const Bounds: TRect);
    //Òþ²ØÐéÄâ¼üÅÌ
    procedure DoVirtualKeyboardHide(KeyboardVisible: Boolean; const Bounds: TRect);
    { Private declarations }
  public
    constructor Create(AOwner:TComponent);override;
    { Public declarations }
  end;


var
  GlobalCartFrame:TFrameCart;

implementation

{$R *.fmx}


uses
  MainForm,
  MainFrame;


procedure TFrameCart.chkDefaultCheckedClick(Sender: TObject);
begin
//  //
//  if Self.lbShoppingCart.Properties.InteractiveItem<>nil then
//  begin
//    Self.lbShoppingCart.Properties.InteractiveItem.Checked:=
//      not Self.lbShoppingCart.Properties.InteractiveItem.Checked;
//  end;
end;

procedure TFrameCart.chkHeaderFromCaptionClick(Sender: TObject);
begin
//  if Self.lbShoppingCart.Properties.InteractiveItem<>nil then
//  begin
//    Self.lbShoppingCart.Properties.InteractiveItem.Checked:=
//      not Self.lbShoppingCart.Properties.InteractiveItem.Checked;
//  end;
end;

constructor TFrameCart.Create(AOwner: TComponent);
begin
  inherited;
  Self.pnlVirtualKeyboard.Height:=0;

  Self.edtEditPrice.Visible:=False;

  Self.SkinFMXPageControl1.Properties.Orientation:=toNone;
end;

procedure TFrameCart.DoVirtualKeyboardHide(KeyboardVisible: Boolean;const Bounds: TRect);
begin
  Self.pnlVirtualKeyboard.Height:=0;
end;

procedure TFrameCart.DoVirtualKeyboardShow(KeyboardVisible: Boolean;const Bounds: TRect);
var
  AEditingItemRect:TRectF;
begin
  if Bounds.Height-GetGlobalVirtualKeyboardFixer.VirtualKeyboardHideHeight>Self.pnlVirtualKeyboard.Height then
  begin
    Self.pnlVirtualKeyboard.Height:=RectHeight(Bounds)-GetGlobalVirtualKeyboardFixer.VirtualKeyboardHideHeight-50;
//    Self.pnlVirtualKeyboard.Height:=RectHeight(Bounds)-pnlBottomBar.Height
//                                                      -50{MainFrameÖÐPageControlµÄTabHeaderHeight};

    if Self.lbShoppingCart.Properties.EditingItem<>nil then
    begin
      AEditingItemRect:=Self.lbShoppingCart.Properties.ItemsType.ItemRectByItem(Self.lbShoppingCart.Properties.EditingItem);

      Self.lbShoppingCart.VertScrollBar.Properties.Position:=
        AEditingItemRect.Top;
    end;
  end;


end;

procedure TFrameCart.edtBindingEditStayClick(Sender: TObject);
begin
  Self.lbShoppingCart.Properties.StartEditingItem(
          Self.lbShoppingCart.Properties.MouseOverItem,
          edtBindingEdit,
          edtEditPrice,
          edtBindingEdit.SkinControlType.FMouseDownPt.X,
          edtBindingEdit.SkinControlType.FMouseDownPt.Y
          );
  edtEditPrice.Text:=Self.lbShoppingCart.Properties.MouseOverItem.Detail3;

end;

procedure TFrameCart.edtEditPriceEnter(Sender: TObject);
begin
  //

end;

procedure TFrameCart.edtEditPriceKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key=13 then
  begin
    Self.lbShoppingCart.Properties.StopEditingItem;
  end;
end;

procedure TFrameCart.lbShoppingCartStopEditingItem(Sender: TObject;
  Item: TSkinListBoxItem; BindingControl, EditControl: TFmxObject);
begin
  Item.Detail3:=edtEditPrice.Text;
end;

procedure TFrameCart.SkinFMXButton2Click(Sender: TObject);
begin
  HideFrame(GlobalMainFrame);

  ShowFrame(TFrame(GlobalLoginFrame),TFrameLogin,frmMain,nil,nil,nil,Application,True);
  GlobalLoginFrame.FrameHistroy:=CurrentFrameHistroy;
end;

end.
