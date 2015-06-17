program OrangeShopping_FMX_XE7;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {frmMain},
  MainFrame in 'MainFrame.pas' {FrameMain: TFrame},
  uUIFunction in '..\..\OrangeProjectCommon\uUIFunction.pas',
  HomeFrame in 'HomeFrame.pas' {FrameHome: TFrame},
  ClassifyFrame in 'ClassifyFrame.pas' {FrameClassify: TFrame},
  ShoppingCartFrame in 'ShoppingCartFrame.pas' {FrameShoppingCart: TFrame},
  MyFrame in 'MyFrame.pas' {FrameMy: TFrame},
  KindProductListFrame in 'KindProductListFrame.pas' {FrameKindProductList: TFrame},
  ProductInfoFrame in 'ProductInfoFrame.pas' {FrameProductInfo: TFrame},
  ClassifyProductListFrame in 'ClassifyProductListFrame.pas' {FrameClassifyProductList: TFrame},
  BuyProductFrame in 'BuyProductFrame.pas' {FrameBuyProduct: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
