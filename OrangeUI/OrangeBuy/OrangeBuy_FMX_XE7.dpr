program OrangeBuy_FMX_XE7;

















{$R *.dres}

uses
  System.StartUpCopy,
  FMX.Forms,
  uUIFunction in '..\..\OrangeProjectCommon\uUIFunction.pas',
  MainFrame in 'MainFrame.pas' {FrameMain: TFrame},
  HomeFrame in 'HomeFrame.pas' {FrameHome: TFrame},
  SearchFrame in 'SearchFrame.pas' {FrameSearch: TFrame},
  ClassifyFrame in 'ClassifyFrame.pas' {FrameClassify: TFrame},
  MyFrame in 'MyFrame.pas' {FrameMy: TFrame},
  CartFrame in 'CartFrame.pas' {FrameCart: TFrame},
  MainForm in 'MainForm.pas' {frmMain},
  LoginFrame in 'LoginFrame.pas' {FrameLogin: TFrame},
  SubClassifyFrame in 'SubClassifyFrame.pas' {FrameSubClassify: TFrame},
  ClassifyProductListFrame in 'ClassifyProductListFrame.pas' {FrameClassifyProductList: TFrame},
  ProductInfoFrame in 'ProductInfoFrame.pas' {FrameProductInfo: TFrame},
  ThemeProductListFrame in 'ThemeProductListFrame.pas' {FrameThemeProductList: TFrame};

{$R *.res}

begin
  ReportMemoryLeaksONShutdown:=DebugHook<>0;
  //'/Users/ggggcexx/Library/Application Support/iPhone Simulator/7.1/Applications/
  //D5A56985-6DE0-4041-B217-E8000AA2A0F1/OrangeBuy_FMX.app/StartUp/'
  //'/Users/ggggcexx/Library/Application Support/iPhone Simulator/7.1/Applications/
  //D5A56985-6DE0-4041-B217-E8000AA2A0F1'
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  //  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
