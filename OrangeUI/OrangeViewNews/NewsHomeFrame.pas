unit NewsHomeFrame;

interface

uses
  System.SysUtils,uFuncCommon, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  uSkinFireMonkeyButton, uSkinFireMonkeyControl, uSkinFireMonkeyPanel,
  uSkinFireMonkeyImage,
  NewsListFrame,
  NewsDetailFrame,
  uInterfaceData,
  uBaseList,
  uBaseLog,
  uInterfaceClass,
  uInterfaceManager,
  uUIFunction, uSkinPageControlType, uSkinFireMonkeyPageControl,
  uSkinFireMonkeySwitchPageListPanel;

type
  TFrameNewsHome = class(TFrame)
    pnlToolBar: TSkinFMXPanel;
    pcNewsCategory: TSkinFMXPageControl;
    tsNew: TSkinFMXTabSheet;
    tsNotice: TSkinFMXTabSheet;
    tsRule: TSkinFMXTabSheet;
    btnSync: TSkinFMXButton;
    procedure btnReturnClick(Sender: TObject);
    procedure pcNewsCategoryChange(Sender: TObject);
    procedure btnSyncClick(Sender: TObject);
  private
    FNewsListFrameList:TBaseList;
    procedure DoClickNews(Sender:TObject);
    { Private declarations }
  public
    FrameHistroy:TFrameHistroy;
  private
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    //加载第一页，最新动态
    procedure LoadFirstCategoryNewsList;
    { Public declarations }
  end;

var
  GlobalNewsHomeFrame:TFrameNewsHome;

implementation

{$R *.fmx}

uses
  MainForm;

procedure TFrameNewsHome.btnReturnClick(Sender: TObject);
begin
  HideFrame(Self);
  ReturnFrame(FrameHistroy);
end;

procedure TFrameNewsHome.btnSyncClick(Sender: TObject);
begin
  //刷新
  if FNewsListFrameList.Count>0 then
  begin
    uBaseLog.HandleException(nil,'Main','NewsHomeFrame','TFrameNewsHome.btnSyncClick','刷新');
//    if Not TFrameNewsList(FNewsListFrameList.Items[0]).IsFirstLoadNewsList then
//    begin
      TFrameNewsList(FNewsListFrameList.Items[Self.pcNewsCategory.Properties.ActivePageIndex]).IsFirstLoadNewsList:=False;
      TFrameNewsList(FNewsListFrameList.Items[Self.pcNewsCategory.Properties.ActivePageIndex]).FirstLoadNewsList;
//    end;
  end;
end;

constructor TFrameNewsHome.Create(AOwner: TComponent);
var
  I: Integer;
  ANewsListFrame:TFrameNewsList;
  ATabSheet: TSkinTabSheet;
begin

  inherited;

  FNewsListFrameList:=TBaseList.Create;



  Self.pcNewsCategory.Properties.SwitchPageListControlGestureManager.CanGestureSwitch:=True;
//  Self.pcNewsCategory.Properties.SwitchPageListControlGestureManager.GestureSwitchLooped:=False;
  Self.pcNewsCategory.Properties.SwitchPageListControlGestureManager.ControlGestureManager.IsUseDecideFirstMouseMoveKind:=True;


  pcNewsCategory.Properties.ClearPages;
  for I := 0 to GlobalInterfaceManager.NewsCategoryDictValueList.Count-1 do
  begin

    ATabSheet := TSkinTabSheet.Create(Self);
    ATabSheet.Name := 'TSkinTabSheet_'+GlobalInterfaceManager.NewsCategoryDictValueList[I].name;
    ATabSheet.Caption := GlobalInterfaceManager.NewsCategoryDictValueList[I].name;
    ATabSheet.Properties.PageControl := Self.pcNewsCategory;
    ATabSheet.ParentMouseEvent:=True;

    ANewsListFrame:=TFrameNewsList.Create(ATabSheet);
    ANewsListFrame.Parent:=ATabSheet;
    ANewsListFrame.Name:='TFrameNewsList_'+GlobalInterfaceManager.NewsCategoryDictValueList[I].name;
    ANewsListFrame.Align:=TAlignLayout.Client;
    ANewsListFrame.OnClickNews:=DoClickNews;
    ANewsListFrame.Init(GlobalInterfaceManager.NewsCategoryDictValueList[I]);
    FNewsListFrameList.Add(ANewsListFrame);

  end;

  Self.pcNewsCategory.Properties.ActivePageIndex:=0;


  Self.LoadFirstCategoryNewsList;

end;

destructor TFrameNewsHome.Destroy;
begin
  FreeAndNil(FNewsListFrameList);


  inherited;
end;

procedure TFrameNewsHome.DoClickNews(Sender: TObject);
begin

  HideFrame(Self);
  //显示新闻详细界面
  ShowFrame(TFrame(GlobalNewsDetailFrame),TFrameNewsDetail,frmMain,nil,nil,nil,Application);
  GlobalNewsDetailFrame.FrameHistroy:=CurrentFrameHistroy;
  GlobalNewsDetailFrame.LoadNews(TNews(Sender));


end;

procedure TFrameNewsHome.LoadFirstCategoryNewsList;
begin
  if FNewsListFrameList.Count>0 then
  begin
//    if Not TFrameNewsList(FNewsListFrameList.Items[0]).IsFirstLoadNewsList then
//    begin
      TFrameNewsList(FNewsListFrameList.Items[0]).FirstLoadNewsList;
//    end;
  end;
end;

procedure TFrameNewsHome.pcNewsCategoryChange(Sender: TObject);
begin
  if Not TFrameNewsList(FNewsListFrameList.Items[Self.pcNewsCategory.Properties.ActivePageIndex]).IsFirstLoadNewsList then
  begin
    TFrameNewsList(FNewsListFrameList.Items[Self.pcNewsCategory.Properties.ActivePageIndex]).FirstLoadNewsList;
  end;

end;

end.
