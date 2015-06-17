unit NewsListFrame;

interface

uses
  System.SysUtils,
  uFuncCommon,
  System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  uSkinImageList,
  uSkinFireMonkeyLabel,
  uSkinFireMonkeyItemDesignerPanel,
  uSkinFireMonkeyScrollControl,
  uSkinFireMonkeyListBox,
  uSkinFireMonkeyButton,
  uSkinFireMonkeyImage,
  uSkinFireMonkeyControl,
  uSkinFireMonkeyPanel,
  uTimerTask,
  uDrawCanvas,
  uSkinItems,
  uBaseLog,
  uBaseList,
  uSkinListBoxType,
  uSkinImageType,
  uDirectoryPublic,
  uInterfaceCollection,
  uInterfaceClass,
  uIdHttpControl,
  uInterfaceManager,
  uInterfaceData, uSkinFireMonkeyPullLoadPanel, uSkinFireMonkeyNotifyNumberIcon,
  uSkinPullLoadPanelType;

type
  TFrameNewsList = class(TFrame)
    lbNewsList: TSkinFMXListBox;
    ItemNewsDefault: TSkinFMXItemDesignerPanel;
    imgImage: TSkinFMXImage;
    lblTitle: TSkinFMXLabel;
    lblIntro: TSkinFMXLabel;
    lblDate: TSkinFMXLabel;
    lblCategory: TSkinFMXLabel;
    plpBottom: TSkinFMXPullLoadPanel;
    lblBottomLoadMore: TSkinFMXLabel;
    imgBottomLoadMore: TSkinFMXImage;
    plpTop: TSkinFMXPullLoadPanel;
    lblTopLoading: TSkinFMXLabel;
    lblTopLoadInfo: TSkinFMXLabel;
    imgTopLoading: TSkinFMXImage;
    imgTopLoadHint: TSkinFMXImage;
    nniUnRead: TSkinFMXNotifyNumberIcon;
    idpNewsNoImage: TSkinFMXItemDesignerPanel;
    lblTitle1: TSkinFMXLabel;
    lblIntro1: TSkinFMXLabel;
    lblDate1: TSkinFMXLabel;
    lblCategory1: TSkinFMXLabel;
    nniUnRead1: TSkinFMXNotifyNumberIcon;
    procedure lbNewsListPrepareDrawItem(Sender: TObject; Canvas: TDrawCanvas;
      ItemDesignerPanel: TSkinFMXItemDesignerPanel; Item: TSkinItem;
      ItemRect: TRect);
    procedure lbNewsListClickItem(Sender: TObject);
    procedure plpTopExecuteLoad(Sender: TObject);
    procedure plpBottomExecuteLoad(Sender: TObject);
    procedure lbNewsListResize(Sender: TObject);
  private
    NewsCategory:TDictValue;
    FNewsList:TBaseList;
    FInterface_GetNewsList:TInterface_GetNewsList;
    procedure DoFirstLoadNewsList(ATimerTask:TObject);
    procedure OnFirstLoadNewsListExecuteEnd(ATimerTask:TObject);
    procedure DoMoreLoadNewsList(ATimerTask:TObject);
    procedure OnMoreLoadNewsListExecuteEnd(ATimerTask:TObject);
    procedure OnDownloadNewsImageExecuteEnd(ATimerTask:TObject);
    { Private declarations }
  public
    IsFirstLoadNewsList:Boolean;
    OnClickNews:TNotifyEvent;
    procedure Init(ANewsCategory:TDictValue);
    procedure FirstLoadNewsList;
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;

    { Public declarations }
  end;

implementation

{$R *.fmx}

uses
  NewsHomeFrame;

{ TFrameNewsList }

constructor TFrameNewsList.Create(AOwner: TComponent);
begin
  inherited;
  FNewsList:=TBaseList.Create;
  Self.lbNewsList.Properties.Items.Clear(True);
  Self.lblTopLoadInfo.Caption:='';


  nniUnRead1.Visible:=False;
  nniUnRead.Visible:=False;


  Self.lbNewsList.VertScrollBar.Properties.ControlGestureManager.IsUseDecideFirstMouseMoveKind:=True;
  Self.lbNewsList.VertScrollBar.Properties.ControlGestureManager.DecideFirstMouseMoveKindPrecision:=30;

end;

destructor TFrameNewsList.Destroy;
begin
  FreeAndNil(FNewsList);
  inherited;
end;

procedure TFrameNewsList.OnDownloadNewsImageExecuteEnd(ATimerTask: TObject);
var
  I: Integer;
begin
  if GlobalNewsHomeFrame=nil then Exit;

  GlobalInterfaceManager.DoDownloadImageExecuteEnd(ATimerTask);

//  uBaseLog.HandleException(nil,'Main','NewsListFrame','TFrameNewsList.OnDownloadNewsImageExecuteEnd','下载图片完成1'
//      +TTimerTask(ATimerTask).TaskOtherInfo[0]);
  //根据ID来确定是否下载完毕
  try
    for I := 0 to Self.lbNewsList.Properties.Items.Count-1 do
    begin
      if TNews(Self.lbNewsList.Properties.Items[I].Data).id=TTimerTask(ATimerTask).TaskOtherInfo[0] then
      begin
//        uBaseLog.HandleException(nil,'Main','NewsListFrame','TFrameNewsList.OnDownloadNewsImageExecuteEnd','下载图片完成2'
//            +TTimerTask(ATimerTask).TaskOtherInfo[0]);
        Self.lbNewsList.Properties.Items[I].FIsBufferBitmapInvalid:=True;
        TNews(Self.lbNewsList.Properties.Items[I].Data).IsDownloadingImage:=False;
        TNews(Self.lbNewsList.Properties.Items[I].Data).IsDownloadedImage:=True;
        Break;
      end;
    end;
  except
    on E:Exception do
    begin
      uBaseLog.HandleException(E,'Main','NewsListFrame','TFrameNewsList.OnDownloadNewsImageExecuteEnd','下载图片完成');
    end;
  end;

  //显示在列表中
  Self.lbNewsList.Invalidate;
end;


procedure TFrameNewsList.DoFirstLoadNewsList(ATimerTask:TObject);
begin
  FInterface_GetNewsList:=TInterface_GetNewsList.Create;
  //调用接口
  if CallInterface(GlobalInterfaceSetting,
                FInterface_GetNewsList,
                GlobalHttpControl,
                ['username','from','region_code','offset','limit','category'],
                ['czadmin','1','3204','1','20',NewsCategory.value],[],[]) then
  begin
    Self.IsFirstLoadNewsList:=True;
  end;
end;

procedure TFrameNewsList.DoMoreLoadNewsList(ATimerTask:TObject);
begin
  FInterface_GetNewsList:=TInterface_GetNewsList.Create;
  //调用接口
  if CallInterface(GlobalInterfaceSetting,
                FInterface_GetNewsList,
                GlobalHttpControl,
                ['username','from','region_code','offset','limit','category'],
                ['czadmin','1','3204',IntToStr(Self.lbNewsList.Properties.Items.Count+1),'20',NewsCategory.value],[],[]) then
  begin

  end;
end;

procedure TFrameNewsList.OnFirstLoadNewsListExecuteEnd(ATimerTask: TObject);
var
  I: Integer;
  AListBoxItem:TSkinListBoxItem;
begin
  if FInterface_GetNewsList.CallResult then
  begin
    Self.lbNewsList.Properties.Items.BeginUpdate;
    try
      Self.lbNewsList.Properties.Items.Clear(True);
      FNewsList.Clear(True);
      if FInterface_GetNewsList.result_<>nil then
      begin
        for I := 0 to FInterface_GetNewsList.result_.Count-1 do
        begin
          AListBoxItem:=Self.lbNewsList.Properties.Items.Add;
          AListBoxItem.Data:=
            FInterface_GetNewsList.result_[I];
          if FInterface_GetNewsList.result_[I].image<>'' then
          begin
            AListBoxItem.ItemType:=sitDefault;
          end
          else
          begin
            AListBoxItem.ItemType:=sitItem1;
          end;
          FNewsList.Add(FInterface_GetNewsList.result_[I]);
        end;
        FInterface_GetNewsList.result_.Clear(False);
      end;
    finally
      Self.lbNewsList.Properties.Items.EndUpdate;
    end;
    Self.lblTopLoadInfo.Caption:='最后更新:'+FormatDateTime('MM-DD HH:MM',Now);
  end;
  FreeAndNil(FInterface_GetNewsList);

  Self.plpTop.Properties.StopLoad;
end;

procedure TFrameNewsList.OnMoreLoadNewsListExecuteEnd(ATimerTask: TObject);
var
  I: Integer;
  AListBoxItem:TSkinListBoxItem;
begin
  if FInterface_GetNewsList.CallResult then
  begin
    Self.lbNewsList.Properties.Items.BeginUpdate;
    try
      if FInterface_GetNewsList.result_<>nil then
      begin
        for I := 0 to FInterface_GetNewsList.result_.Count-1 do
        begin
          AListBoxItem:=Self.lbNewsList.Properties.Items.Add;
          AListBoxItem.Data:=
            FInterface_GetNewsList.result_[I];
          if FInterface_GetNewsList.result_[I].image<>'' then
          begin
            AListBoxItem.ItemType:=sitDefault;
          end
          else
          begin
            AListBoxItem.ItemType:=sitItem1;
          end;
          FNewsList.Add(FInterface_GetNewsList.result_[I]);
        end;
        FInterface_GetNewsList.result_.Clear(False);
      end;
    finally
      Self.lbNewsList.Properties.Items.EndUpdate;
    end;
  end;
  FreeAndNil(FInterface_GetNewsList);


  Self.plpBottom.Properties.StopLoad;
end;

procedure TFrameNewsList.plpBottomExecuteLoad(Sender: TObject);
var
  ATimerTask:TTimerTask;
begin
  //加载更多
  ATimerTask:=TTimerTask.Create(0);
  ATimerTask.OnExecute:=DoMoreLoadNewsList;
  ATimerTask.OnExecuteEnd:=OnMoreLoadNewsListExecuteEnd;
  GetGlobalTimerThread.RunTask(ATimerTask);
end;

procedure TFrameNewsList.plpTopExecuteLoad(Sender: TObject);
var
  ATimerTask:TTimerTask;
begin
  //刷新
  ATimerTask:=TTimerTask.Create(0);
  ATimerTask.OnExecute:=DoFirstLoadNewsList;
  ATimerTask.OnExecuteEnd:=OnFirstLoadNewsListExecuteEnd;
  GetGlobalTimerThread.RunTask(ATimerTask);
end;

procedure TFrameNewsList.FirstLoadNewsList;
begin
  if Not IsFirstLoadNewsList then
  begin
    Self.plpTop.Properties.StartLoad;
  end;
end;

procedure TFrameNewsList.Init(ANewsCategory: TDictValue);
begin
  NewsCategory:=ANewsCategory;
end;

procedure TFrameNewsList.lbNewsListClickItem(Sender: TObject);
var
  ANews:TNews;
begin
//  Exit;
  //显示新闻详细
  ANews:=TNews(TSkinItem(Sender).Data);
  if ANews<>nil then
  begin
    if Assigned(Self.OnClickNews) then
    begin
      Self.OnClickNews(ANews);
    end;
  end;
end;

procedure TFrameNewsList.lbNewsListPrepareDrawItem(Sender: TObject;
  Canvas: TDrawCanvas; ItemDesignerPanel: TSkinFMXItemDesignerPanel;
  Item: TSkinItem; ItemRect: TRect);
var
  ANews:TNews;
begin

  ANews:=TNews(Item.Data);
  if (ANews<>nil) then
  begin
    if (Item.ItemType=sitDefault) then
    begin
      //有图片的新闻
      Self.lblTitle.StaticCaption:=ANews.title;
      Self.lblIntro.StaticCaption:=ANews.intro;
      Self.lblDate.StaticCaption:=ANews.date;
      Self.lblCategory.StaticCaption:=NewsCategory.name;


      //是否是未读的新闻
      nniUnRead.DirectUIVisible:=(ANews.is_read=0);

      //判断图片有没有存在,如果不存在,那么下载
      Self.imgImage.Properties.Picture.StaticRefPicture:=nil;


      if not ANews.IsDownloadingImage
        and Item.Icon.IsEmpty
        and FileExists(ANews.LocalImagePath) then
      begin

        //没有在下载图片，而且图片也存在
        //加载图片
        //在线程中加载图片
        Item.Icon.LoadFromFile(ANews.LocalImagePath);


      end;



      if not Item.Icon.IsEmpty then
      begin
        //已经下载了图片
        Self.imgImage.Properties.Picture.StaticRefPicture:=Item.Icon;
      end
      else
//      if not ANews.IsDownloadingImage
//        and FileExists(ANews.LocalImagePath) then
//      begin
//        //没有在下载图片，而且图片也存在
//        if Item.Icon.IsEmpty then
//        begin
//          Item.Icon.LoadFromFile(ANews.LocalImagePath);
//        end;
//        Self.imgImage.Properties.Picture.StaticRefPicture:=Item.Icon;
//      end
//      else
      if//(ANews.image<>'')
        //and
        not ANews.IsDownloadedImage
        and not ANews.IsDownloadingImage then
      begin
        //没在下载图片，那么下载
        ANews.IsDownloadingImage:=True;
        //下载
        DownloadImage(ANews.id,
                      ANews.RemoteImagePath,
                      ANews.LocalImagePath,
                      OnDownloadNewsImageExecuteEnd);
      end;



//      //是否是未读的新闻
//      nniUnRead.Visible:=(ANews.is_read=0);

    end;

    if (Item.ItemType=sitItem1) then
    begin
      //没有图片的新闻
      Self.lblTitle1.StaticCaption:=ANews.title;
      Self.lblIntro1.StaticCaption:=ANews.intro;
      Self.lblDate1.StaticCaption:=ANews.date;
      Self.lblCategory1.StaticCaption:=NewsCategory.name;

      //是否是未读的新闻
      nniUnRead1.DirectUIVisible:=(ANews.is_read=0);
    end;







  end;

end;


procedure TFrameNewsList.lbNewsListResize(Sender: TObject);
begin
  Self.plpTop.Left:=(Self.lbNewsList.WidthInt-Self.plpTop.WidthInt) div 2;
  Self.plpBottom.Left:=(Self.lbNewsList.WidthInt-Self.plpBottom.WidthInt) div 2;
end;

end.


