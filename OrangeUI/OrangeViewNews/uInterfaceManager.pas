unit uInterfaceManager;

interface

uses
  Classes,
  SysUtils,
  uFuncCommon,
  uPublic,
  IniFiles,
  Types,
  Math,
  UITypes,
  FMX.Types,
  uBaseLog,
  uTimerTask,
  uBaseList,
  uFileCommon,
  uSkinPicture,
  uDrawEngine,
  uDirectoryPublic,
//  uTimerTask,
  uSkinGIFImage,
  FMX.Graphics,
  uIdHttpControl,
  uInterfaceClass,
  uInterfaceCollection,
  uInterfaceData;


type
  TInterfaceManager=class
  private
    FNewsCategoryDict:TDict;
    FNewsCategoryDictValueList:TDictValueList;
    function GetNewsCategoryDict:TDict;
    function GetNewsCategoryDictValueList:TDictValueList;
  public
    constructor Create;
    destructor Destroy;override;
  public
    //用户名
    UserName:String;
    //密码
    Password:String;

  public
    //新闻分类
    property NewsCategoryDict:TDict read GetNewsCategoryDict;
    property NewsCategoryDictValueList:TDictValueList read GetNewsCategoryDictValueList;
  public
    //下载网络图片
    procedure DoDownloadImage(ATimerTask:TObject);
    procedure DoDownloadImageExecuteEnd(ATimerTask:TObject);
  end;


var
  GlobalInterfaceManager:TInterfaceManager;
  GlobalHttpControl:TIdHttpControl;





//下载网上的图片
function DownloadImage(Name:String;Url:String;LocalPath:String;OnExecuteEnd:TTaskNotify):TTimerTask;


implementation


uses
  MainForm;

var
  DownloadImageThread:TTimerThread;
  DownloadImageHttpControl:THttpControl;

//下载网上的图片
function DownloadImage(Name:String;Url:String;LocalPath:String;OnExecuteEnd:TTaskNotify):TTimerTask;
var
  ATimerTask:TTimerTask;
begin
  if DownloadImageThread=nil then
  begin
    DownloadImageThread:=TTimerThread.Create(True);
  end;
  if DownloadImageHttpControl=nil then
  begin
    DownloadImageHttpControl:=TIdHttpControl.Create;
  end;


//  uBaseLog.OutputDebugString('正在下载'+Name);
  ATimerTask:=TTimerTask.Create(0);
  ATimerTask.OnExecute:=GlobalInterfaceManager.DoDownloadImage;
  ATimerTask.OnExecuteEnd:=OnExecuteEnd;
  ATimerTask.TaskOtherInfo.Add(Name);
  ATimerTask.TaskOtherInfo.Add(Url);
  ATimerTask.TaskOtherInfo.Add(LocalPath);

//  GlobalInterfaceManager.DoDownloadImage(ATimerTask);
//  OnExecuteEnd(ATimerTask);
  DownloadImageThread.RunTask(ATimerTask);

end;

{ TInterfaceManager }

constructor TInterfaceManager.Create;
var
  ADictValue:TDictValue;
begin
  FNewsCategoryDict:=TDict.Create;
  FNewsCategoryDict.module:='news';
  FNewsCategoryDict.code:='category';
  FNewsCategoryDict.name:='新闻分类';
  FNewsCategoryDictValueList:=TDictValueList.Create(FNewsCategoryDict,ooOwned);
  //check","code":"result","name":"检查结果"
  ADictValue:=TDictValue.Create(FNewsCategoryDict);
  ADictValue.name:='最新动态';
  ADictValue.value:='1';
  FNewsCategoryDictValueList.Add(ADictValue);
  ADictValue:=TDictValue.Create(FNewsCategoryDict);
  ADictValue.name:='通知通告';
  ADictValue.value:='2';
  FNewsCategoryDictValueList.Add(ADictValue);
  ADictValue:=TDictValue.Create(FNewsCategoryDict);
  ADictValue.name:='行业规范';
  ADictValue.value:='3';
  FNewsCategoryDictValueList.Add(ADictValue);

end;

destructor TInterfaceManager.Destroy;
begin

  FreeAndNil(FNewsCategoryDict);
  FreeAndNil(FNewsCategoryDictValueList);
  inherited;
end;

procedure TInterfaceManager.DoDownloadImage(ATimerTask: TObject);
const
  Const_ThumbMinSize=100;
var
  AResponseStream:TMemoryStream;

  OriginBitmap:TBitmap;
  ASkinGIFImage:TSkinGIFImage;
  ThumbBitmap:TBitmap;
  Scale:Double;
begin

  //下载图片
  AResponseStream:=TMemoryStream.Create;
  try
    try
      if DownloadImageHttpControl.Get(TTimerTask(ATimerTask).TaskOtherInfo[1],AResponseStream) then
      begin


        AResponseStream.Position:=0;
        CreateFileDir(TTimerTask(ATimerTask).TaskOtherInfo[2]);
        //生成缩略图
        AResponseStream.Position:=0;


        OriginBitmap:=nil;
        ThumbBitmap:=nil;
        OriginBitmap:=TBitmap.Create;


        //保持比例
        if uSkinGIFImage.TGIFHeader.Check(AResponseStream) then
        begin
          //GIF图片
          ASkinGIFImage:=uSkinGIFImage.TSkinGIFImage.Create;
          try
            AResponseStream.Position:=0;
            ASkinGIFImage.LoadFromStream(AResponseStream);

            OriginBitmap.Assign(ASkinGIFImage.Bitmap);
            if (OriginBitmap.Width<Const_ThumbMinSize) or (OriginBitmap.Height<Const_ThumbMinSize) then
            begin
                ThumbBitmap:=TBitmap.Create(OriginBitmap.Width,OriginBitmap.Height);
                ThumbBitmap.Assign(OriginBitmap);
            end
            else
            begin
              if OriginBitmap.Width>OriginBitmap.Height then
              begin
                Scale:=OriginBitmap.Height/Const_ThumbMinSize;
              end
              else
              begin
                Scale:=OriginBitmap.Width/Const_ThumbMinSize;
              end;
              ThumbBitmap:=TBitmap.Create(Ceil(OriginBitmap.Width/Scale),Ceil(OriginBitmap.Height/Scale));

            end;
          finally
            FreeAndNil(ASkinGIFImage);
          end;
        end
        else
        begin

//        uBaseLog.HandleException(nil,'Main','uInterfaceManager',
//          'TInterfaceManager.DoDownloadImageExecuteEnd','AResponseStream.Size:'+IntToStr(AResponseStream.Size));

          AResponseStream.Position:=0;
          OriginBitmap.LoadFromStream(AResponseStream);

//        uBaseLog.HandleException(nil,'Main','uInterfaceManager',
//          'TInterfaceManager.DoDownloadImageExecuteEnd','OriginBitmap.Width:'+IntToStr(OriginBitmap.Width));
//        uBaseLog.HandleException(nil,'Main','uInterfaceManager',
//          'TInterfaceManager.DoDownloadImageExecuteEnd','OriginBitmap.Height:'+IntToStr(OriginBitmap.Height));
//        uBaseLog.HandleException(nil,'Main','uInterfaceManager',
//          'TInterfaceManager.DoDownloadImageExecuteEnd','ThumbBitmap.Width:'+IntToStr(ThumbBitmap.Width));
//        uBaseLog.HandleException(nil,'Main','uInterfaceManager',
//          'TInterfaceManager.DoDownloadImageExecuteEnd','ThumbBitmap.Height:'+IntToStr(ThumbBitmap.Height));

          if (OriginBitmap.Width<Const_ThumbMinSize) or (OriginBitmap.Height<Const_ThumbMinSize) then
          begin
              ThumbBitmap:=TBitmap.Create(OriginBitmap.Width,OriginBitmap.Height);
              ThumbBitmap.Assign(OriginBitmap);
          end
          else
          begin
              if OriginBitmap.Width>OriginBitmap.Height then
              begin
                Scale:=OriginBitmap.Height/Const_ThumbMinSize;
              end
              else
              begin
                Scale:=OriginBitmap.Width/Const_ThumbMinSize;
              end;
              ThumbBitmap:=TBitmap.Create(Ceil(OriginBitmap.Width/Scale),Ceil(OriginBitmap.Height/Scale));

          end;
        end;





        TTimerTask(ATimerTask).TaskObject:=OriginBitmap;
        TTimerTask(ATimerTask).TaskObject1:=ThumbBitmap;

      end;
    finally
      FreeAndNil(AResponseStream);
    end;
  except
    on E:Exception do
    begin
      uBaseLog.HandleException(E,'Main','uInterfaceManager','TInterfaceManager.DoDownloadImage','下载图片'
                                +TTimerTask(ATimerTask).TaskOtherInfo[1]
                                +' 保存图片'
                                +TTimerTask(ATimerTask).TaskOtherInfo[2]);
    end;
  end;
end;


procedure TInterfaceManager.DoDownloadImageExecuteEnd(ATimerTask: TObject);
var
  OriginBitmap:TBitmap;
  ThumbBitmap:TBitmap;
begin
  try
    try


        OriginBitmap:=TBitmap(TTimerTask(ATimerTask).TaskObject);
        ThumbBitmap:=TBitmap(TTimerTask(ATimerTask).TaskObject1);
        if (ThumbBitmap<>nil) and (OriginBitmap<>nil) then
        begin

          ThumbBitmap.Canvas.BeginScene;
          try
            ThumbBitmap.Canvas.Clear(TAlphaColorRec.White);
      uBaseLog.HandleException(nil,'Main','uInterfaceManager',
        'TInterfaceManager.DoDownloadImageExecuteEnd','OriginBitmap.Width:'+IntToStr(OriginBitmap.Width));
      uBaseLog.HandleException(nil,'Main','uInterfaceManager',
        'TInterfaceManager.DoDownloadImageExecuteEnd','OriginBitmap.Height:'+IntToStr(OriginBitmap.Height));
      uBaseLog.HandleException(nil,'Main','uInterfaceManager',
        'TInterfaceManager.DoDownloadImageExecuteEnd','ThumbBitmap.Width:'+IntToStr(ThumbBitmap.Width));
      uBaseLog.HandleException(nil,'Main','uInterfaceManager',
        'TInterfaceManager.DoDownloadImageExecuteEnd','ThumbBitmap.Height:'+IntToStr(ThumbBitmap.Height));
            ThumbBitmap.Canvas.DrawBitmap(OriginBitmap,
                        RectF(0,0,OriginBitmap.Width,OriginBitmap.Height),
                        RectF(0,0,ThumbBitmap.Width,ThumbBitmap.Height),1);
          finally
            ThumbBitmap.Canvas.EndScene;
          end;

          ThumbBitmap.SaveToFile(TTimerTask(ATimerTask).TaskOtherInfo[2]);

        end;
    finally
      TTimerTask(ATimerTask).TaskObject:=nil;
      TTimerTask(ATimerTask).TaskObject1:=nil;
      FreeAndNil(OriginBitmap);
      FreeAndNil(ThumbBitmap);
    end;
  except
    on E:Exception do
    begin
      uBaseLog.HandleException(E,'Main','uInterfaceManager','TInterfaceManager.DoDownloadImageExecuteEnd','下载图片'
                                +TTimerTask(ATimerTask).TaskOtherInfo[1]
                                +' 生成图片'
                                +TTimerTask(ATimerTask).TaskOtherInfo[2]);
    end;
  end;
end;

function TInterfaceManager.GetNewsCategoryDict: TDict;
begin
  Result:=FNewsCategoryDict;
end;

function TInterfaceManager.GetNewsCategoryDictValueList: TDictValueList;
begin
  Result:=FNewsCategoryDictValueList;
end;



initialization
  GlobalInterfaceManager:=TInterfaceManager.Create;

  GlobalHttpControl:=TIdHttpControl.Create;


  DownloadImageThread:=nil;
  DownloadImageHttpControl:=nil;

finalization
  FreeAndNil(DownloadImageThread);
  FreeAndNil(DownloadImageHttpControl);

  FreeAndNil(GlobalHttpControl);
  FreeAndNil(GlobalInterfaceManager);

end.
