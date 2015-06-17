unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  uBaseList,
  uFileCommon,
  uDrawCanvas,
  uSkinItems,
  uFuncCommon,
  uSkinPicture,
  uDrawEngine,
  uSkinListBoxType,

  uSkinFireMonkeyImage, uSkinFireMonkeyButton, uSkinFireMonkeyLabel,
  uSkinFireMonkeyPanel, uSkinFireMonkeyItemDesignerPanel,
  uSkinFireMonkeyControl, uSkinFireMonkeyScrollControl, uSkinFireMonkeyListBox,
  uDrawPicture, uSkinImageList, uSkinFireMonkeyVirtualList;

type
  TMyData=class
  public
    //左边图片文件(存到ListBoxItem.Icon里面)
    LeftPicFileName:String;
    //右上图片文件
    RightTopPicFileName:String;
    RightTopPicture:TSkinPicture;
    //右下图片文件
    RightBottomPicFileName:String;
    RightBottomPicture:TSkinPicture;
  public
    destructor Destroy;override;
  end;


  TfrmMain = class(TForm)
    lbExample: TSkinFMXListBox;
    ItemItem: TSkinFMXItemDesignerPanel;
    imgSearchBarLeft: TSkinFMXImage;
    imgSearchBarRightBottom: TSkinFMXImage;
    imgSearchBarRightTop: TSkinFMXImage;
    imglistRightTop: TSkinImageList;
    imglistRightBottom: TSkinImageList;
    imglistLeft: TSkinImageList;
    procedure lbExamplePrepareDrawItem(Sender: TObject; Canvas: TDrawCanvas;
      ItemDesignerPanel: TSkinFMXItemDesignerPanel; Item: TSkinItem;
      ItemRect: TRect);
  private
    FMyDataList:TBaseList;
    { Private declarations }
  public
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

{ TfrmMain }

constructor TfrmMain.Create(AOwner: TComponent);
var
  AMyData:TMyData;
  AListItem:TSkinListBoxItem;
  I: Integer;
begin
  inherited;
  FMyDataList:=TBaseList.Create;


  //先把ImageList中的图片保存到文件夹，做为DEMO测试
  for I := 0 to Self.imglistLeft.PictureList.Count-1 do
  begin
    imglistLeft.PictureList[I].SaveToFile(uFileCommon.GetApplicationPath+'Left_'+IntToStr(I)+'.png');
  end;
  for I := 0 to Self.imglistRightTop.PictureList.Count-1 do
  begin
    imglistRightTop.PictureList[I].SaveToFile(uFileCommon.GetApplicationPath+'RightTop_'+IntToStr(I)+'.png');
  end;
  for I := 0 to Self.imglistRightBottom.PictureList.Count-1 do
  begin
    imglistRightBottom.PictureList[I].SaveToFile(uFileCommon.GetApplicationPath+'RightBottom_'+IntToStr(I)+'.png');
  end;



  //创建自定义数据绑到ListItem
  Self.lbExample.Properties.Items.BeginUpdate;
  try
    for I := 0 to 12 do
    begin

      AListItem:=Self.lbExample.Properties.Items.Add;


      //创建自定义数据
      AMyData:=TMyData.Create;
      AMyData.LeftPicFileName:='Left_'+IntToStr(I)+'.png';
      AMyData.RightTopPicFileName:='RightTop_'+IntToStr(I)+'.png';
      AMyData.RightBottomPicFileName:='RightBottom_'+IntToStr(I)+'.png';


      //把自定义的类绑定到ListBoxItem的Data属性中
      AListItem.DataObject:=AMyData;

    end;
  finally
    Self.lbExample.Properties.Items.EndUpdate();
  end;

end;

destructor TfrmMain.Destroy;
begin
  FreeAndNil(FMyDataList);
  inherited;
end;

procedure TfrmMain.lbExamplePrepareDrawItem(Sender: TObject;
  Canvas: TDrawCanvas; ItemDesignerPanel: TSkinFMXItemDesignerPanel;
  Item: TSkinItem; ItemRect: TRect);
var
  AMyData:TMyData;
begin
  //绘制事件
  AMyData:=TMyData(Item.DataObject);

  //判断有没有加载过图片，
  //如果加载过就不再加载
  //如果没有加载，那么就加载一次
  if Item.Icon.IsEmpty then
  begin
    //加载
    Item.Icon.LoadFromFile(uFileCommon.GetApplicationPath+AMyData.LeftPicFileName);
  end;

  if AMyData.RightTopPicture=nil then
  begin
    AMyData.RightTopPicture:=uDrawEngine.CreateCurrentEngineSkinPicture;
    AMyData.RightTopPicture.LoadFromFile(uFileCommon.GetApplicationPath+AMyData.RightTopPicFileName);
  end;

  if AMyData.RightBottomPicture=nil then
  begin
    AMyData.RightBottomPicture:=uDrawEngine.CreateCurrentEngineSkinPicture;
    AMyData.RightBottomPicture.LoadFromFile(uFileCommon.GetApplicationPath+AMyData.RightBottomPicFileName);
  end;


  Self.imgSearchBarLeft.Properties.Picture.PictureDrawType:=pdtReference;
  Self.imgSearchBarLeft.Properties.Picture.RefPicture:=Item.Icon;

  Self.imgSearchBarRightTop.Properties.Picture.PictureDrawType:=pdtReference;
  Self.imgSearchBarRightTop.Properties.Picture.RefPicture:=AMyData.RightTopPicture;

  Self.imgSearchBarRightBottom.Properties.Picture.PictureDrawType:=pdtReference;
  Self.imgSearchBarRightBottom.Properties.Picture.RefPicture:=AMyData.RightBottomPicture;


end;

{ TMyData }

destructor TMyData.Destroy;
begin
  //右上图片文件
  FreeAndNil(RightTopPicture);
  //右下图片文件
  FreeAndNil(RightBottomPicture);

  inherited;
end;

end.
