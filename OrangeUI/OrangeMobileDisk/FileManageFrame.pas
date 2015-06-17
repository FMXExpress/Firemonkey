unit FileManageFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.ListView.Types, FMX.ListView,
  FMX.Layouts,
  FMX.ListBox,
  FMX.Objects,
  Math,
  uFileCommon,
  uFileManage,
  uBaseLog,
  uDrawCanvas,
  uGraphicCommon,
  uSkinMaterial,
  uUIFunction,
  uDrawPicture,
  uSkinScrollBarType,
  uSkinListBoxType,
  uSkinFireMonkeyScrollControl,
  uSkinFireMonkeyListBox,
  uSkinImageList,
  uSkinFireMonkeyImage,
  uSkinFireMonkeyLabel,
  uSkinFireMonkeyControl,
  uSkinFireMonkeyItemDesignerPanel,
  uSkinFireMonkeyButton,uSkinItems,
  uSkinFireMonkeyPanel, uSkinFireMonkeyCheckBox, uSkinFireMonkeyTrackBar,
  uSkinFireMonkeyVirtualList;

type
  TFrameFileManage = class(TFrame)
    idpFile: TSkinFMXItemDesignerPanel;
    lblFileName: TSkinFMXLabel;
    imgFileIcon: TSkinFMXImage;
    imgFileState: TSkinFMXImage;
    lblFileSize: TSkinFMXLabel;
    lblFileDate: TSkinFMXLabel;
    imglistFileStateIcon: TSkinImageList;
    imglistFileExtIcon: TSkinImageList;
    pnlSortToolBar: TSkinFMXPanel;
    btnSortByName: TSkinFMXButton;
    btnSortByDate: TSkinFMXButton;
    btnSortBySize: TSkinFMXButton;
    imglistSortType: TSkinImageList;
    imgBatchToolBar: TSkinFMXImage;
    btnBatchStar: TSkinFMXButton;
    btnBatchMove: TSkinFMXButton;
    btnBatchDel: TSkinFMXButton;
    btnBatchCopy: TSkinFMXButton;
    chkSelected: TSkinFMXCheckBox;
    imgNoFile: TSkinFMXImage;
    lblNoFile: TSkinFMXLabel;
    btnMultiSelected: TSkinFMXButton;
    imgFTPServerState: TSkinImageList;
    lbFileList: TSkinFMXListBox;
    tmrSync: TTimer;
    AniIndicator1: TAniIndicator;
    pnlToolBar: TSkinFMXPanel;
    btnBatchProcess: TSkinFMXButton;
    btnFTPServer: TSkinFMXButton;
    lblDirName: TSkinFMXLabel;
    btnReturn: TSkinFMXButton;
    btnComplete: TSkinFMXButton;
    btnNewFolder: TSkinFMXButton;
    procedure lbFileListPrepareDrawItem(Sender: TObject; Canvas: TDrawCanvas;
      ItemDesignerPanel: TSkinFMXItemDesignerPanel; Item: TSkinItem;
      ItemRect: TRect);
    procedure lbFileListSelectedItem(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure btnBatchProcessClick(Sender: TObject);
    procedure btnReturnClick(Sender: TObject);
    procedure btnSortByNameClick(Sender: TObject);
    procedure btnSortByDateClick(Sender: TObject);
    procedure btnSortBySizeClick(Sender: TObject);
    procedure btnCompleteClick(Sender: TObject);
    procedure chkSelectedClick(Sender: TObject);
    procedure btnBatchDelClick(Sender: TObject);
    procedure btnBatchCopyClick(Sender: TObject);
    procedure btnNewFolderClick(Sender: TObject);
    procedure btnMultiSelectedClick(Sender: TObject);
    procedure btnFTPServerClick(Sender: TObject);
    procedure lbFileListClickItem(Sender: TObject);
    procedure imgNoFileGesture(Sender: TObject;
      const EventInfo: TGestureEventInfo; var Handled: Boolean);
    procedure lbFileListCalcItemDrawDesignerPanel(Sender: TObject;
      var ItemDrawDesignerPanel: TSkinFMXItemDesignerPanel; Item: TObject);
    procedure lbFileListVertScrollBarMinOverRangePosValueChange(Sender: TObject;
      NextValue:Double;
                                          LastValue:Double;
                                          Step:Double;var NewValue:Double;var CanChange: Boolean);
    procedure tmrSyncTimer(Sender: TObject);
    procedure lbFileListResize(Sender: TObject);
  private
    FIsSyncing:Boolean;
    procedure DoNewFolderCreate(Sender:TObject);
    { Private declarations }
  public
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
  public
    FrameHistroy:TFrameHistroy;
    procedure OnReturnFrame(FromFrame:TFrame);
  public

    //当前列表的排序模式
    CurrentCompare: TListSortCompare;

    //批处理框
    FIsBatchProcessing:Boolean;


    //父文件夹
    FParentDir:TFileItem;
    //父文件夹管理
    ParentFileManageFrame:TFrameFileManage;
    //子文件夹管理
    ChildFileManageFrame:TFrameFileManage;


    //加载文件列表
    function LoadFileList(AParentDir:TFileItem):Boolean;
    //更新文件列表
    function UpdateFileList:Boolean;
    { Public declarations }
  end;






var
  GlobalFileManageFrame:TFrameFileManage;
  CurrentFileManageFrame:TFrameFileManage;




implementation





uses
  MainForm,
  HintFrame,
  NewFolderFrame,
  PictureFileFrame,
  TextFileFrame;




{$R *.fmx}


{ TframeFileList }

procedure TFrameFileManage.btnCompleteClick(Sender: TObject);
begin
  FIsBatchProcessing:=False;

  //完成批量处理
  Self.imgBatchToolBar.Visible:=False;
  Self.btnComplete.Visible:=False;
  Self.btnBatchProcess.Visible:=True;
  Self.btnNewFolder.Visible:=False;
  btnMultiSelected.Visible:=False;


  Self.lbFileList.Properties.IsAutoSelected:=True;

  if FParentDir=GlobalRootDir then
  begin
    Self.btnReturn.Visible:=False;
    Self.btnFTPServer.Visible:=True;
  end
  else
  begin
    Self.btnReturn.Visible:=True;
    Self.btnFTPServer.Visible:=False;
    Self.btnReturn.Caption:='返回'+ParentFileManageFrame.FParentDir.FileName;
    Self.btnReturn.Width:=120;
  end;

end;

procedure TFrameFileManage.btnFTPServerClick(Sender: TObject);
begin
  if Not Self.btnFTPServer.Properties.IsPushed then
  begin
    //启动FTPServer
    try
      frmMain.IdFTPServer1.AllowAnonymousLogin:=True;
      frmMain.IdFTPServer1.Active:=True;
      Self.btnFTPServer.Properties.IsPushed:=True;

      ShowHintFrame(Self,'无线U盘已打开!'
                        +'FTP://'+frmMain.IdIPWatch1.LocalIP);
    Except
      On E:Exception do
      begin
        ShowHintFrame(Self,'无线U盘打开失败!');
      end;
    end;
  end
  else
  begin
    //停止FTPServer
    try
      Self.btnFTPServer.Properties.IsPushed:=False;
      ShowHintFrame(Self,'无线U盘已关闭!');
      //frmMain.IdFTPServer1.Active:=False;
    Except
      On E:Exception do
      begin
        ShowHintFrame(Self,'无线U盘关闭失败!');
      end;
    end;
  end;
end;

procedure TFrameFileManage.btnMultiSelectedClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to Self.FParentDir.ChildFileList.Count-1 do
  begin
    Self.FParentDir.ChildFileList[I].OperationRec.Selected:=Self.btnMultiSelected.Properties.IsPushed;
  end;
  Self.lbFileList.Invalidate;
end;

procedure TFrameFileManage.btnNewFolderClick(Sender: TObject);
begin
  HideFrame(Self);
  //新建文件夹
  ShowFrame(TFrame(GlobalNewFolderFrame),TFrameNewFolder,frmMain,nil,nil,OnReturnFrame,Application);
  GlobalNewFolderFrame.FrameHistroy:=CurrentFrameHistroy;
  GlobalNewFolderFrame.LoadFile(Self.FParentDir);
  GlobalNewFolderFrame.OnNewFolderCreate:=DoNewFolderCreate;
end;

procedure TFrameFileManage.btnReturnClick(Sender: TObject);
begin
  HideFrame(Self);
  ReturnFrame(FrameHistroy);
  FreeAndNil(ParentFileManageFrame.ChildFileManageFrame);
end;


function ListCompareBySizeAsc(Item1, Item2: Pointer): Integer;
begin
  Result:=0;
  if TFileItem(TSkinListBoxItem(Item1).Data).Size
    >TFileItem(TSkinListBoxItem(Item2).Data).Size then
  begin
    Result:=1;
  end
  else if TFileItem(TSkinListBoxItem(Item1).Data).Size
          <TFileItem(TSkinListBoxItem(Item2).Data).Size then
  begin
    Result:=-1;
  end;
end;
function ListCompareBySizeDesc(Item1, Item2: Pointer): Integer;
begin
  Result:=0;
  if TFileItem(TSkinListBoxItem(Item1).Data).Size
    >TFileItem(TSkinListBoxItem(Item2).Data).Size then
  begin
    Result:=-1;
  end
  else if TFileItem(TSkinListBoxItem(Item1).Data).Size
          <TFileItem(TSkinListBoxItem(Item2).Data).Size then
  begin
    Result:=1;
  end;
end;


function ListCompareByDateAsc(Item1, Item2: Pointer): Integer;
begin
  Result:=0;
  if TFileItem(TSkinListBoxItem(Item1).Data).FileDate
    >TFileItem(TSkinListBoxItem(Item2).Data).FileDate then
  begin
    Result:=1;
  end
  else if TFileItem(TSkinListBoxItem(Item1).Data).FileDate
          <TFileItem(TSkinListBoxItem(Item2).Data).FileDate then
  begin
    Result:=-1;
  end;
end;
function ListCompareByDateDesc(Item1, Item2: Pointer): Integer;
begin
  Result:=0;
  if TFileItem(TSkinListBoxItem(Item1).Data).FileDate
    >TFileItem(TSkinListBoxItem(Item2).Data).FileDate then
  begin
    Result:=-1;
  end
  else if TFileItem(TSkinListBoxItem(Item1).Data).FileDate
          <TFileItem(TSkinListBoxItem(Item2).Data).FileDate then
  begin
    Result:=1;
  end;
end;



function ListCompareByNameAsc(Item1, Item2: Pointer): Integer;
begin
  Result:=0;
  if LowerCase(TFileItem(TSkinListBoxItem(Item1).Data).FileName)
    >LowerCase(TFileItem(TSkinListBoxItem(Item2).Data).FileName) then
  begin
    Result:=1;
  end
  else if LowerCase(TFileItem(TSkinListBoxItem(Item1).Data).FileName)
          <LowerCase(TFileItem(TSkinListBoxItem(Item2).Data).FileName) then
  begin
    Result:=-1;
  end;
end;
function ListCompareByNameDesc(Item1, Item2: Pointer): Integer;
begin
  Result:=0;
  if LowerCase(TFileItem(TSkinListBoxItem(Item1).Data).FileName)
    >LowerCase(TFileItem(TSkinListBoxItem(Item2).Data).FileName) then
  begin
    Result:=-1;
  end
  else if LowerCase(TFileItem(TSkinListBoxItem(Item1).Data).FileName)
          <LowerCase(TFileItem(TSkinListBoxItem(Item2).Data).FileName) then
  begin
    Result:=1;
  end;
end;




procedure TFrameFileManage.btnSortByDateClick(Sender: TObject);
begin
  Self.btnSortByName.Properties.Icon.ImageIndex:=-1;
  Self.btnSortBySize.Properties.Icon.ImageIndex:=-1;
  Self.btnSortByName.Tag:=0;
  Self.btnSortBySize.Tag:=0;

  if Self.btnSortByDate.Tag=0 then
  begin
    Self.btnSortByDate.Tag:=1;
    Self.btnSortByDate.Properties.Icon.ImageIndex:=0;
    Self.lbFileList.Properties.Items.Sort(ListCompareByDateAsc);
    CurrentCompare:=ListCompareByDateAsc;
  end
  else
  begin
    Self.btnSortByDate.Tag:=0;
    Self.btnSortByDate.Properties.Icon.ImageIndex:=1;
    Self.lbFileList.Properties.Items.Sort(ListCompareByDateDesc);
    CurrentCompare:=ListCompareByDateDesc;
  end;
end;

procedure TFrameFileManage.btnSortByNameClick(Sender: TObject);
begin
  Self.btnSortByDate.Properties.Icon.ImageIndex:=-1;
  Self.btnSortBySize.Properties.Icon.ImageIndex:=-1;
  Self.btnSortByDate.Tag:=0;
  Self.btnSortBySize.Tag:=0;

  if Self.btnSortByName.Tag=0 then
  begin
    Self.btnSortByName.Tag:=1;
    Self.btnSortByName.Properties.Icon.ImageIndex:=0;
    Self.lbFileList.Properties.Items.Sort(ListCompareByNameAsc);
    CurrentCompare:=ListCompareByNameAsc;
  end
  else
  begin
    Self.btnSortByName.Tag:=0;
    Self.btnSortByName.Properties.Icon.ImageIndex:=1;
    Self.lbFileList.Properties.Items.Sort(ListCompareByNameDesc);
    CurrentCompare:=ListCompareByNameDesc;
  end;
end;

procedure TFrameFileManage.btnSortBySizeClick(Sender: TObject);
begin
  Self.btnSortByName.Properties.Icon.ImageIndex:=-1;
  Self.btnSortByDate.Properties.Icon.ImageIndex:=-1;
  Self.btnSortByName.Tag:=0;
  Self.btnSortByDate.Tag:=0;

  if Self.btnSortBySize.Tag=0 then
  begin
    Self.btnSortBySize.Tag:=1;
    Self.btnSortBySize.Properties.Icon.ImageIndex:=0;
    Self.lbFileList.Properties.Items.Sort(ListCompareBySizeAsc);
    CurrentCompare:=ListCompareBySizeAsc;
  end
  else
  begin
    Self.btnSortBySize.Tag:=0;
    Self.btnSortBySize.Properties.Icon.ImageIndex:=1;
    Self.lbFileList.Properties.Items.Sort(ListCompareBySizeDesc);
    CurrentCompare:=ListCompareBySizeDesc;
  end;

end;

procedure TFrameFileManage.chkSelectedClick(Sender: TObject);
begin
  //选中或非选中某一项
  if Self.lbFileList.Properties.InteractiveItem<>nil then
  begin
    TFileItem(TSkinListBoxItem(Self.lbFileList.Properties.InteractiveItem).Data)
        .OperationRec.Selected:=not TFileItem(TSkinListBoxItem(Self.lbFileList.Properties.InteractiveItem).Data)
        .OperationRec.Selected;
  end;
end;

constructor TFrameFileManage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FIsSyncing:=False;


  Self.idpFile.Visible:=False;

  imgBatchToolBar.Visible:=False;
  Self.OnResize:=FrameResize;

  Self.btnBatchMove.Visible:=False;
  Self.btnBatchCopy.Visible:=False;
  Self.btnBatchStar.Visible:=False;
end;

destructor TFrameFileManage.Destroy;
var
  I: Integer;
begin
  Self.idpFile.Free;
//  for I := 0 to Self.lbFileList.ComponentCount-1 do
//  begin
//    uBaseLog.OutputDebugString(Self.lbFileList.Components[I].ClassName);
//  end;
  Self.lbFileList.Free;
  inherited;
end;

procedure TFrameFileManage.DoNewFolderCreate(Sender: TObject);
var
  ANewFolder:TFileItem;
  ListBoxItem:TSkinListBoxItem;
begin
  ANewFolder:=TFileItem(Sender);
  ListBoxItem:=TSkinListBoxItem(Self.lbFileList.Properties.Items.Add);
  ListBoxItem.Data:=ANewFolder;
  ListBoxItem.Icon.SkinImageList:=Self.imglistFileExtIcon;
  ListBoxItem.Icon.PictureDrawType:=TPictureDrawType.pdtImageList;
  if Assigned(CurrentCompare) then
  begin
    Self.lbFileList.Properties.Items.Sort(CurrentCompare);
  end;


  if FParentDir.ChildFileList.Count>0 then
  begin

    //有文件
    Self.imgNoFile.Visible:=False;
    Self.lbFileList.Visible:=True;
    Self.lbFileList.Align:=TAlignLayout.alClient;

  end
  else
  begin

    //无文件
    Self.imgNoFile.Visible:=True;
    Self.lbFileList.Visible:=False;
    Self.imgNoFile.Align:=TAlignLayout.alClient;


  end;


end;

procedure TFrameFileManage.FrameResize(Sender: TObject);
begin
  btnSortByName.Left:=Ceil(Width-(btnSortByName.Width*3)+20) div 2;
  btnSortByDate.Left:=btnSortByName.Left+Ceil(btnSortByName.Width);
  btnSortBySize.Left:=btnSortByDate.Left+Ceil(btnSortByDate.Width);

//
//  Self.btnBatchDel.Left:=0;
//  Self.btnBatchMove.Left:=Self.btnBatchDel.Left+Ceil(Self.btnBatchDel.Width)+Ceil(Width-Self.btnBatchDel.Width*4) div 3;
//  Self.btnBatchCopy.Left:=Self.btnBatchMove.Left+Ceil(Self.btnBatchMove.Width)+Ceil(Width-Self.btnBatchDel.Width*4) div 3;
//  Self.btnBatchStar.Left:=Self.btnBatchCopy.Left+Ceil(Self.btnBatchCopy.Width)+Ceil(Width-Self.btnBatchDel.Width*4) div 3;
//

  lblNoFile.Left:=Ceil(Width-lblNoFile.Width) div 2;

end;

procedure TFrameFileManage.imgNoFileGesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  //
end;

procedure TFrameFileManage.lbFileListCalcItemDrawDesignerPanel(Sender: TObject;
  var ItemDrawDesignerPanel: TSkinFMXItemDesignerPanel; Item: TObject);
begin
//  ItemDrawDesignerPanel:=Self.idpFile;
//  Self.idpFile.HostComponent:=Self.lbFileList;
end;

procedure TFrameFileManage.lbFileListClickItem(Sender: TObject);
var
  AFileItem:TFileItem;
begin
  //查看文件

  AFileItem:=TFileItem(TSkinListBoxItem(Sender).Data);

  if FIsBatchProcessing then
  begin

        //批处理模式，不查看文件
        AFileItem.OperationRec.Selected:=Not AFileItem.OperationRec.Selected;

  end;


end;

procedure TFrameFileManage.lbFileListPrepareDrawItem(Sender: TObject;
  Canvas: TDrawCanvas; ItemDesignerPanel: TSkinFMXItemDesignerPanel;
  Item: TSkinItem; ItemRect: TRect);
  function GetFileExtIconIndex(AFileItem:TFileItem):Integer;
  begin
    Result:=4;
    case AFileItem.FileExtType of
      fetWord: Result:=1;
      fetExcel: Result:=2;
      fetPoint: Result:=3;
      fetPDF: Result:=5;
      fetText: Result:=6;
      fetVideo: Result:=7;
      fetCompress: Result:=8;
      fetAudio: Result:=9;
      fetPicture: Result:=10;
    end;
  end;
var
  FileItem:TFileItem;
  ListBoxItem:TSkinListBoxItem;
begin
  idpFile.Width:=Self.lbFileList.Width;

  ListBoxItem:=TSkinListBoxItem(Item);

  if (ListBoxItem.Data<>nil) and (TObject(ListBoxItem.Data) is TFileItem) then
  begin
    FileItem:=TFileItem(ListBoxItem.Data);

    case FileItem.FileType of
      ftDir:
      begin
        Self.imgFileIcon.Properties.Picture.ImageIndex:=0;
        Self.imgFileIcon.Properties.Picture.SkinImageList:=Self.imglistFileExtIcon;
        Self.imgFileState.Properties.Picture.ImageIndex:=0;
        Self.lblFileSize.Visible:=False;
      end;
      ftFile:
      begin
        Self.imgFileIcon.Properties.Picture.ImageIndex:=GetFileExtIconIndex(FileItem);
        Self.imgFileIcon.Properties.Picture.SkinImageList:=Self.imglistFileExtIcon;
        Self.imgFileState.Properties.Picture.ImageIndex:=2;
        Self.lblFileSize.Visible:=True;
        Self.lblFileSize.Caption:=GetSpeedStr(FileItem.Size);
      end;
    end;

    Self.lblFileName.Caption:=FileItem.FileName;

    Self.lblFileDate.Caption:=DateTimeToStr(FileItem.FileDate);



    //处理复选框选中
    if Self.FIsBatchProcessing then
    begin

      chkSelected.Visible:=True;
      chkSelected.Properties.Checked:=FileItem.OperationRec.Selected;

      Self.imgFileIcon.Left:=chkSelected.Left+Ceil(chkSelected.Width)+5;
      Self.lblFileName.Left:=Self.imgFileIcon.Left+Ceil(Self.imgFileIcon.Width)+5;
      Self.lblFileDate.Left:=Self.lblFileName.Left;

    end
    else
    begin

      chkSelected.Visible:=False;

      Self.imgFileIcon.Left:=chkSelected.Left;
      Self.lblFileName.Left:=Self.imgFileIcon.Left+Ceil(Self.imgFileIcon.Width)+5;
      Self.lblFileDate.Left:=Self.lblFileName.Left;

    end;



  end;

end;

procedure TFrameFileManage.lbFileListResize(Sender: TObject);
begin
  Self.AniIndicator1.Position.X:=(Self.lbFileList.Width-Self.AniIndicator1.Width)/2;
end;

procedure TFrameFileManage.lbFileListSelectedItem(Sender: TObject);
var
  AFileItem:TFileItem;
begin
  //查看文件
  if Self.lbFileList.Properties.SelectedItem<>nil then
  begin
    AFileItem:=TFileItem(Self.lbFileList.Properties.SelectedItem.Data);


    if Not FIsBatchProcessing then
    begin

          if AFileItem.FileType=ftDir then
          begin

            LoadChildFileList(AFileItem);

            HideFrame(Self);
            //显示子文件夹文件列表界面
            ShowFrame(TFrame(ChildFileManageFrame),TFrameFileManage,frmMain,nil,nil,OnReturnFrame,Application);
            ChildFileManageFrame.FrameHistroy:=CurrentFrameHistroy;
            ChildFileManageFrame.ParentFileManageFrame:=Self;
            ChildFileManageFrame.LoadFileList(AFileItem);
            CurrentFileManageFrame:=ChildFileManageFrame;


          end
          else
          begin

            //显示文件
            case AFileItem.FileExtType of
              fetOther: ;
              fetWord: ;
              fetExcel: ;
              fetPoint: ;
              fetPDF: ;
              fetText:
              begin
                HideFrame(Self);
                ShowFrame(TFrame(GlobalTextFileFrame),TFrameTextFile,frmMain,nil,nil,OnReturnFrame,Application);
                GlobalTextFileFrame.FrameHistroy:=CurrentFrameHistroy;
                GlobalTextFileFrame.LoadFile(AFileItem);
              end;
              fetVideo: ;
              fetCompress: ;
              fetAudio: ;
              fetPicture:
              begin
                HideFrame(Self);
                ShowFrame(TFrame(GlobalPictureFileFrame),TFramePictureFile,frmMain,nil,nil,OnReturnFrame,Application);
                GlobalPictureFileFrame.FrameHistroy:=CurrentFrameHistroy;
                GlobalPictureFileFrame.LoadFile(AFileItem);
              end;
            end;

          end;
    end;

  end;
end;

procedure TFrameFileManage.lbFileListVertScrollBarMinOverRangePosValueChange(
  Sender: TObject; NextValue:Double;
                                          LastValue:Double;
                                          Step:Double;var NewValue:Double;var CanChange: Boolean);
begin
//  uBaseLog.OutputDebugString(IntToStr(NewValue)+' '+IntToStr(Ord(TScrollBarProperties(Sender).InertiaScrollDirection)));


  //如果用户拖动越界超过100时，开始刷新
  if (NewValue>100)
    and (TScrollBarProperties(Sender).ControlGestureManager.IsUserDraging)
  then
  begin
    if Not FIsSyncing then
    begin
      FIsSyncing:=True;
      Self.AniIndicator1.Enabled:=True;
      Self.AniIndicator1.Visible:=True;
      Self.tmrSync.Enabled:=True;
    end;
  end;


  //在刷新的时候松开鼠标，保持一定的值
  if (NewValue<100)
    and FIsSyncing
  then
  begin
    NewValue:=100;
    //并停止回滚到初始
    TScrollBarProperties(Sender).ControlGestureManager.ScrollingToInitialAnimator.Pause;
  end;


end;

function TFrameFileManage.LoadFileList(AParentDir:TFileItem): Boolean;
var
  I: Integer;
  ListBoxItem:TSkinListBoxItem;
begin
  Result:=False;

  FParentDir:=AParentDir;


  if FParentDir.ChildFileList.Count>0 then
  begin

    //有文件
    Self.lbFileList.BeginUpdate;
    Self.lbFileList.Properties.Items.BeginUpdate;
    try
      Self.lbFileList.Properties.Items.Clear(True);

      for I := 0 to FParentDir.ChildFileList.Count-1 do
      begin
        ListBoxItem:=TSkinListBoxItem(Self.lbFileList.Properties.Items.Add);
        ListBoxItem.Data:=FParentDir.ChildFileList[I];
        ListBoxItem.Icon.SkinImageList:=Self.imglistFileExtIcon;
        ListBoxItem.Icon.PictureDrawType:=TPictureDrawType.pdtImageList;
      end;

    finally
      Self.lbFileList.Properties.Items.EndUpdate;
      Self.lbFileList.EndUpdate;
    end;

    Self.imgNoFile.Visible:=False;
    Self.lbFileList.Visible:=True;
    Self.lbFileList.Align:=TAlignLayout.alClient;

  end
  else
  begin
    Self.lbFileList.Properties.Items.Clear(True);

    //无文件
    Self.imgNoFile.Visible:=True;
    Self.lbFileList.Visible:=False;
    Self.imgNoFile.Align:=TAlignLayout.alClient;

  end;

  Self.lblDirName.Caption:=FParentDir.FileName;


  Self.btnComplete.Visible:=False;
  Self.btnNewFolder.Visible:=False;
  if FParentDir=GlobalRootDir then
  begin
    Self.btnReturn.Visible:=False;
    Self.btnFTPServer.Visible:=True;
  end
  else
  begin
    Self.btnReturn.Visible:=True;
    Self.btnFTPServer.Visible:=False;
    Self.btnReturn.Caption:='返回'+ParentFileManageFrame.FParentDir.FileName;
    Self.btnReturn.Width:=120;
  end;

  Result:=True;
end;

procedure TFrameFileManage.OnReturnFrame(FromFrame: TFrame);
begin
  CurrentFileManageFrame:=Self;
end;

procedure TFrameFileManage.tmrSyncTimer(Sender: TObject);
begin
  FIsSyncing:=False;

  Self.AniIndicator1.Enabled:=False;
  Self.AniIndicator1.Visible:=False;

  Self.tmrSync.Enabled:=False;
  Self.lbFileList.GetVertScrollBarIntf.ScrollBarProperties.ControlGestureManager.ScrollingToInitialAnimator.Continue;
end;

function TFrameFileManage.UpdateFileList: Boolean;
var
  I: Integer;
  NowParentDir:TFileItem;
  ListBoxItem:TSkinListBoxItem;
begin
  NowParentDir:=TFileItem.Create;
  try
    NowParentDir.FileName:=Self.FParentDir.FileName;
    NowParentDir.FilePath:=Self.FParentDir.FilePath;
    NowParentDir.FileType:=Self.FParentDir.FileType;

    //获取最新的文件列表
    LoadChildFileList(NowParentDir);

    Self.lbFileList.BeginUpdate;
    Self.lbFileList.Properties.Items.BeginUpdate;

    try
      //比对
      //先判断哪些是被删除了的
      for I := Self.lbFileList.Properties.Items.Count-1 downto 0 do
      begin
        //在新文件列表中不存在相同的文件了
        if Not NowParentDir.ChildFileList.HasSameFile(TFileItem(Self.lbFileList.Properties.Items[I].Data)) then
        begin
          //已经被删除掉了
          Self.FParentDir.ChildFileList.Remove(TFileItem(Self.lbFileList.Properties.Items[I].Data),True);
          Self.lbFileList.Properties.Items.Delete(I,True);
        end;
      end;

      //然后再添加新增的
      for I := NowParentDir.ChildFileList.Count-1 downto 0 do
      begin
        if Not Self.FParentDir.ChildFileList.HasSameFile(NowParentDir.ChildFileList[I]) then
        begin
          //新增的
          ListBoxItem:=TSkinListBoxItem(Self.lbFileList.Properties.Items.Add);
          ListBoxItem.Data:=NowParentDir.ChildFileList[I];
          ListBoxItem.Icon.SkinImageList:=Self.imglistFileExtIcon;
          ListBoxItem.Icon.PictureDrawType:=TPictureDrawType.pdtImageList;
          NowParentDir.ChildFileList.Delete(I,False);
        end;
      end;

      if Assigned(CurrentCompare) then
      begin
        Self.lbFileList.Properties.Items.Sort(CurrentCompare);
      end;


    finally
      Self.lbFileList.EndUpdate;
      Self.lbFileList.Properties.Items.EndUpdate;
    end;

  finally
    NowParentDir.Free;

  end;
end;

procedure TFrameFileManage.btnBatchCopyClick(Sender: TObject);
begin
  //复制

end;

procedure TFrameFileManage.btnBatchDelClick(Sender: TObject);
var
  I: Integer;
  HasSelected:Boolean;
begin

  HasSelected:=False;
  for I := 0 to Self.FParentDir.ChildFileList.Count-1 do
  begin
    if Self.FParentDir.ChildFileList[I].OperationRec.Selected then
    begin
      HasSelected:=True;
    end;
  end;

  if Not HasSelected then
  begin
    Exit;
  end;


  //删除文件
//  if MessageDlg('确实要删除这些文件吗?',
//                TMsgDlgType.mtConfirmation,
//                mbOKCancel,0)=idOK then
//  begin
    Self.lbFileList.BeginUpdate;
    Self.lbFileList.Properties.Items.BeginUpdate;
    try

      //确定
      for I := Self.FParentDir.ChildFileList.Count-1 downto 0 do
      begin
        if Self.FParentDir.ChildFileList[I].OperationRec.Selected then
        begin
          if (Self.FParentDir.ChildFileList[I].FileType=ftDir) then
          begin
            //删除文件夹
            if RemoveDir(Self.FParentDir.ChildFileList[I].FilePath) then
            begin
              Self.FParentDir.ChildFileList.Remove(TObject(Self.lbFileList.Properties.Items[I].Data),True);
              Self.lbFileList.Properties.Items.Delete(I,True);
            end
            else
            begin
//              MessageDlg('删除文件夹'+Self.FParentDir.ChildFileList[I].FileName+'失败!',TMsgDlgType.mtError,[TMsgDlgBtn.mbOK],0);
            end;
          end
          else if (Self.FParentDir.ChildFileList[I].FileType=ftFile) then
          begin
            //删除文件
            if DeleteFile(Self.FParentDir.ChildFileList[I].FilePath) then
            begin
              Self.FParentDir.ChildFileList.Remove(TObject(Self.lbFileList.Properties.Items[I].Data),True);
              Self.lbFileList.Properties.Items.Delete(I,True);
            end
            else
            begin
//              MessageDlg('删除文件'+Self.FParentDir.ChildFileList[I].FileName+'失败!',TMsgDlgType.mtError,[TMsgDlgBtn.mbOK],0);
            end;
          end;
        end;
      end;

    finally
      Self.lbFileList.EndUpdate;
      Self.lbFileList.Properties.Items.EndUpdate;
    end;
//  end;

end;

procedure TFrameFileManage.btnBatchProcessClick(Sender: TObject);
var
  I:Integer;
begin
  FIsBatchProcessing:=True;

  Self.btnMultiSelected.Properties.IsPushed:=False;

  //初始选中状态
  for I := 0 to Self.FParentDir.ChildFileList.Count-1 do
  begin
    Self.FParentDir.ChildFileList[I].OperationRec.Selected:=False;
  end;

  Self.lbFileList.Properties.IsAutoSelected:=False;

  //批量处理
  Self.imgBatchToolBar.Visible:=True;
  Self.btnBatchProcess.Visible:=False;
  Self.btnComplete.Visible:=True;
  Self.btnNewFolder.Visible:=True;
  Self.btnFTPServer.Visible:=False;
  Self.btnReturn.Visible:=False;
  btnMultiSelected.Visible:=True;

end;

end.
