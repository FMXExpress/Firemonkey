//文件管理
unit uFileManage;

interface

uses
  Classes,
  SysUtils,
  uBaseList,
  IOUtils,
  Types,
  IniFiles,
  uFileCommon;

const
  Const_DirName_FTPRootDir='FTPRootDir';

const
  Const_FileName_Config='Config.ini';


type
  TFileType=(ftFile,  //文件
              ftDir); //目录

  TFileState=(fsNone, //无
              fsArrow,//有子内容
              fsNew   //新文件
              );
  TFileExtType=(fetOther,
                fetWord,
                fetExcel,
                fetPoint,
                fetPDF,
                fetText,
                fetVideo,
                fetCompress,
                fetAudio,
                fetPicture
                );

  TFileOperationRec=record
    Selected:Boolean;
//    Position:Integer;
  end;



  TFileList=class;
  TFileItem=class(TBinaryObject)
  private
    FFileName: String;
//    FParent:TFileItem;
    FFileType:TFileType;
    FFileState:TFileState;
    FSize: Int64;
    FFileDate: TDateTime;
    FFilePath:String;
    FFileExtType: TFileExtType;
    FChildFileList:TFileList;

  public
    constructor Create;
    destructor Destroy;override;
  public
    OperationRec:TFileOperationRec;

    function IsSame(CompareFileItem:TFileItem):Boolean;

    //本地路径
    property FilePath:String read FFilePath write FFilePath;
    //文件名
    property FileName:String read FFileName write FFileName;
    //文件类型
    property FileType:TFileType read FFileType write FFileType;
    //文件状态
    property FileState:TFileState read FFileState write FFileState;
    //文件大小
    property Size:Int64 read FSize write FSize;
    //文件日期
    property FileDate:TDateTime read FFileDate write FFileDate;
    //文件后缀类型
    property FileExtType:TFileExtType read FFileExtType write FFileExtType;
    //
    property ChildFileList:TFileList read FChildFileList write FChildFileList;
  end;





  TFileList=class(TBinaryObjectList)
  private
    FParent: TFileItem;
    function GetItem(Index: Integer): TFileItem;
    procedure SetItem(Index: Integer; const Value: TFileItem);
  protected
    function CreateBinaryObject:TInterfacedPersistent;override;
    function GetBinaryObjectClassName:String;override;
  public
    function HasSameFile(CompareFileItem:TFileItem):Boolean;
    property Parent:TFileItem read FParent write FParent;
    property Items[Index:Integer]:TFileItem read GetItem write SetItem;default;
  end;




//当前目录下的文件列表
function LoadChildFileList(ParentFileItem:TFileItem):Boolean;


var
  FTPRootDir:String;
  GlobalRootDir:TFileItem;
  TestFileStringList:TStringList;

implementation




function LoadChildFileList(ParentFileItem:TFileItem): Boolean;
var
  I : Integer;
  FileItem:TFileItem;
  AFileExt:String;
  AAllFiles:TStringDynArray;
  AAllDirs:TStringDynArray;
begin
  Result:=False;

  ParentFileItem.ChildFileList.Clear(True);

  AAllDirs:=TDirectory.GetDirectories(ParentFileItem.FFilePath);
  for I := 0 to Length(AAllDirs)-1 do
  begin

    FileItem:=TFileItem(ParentFileItem.ChildFileList.Add);

    FileItem.FFileName:=ExtractFileName(AAllDirs[I]);
    FileItem.FFileType:=ftDir;
    FileItem.FFilePath:=ParentFileItem.FFilePath+FileItem.FFileName+PathDelim;
    FileItem.FFileDate:=TDirectory.GetCreationTime(AAllDirs[I]);

  end;



  AAllFiles:=TDirectory.GetFiles(ParentFileItem.FFilePath);
  for I := 0 to Length(AAllFiles)-1 do
  begin

        FileItem:=TFileItem(ParentFileItem.ChildFileList.Add);

        FileItem.FFileName:=ExtractFileName(AAllFiles[I]);
        FileItem.FFileDate:=TFile.GetCreationTime(AAllFiles[I]);

        FileItem.FSize:=GetSizeOfFile(AAllFiles[I]);
        FileItem.FFileType:=ftFile;
        FileItem.FFilePath:=ParentFileItem.FFilePath+FileItem.FFileName;




        AFileExt:=ExtractFileExt(FileItem.FFileName);


        FileItem.FFileExtType:=fetOther;

        if SameText(AFileExt,'.doc')
            or SameText(AFileExt,'.docx') then
        begin
          FileItem.FFileExtType:=fetWord;
        end
        else if SameText(AFileExt,'.xls')
              or SameText(AFileExt,'.xlsx') then
        begin
          FileItem.FFileExtType:=fetExcel;
        end
        else if SameText(AFileExt,'.ppt')
              or SameText(AFileExt,'.pptx') then
        begin
          FileItem.FFileExtType:=fetPoint;
        end
        else if SameText(AFileExt,'.pdf') then
        begin
          FileItem.FFileExtType:=fetPDF;
        end
        else if SameText(AFileExt,'.txt')
              or SameText(AFileExt,'.ini') then
        begin
          FileItem.FFileExtType:=fetText;
        end
        else if SameText(AFileExt,'.mov')
              or SameText(AFileExt,'.avi')
              or SameText(AFileExt,'.wmv')
              or SameText(AFileExt,'.rmvb')
              or SameText(AFileExt,'.mp4') then
        begin
          FileItem.FFileExtType:=fetVideo;
        end
        else if SameText(AFileExt,'.zip')
            or SameText(AFileExt,'.7z')
            or SameText(AFileExt,'.rar') then
        begin
          FileItem.FFileExtType:=fetCompress;
        end
        else if SameText(AFileExt,'.mp3')
            or SameText(AFileExt,'.wma')
            or SameText(AFileExt,'.wav') then
        begin
          FileItem.FFileExtType:=fetAudio;
        end
        else if SameText(AFileExt,'.ico')
            or SameText(AFileExt,'.bmp')
            or SameText(AFileExt,'.dib')
            or SameText(AFileExt,'.jpg')
            or SameText(AFileExt,'.gif')
            or SameText(AFileExt,'.png')
            or SameText(AFileExt,'.jpeg') then
        begin
          FileItem.FFileExtType:=fetPicture;
        end
        ;

  end;

  Result:=True;
end;



function GetFTPRootDir:String;
begin
  Result:=GetApplicationPath+Const_DirName_FTPRootDir;//+PathDelim;
  if Not DirectoryExists(Result) then
  begin
    ForceDirectories(Result);
  end;
end;
{ TFileList }

function TFileList.CreateBinaryObject: TInterfacedPersistent;
begin
  Result:=TFileItem.Create;
end;

function TFileList.GetBinaryObjectClassName: String;
begin
  Result:='FileItem';
end;

function TFileList.GetItem(Index: Integer): TFileItem;
begin
  Result:=TFileItem(Inherited Items[Index]);
end;

function TFileList.HasSameFile(CompareFileItem: TFileItem): Boolean;
var
  I: Integer;
begin
  Result:=False;
  for I := 0 to Self.Count-1 do
  begin
    if Items[I].IsSame(CompareFileItem) then
    begin
      Result:=True;
      Break;
    end;
  end;
end;

procedure TFileList.SetItem(Index: Integer; const Value: TFileItem);
begin
  Inherited Items[Index]:=Value;
end;




{ TFileItem }

constructor TFileItem.Create;
begin
  FChildFileList:=TFileList.Create;
  FChildFileList.FParent:=Self;
end;

destructor TFileItem.Destroy;
begin
  FChildFileList.Clear(True);
  FreeAndNil(FChildFileList);
  inherited;
end;


function TFileItem.IsSame(CompareFileItem: TFileItem): Boolean;
begin
  Result:=False;
  case CompareFileItem.FFileType of
    ftDir:
    begin
      Result:=(CompareFileItem.FFileName=Self.FFileName);
    end;
    ftFile:
    begin
      Result:=(CompareFileItem.FFileName=Self.FFileName)
                and (CompareFileItem.FFileDate=Self.FFileDate)
                and (CompareFileItem.FSize=Self.FSize);
    end;
  end;
end;

initialization
  FTPRootDir:=GetFTPRootDir;

  GlobalRootDir:=TFileItem.Create;
  GlobalRootDir.FFileName:='所有文件';
  GlobalRootDir.FFilePath:=GetFTPRootDir+PathDelim;
  GlobalRootDir.FFileType:=ftDir;

  //自动创建测试文件
  if Not FileExists(GetFTPRootDir+PathDelim+'TestFile.txt') then
  begin
    CreateDir(GetFTPRootDir+PathDelim+'TestDir');

    TestFileStringList:=TStringList.Create;
    TestFileStringList.Add('adfasdf');
    TestFileStringList.SaveToFile(GetFTPRootDir+PathDelim+'TestFile.txt');
    TestFileStringList.SaveToFile(GetFTPRootDir+PathDelim+'TestFile.avi');
    TestFileStringList.SaveToFile(GetFTPRootDir+PathDelim+'TestFile.mp3');
    TestFileStringList.SaveToFile(GetFTPRootDir+PathDelim+'TestFile.ini');
    TestFileStringList.SaveToFile(GetFTPRootDir+PathDelim+'TestFile.rar');
    TestFileStringList.SaveToFile(GetFTPRootDir+PathDelim+'TestFile.pdf');
    TestFileStringList.SaveToFile(GetFTPRootDir+PathDelim+'TestFile.ppt');
    TestFileStringList.SaveToFile(GetFTPRootDir+PathDelim+'TestFile.xls');
    TestFileStringList.SaveToFile(GetFTPRootDir+PathDelim+'TestFile.doc');

    TestFileStringList.Free;
  end;

finalization
  GlobalRootDir.Free;

end.
