unit uDirectoryPublic;


interface

uses
  SysUtils,
  uFuncCommon,
  uFileCommon;

//应用数据根目录
function GetAppDataDir_Root(WidthPathDelim:Boolean=True):String;
function GetAppDataDir_NewsImage(WidthPathDelim:Boolean=True):String;

//function GetResponseTempDir:String;
procedure CreateFileDir(AFileName:String);

implementation

procedure CreateFileDir(AFileName:String);
var
  FileDir:String;
begin
  FileDir:=ExtractFilePath(AFileName);
  if Not DirectoryExists(FileDir) then
  begin
    SysUtils.ForceDirectories(FileDir);
  end;
end;
//function GetResponseTempDir:String;
//begin
//  Result:=GetApplicationPath+'Response'+PathDelim;
//  if Not DirectoryExists(Result) then
//  begin
//    SysUtils.ForceDirectories(Result);
//  end;
//end;

function GetAppDataDir_Root(WidthPathDelim:Boolean=True):String;
begin
  Result:=GetApplicationPath+'AppData';
  if Not DirectoryExists(Result) then
  begin
    SysUtils.ForceDirectories(Result);
  end;
  if WidthPathDelim then
  begin
    Result:=Result+PathDelim;
  end;
end;

function GetAppDataDir_User(User:String;WidthPathDelim:Boolean=True):String;
begin
  Result:=GetApplicationPath+'Users'+PathDelim+User;
  if Not DirectoryExists(Result) then
  begin
    SysUtils.ForceDirectories(Result);
  end;
  if WidthPathDelim then
  begin
    Result:=Result+PathDelim;
  end;
end;

function GetAppDataDir_NewsImage(WidthPathDelim:Boolean=True):String;
begin
  Result:=GetAppDataDir_Root+'NewsImage';//+PathDelim;
  if Not DirectoryExists(Result) then
  begin
    SysUtils.ForceDirectories(Result);
  end;
  if WidthPathDelim then
  begin
    Result:=Result+PathDelim;
  end;
end;



end.
