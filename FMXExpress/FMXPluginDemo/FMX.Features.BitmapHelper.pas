unit FMX.Features.BitmapHelper;

interface

uses
  System.Classes, FMX.Graphics, Soap.EncdDecd, System.IOUtils;

type
  TBitmapHelper = class helper for TBitmap
  private
    procedure RefreshBitMap(Sender : TObject);
    procedure SetBmp(Value: TBitmap);
    function GetBmp : TBitmap;
    procedure LoadFromBase64(const Value: string);

    function SaveToBase64: string;
  public
    property BmpTest: TBitmap read GetBmp write SetBmp;
    property Base64 : string read SaveToBase64 write LoadFromBase64;
    procedure LoadFromUrl(AUrl: string);

    procedure LoadThumbnailFromUrl(AUrl: string; const AFitWidth, AFitHeight: Integer);
    function SaveToFilef(fName : string) : boolean;
    function LoadFromResource(bmpName : string) : Boolean;
  end;

implementation

uses
  System.SysUtils, System.Types, IdHttp, IdTCPClient;
{------------------------------------------------------------------------------}
function TBitmapHelper.GetBmp: TBitmap;
begin
    Result := self;
end;
{------------------------------------------------------------------------------}
procedure TBitmapHelper.LoadFromBase64(const Value: string);
var
    stream : TBytesStream;
    bytes : TBytes;
    temp : string;
begin
    if Value.IsEmpty then
        exit;
    bytes :=  DecodeBase64(Value);
    stream := TBytesStream.Create(bytes);
    try
        LoadFromStream(stream);
    except
        on e : Exception do
            temp := e.Message;
    end;
    FreeAndNil(stream);
end;
{------------------------------------------------------------------------------}
function TBitmapHelper.LoadFromResource(bmpName: string): Boolean;
var
    RStream: TResourceStream;
begin
    Result := false;
    if bmpName.IsEmpty then
        exit;
     RStream := TResourceStream.Create(HInstance, bmpName, RT_RCDATA);
     try
        if RStream.Size < 1000000 then
            self.LoadFromStream(RStream);
        Result := true;
     finally
        RStream.Free;
     end;
end;

{------------------------------------------------------------------------------}
function TBitmapHelper.SaveToBase64: string;
var
    stream : TMemoryStream;
    encodeString : string;
begin
    if IsEmpty then
        Exit(string.Empty);
    stream := TMemoryStream.Create;
    try
        SaveToStream(stream);
        encodeString := EncodeBase64(stream.Memory, stream.Size);
        Result := encodeString;
    finally
        FreeAndNil(stream);
    end;
end;
{------------------------------------------------------------------------------}
function TBitmapHelper.SaveToFilef(fName: string): boolean;
var
    dirName : string;
    fileName : string;
begin
    Result := false;
    if fName.IsEmpty then
        exit;
    dirName := TPath.GetDirectoryName(fName);
    fileName := TPath.GetFileName(fName);
    if ForceDirectories(dirName) then
    begin
        SaveToFile(fName);
        Result := True;
    end;
end;
{------------------------------------------------------------------------------}
procedure TBitmapHelper.LoadFromUrl(AUrl: string);
var
  thread : TThread;
begin
 thread :=  TThread.CreateAnonymousThread(
    procedure
    var
      Http: TIdHttp;
      Result : TMemoryStream;
    begin
      Result := TMemoryStream.Create;
      Http := TIdHttp.Create(nil);
      try
        try
        Http.Get(AUrl, Result);
        TThread.Synchronize (TThread.CurrentThread,
            procedure ()
            var
                tempBitMap : TBitmap;
            begin
                tempBitMap := TBitmap.Create;
                tempBitMap.LoadFromStream(Result);
                if not tempBitMap.IsEmpty then
                begin
                    self.Assign(tempBitMap);
                end;
            end);
        except
          Result.Free;
        end;
      finally
        Http.Free;
      end;
    end
  );
  thread.FreeOnTerminate := True;
  thread.start;
end;
{------------------------------------------------------------------------------}
procedure TBitmapHelper.LoadThumbnailFromUrl(AUrl: string; const AFitWidth,
  AFitHeight: Integer);
var
  Bitmap: TBitmap;
  scale: Single;
begin
  LoadFromUrl(AUrl);
  Bitmap := CreateThumbnail(AFitWidth, AFitHeight);
  try
    Assign(Bitmap);
  finally
    Bitmap.Free;
  end;
end;
{------------------------------------------------------------------------------}
procedure TBitmapHelper.RefreshBitMap(Sender: TObject);
var
    tempBmp : TBitmap;
begin
    tempBmp := TBitmap(Sender);
    if Assigned(tempBmp) then
        self.Assign(tempBmp);
end;
{------------------------------------------------------------------------------}
procedure TBitmapHelper.SetBmp(Value: TBitmap);
begin
    self.Assign(Value);
    Value.OnChange := RefreshBitMap;
end;
{------------------------------------------------------------------------------}
end.
