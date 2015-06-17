unit uDownloadThread;

interface

uses
  System.Classes;

type
  TDownloadThreadDataEvent = procedure(const Sender: TObject; ThreadNo, ASpeed: Integer; AContentLength: Int64; AReadCount: Int64; var Abort: Boolean; const VisualObject: TObject) of object;
  TDownloadThread = class(TThread)
  private
    FOnThreadData: TDownloadThreadDataEvent;
    FVisualObject: TObject;
    FCanResume: Boolean;
    FShouldResume: Boolean;
    FLastDownloaded: Int64;
    FPauseDownload: Boolean;
    procedure Download(AStartPoint, AEndPoint: Int64);
  protected
    FURL, FFileName: string;
    FStartPoint, FEndPoint: Int64;
    FThreadNo: Integer;
    FTimeStart: Cardinal;

    procedure ReceiveDataEvent(const Sender: TObject; AContentLength: Int64; AReadCount: Int64; var Abort: Boolean);
  public
    constructor Create(const URL, FileName: string; ThreadNo: Integer; StartPoint, EndPoint: Int64);
    destructor Destroy; override;
    procedure Execute; override;
    procedure ResumeDownload;
    procedure PauseDownload;

    property VisualObject: TObject read FVisualObject write FVisualObject;
    property CanResume: Boolean read FCanResume write FCanResume;
    property IsPaused: Boolean read FPauseDownload;
    property OnThreadData: TDownloadThreadDataEvent write FOnThreadData;
  end;


implementation

uses
  System.Net.URLClient, System.Net.HttpClient, System.SysUtils;

{ TDownloadThread }

constructor TDownloadThread.Create(const URL, FileName: string; ThreadNo: Integer; StartPoint, EndPoint: Int64);
begin
  inherited Create(True);
  FURL := URL;
  FFileName := FileName;
  FThreadNo := ThreadNo;
  FStartPoint := StartPoint;
  FEndPoint := EndPoint;
  FLastDownloaded := 0;
  FShouldResume := False;
  FPauseDownload := False;
end;

destructor TDownloadThread.Destroy;
begin
  inherited;
end;

procedure TDownloadThread.Execute;
begin
  Download(FStartPoint, FEndPoint);
  while not terminated and FCanResume do
  begin
    if FShouldResume then
    begin
      FShouldResume := False;
      FPauseDownload := False;
      Download(FLastDownloaded, FEndPoint);
    end;
  end;
end;

procedure TDownloadThread.PauseDownload;
begin
  FPauseDownload := True;
end;

procedure TDownloadThread.Download(AStartPoint, AEndPoint: Int64);
var
  LResponse: IHTTPResponse;
  LStream: TFileStream;
  LHttpClient: THTTPClient;
begin
  inherited;
  LHttpClient := THTTPClient.Create;
  try
    LHttpClient.OnReceiveData := ReceiveDataEvent;
    LStream := TFileStream.Create(FFileName, fmOpenWrite or fmShareDenyNone);
    try
      FTimeStart := GetTickCount;
      if FEndPoint = 0 then
        LResponse := LHttpClient.Get(FURL, LStream)
      else
      begin
        LStream.Seek(AStartPoint, TSeekOrigin.soBeginning);
        LResponse := LHttpClient.GetRange(FURL, AStartPoint, AEndPoint, LStream);
      end;
    finally
      LStream.Free;
    end;
  finally
    LHttpClient.Free;
  end;
end;

procedure TDownloadThread.ReceiveDataEvent(const Sender: TObject; AContentLength, AReadCount: Int64;
  var Abort: Boolean);
var
  LTime: Cardinal;
  LSpeed: Integer;
begin
  if Terminated or FPauseDownload then
  begin
    ABort := true;
    FLastDownloaded := FLastDownloaded + AReadCount;
  end
  else if Assigned(FOnThreadData) then
  begin
    LTime := GetTickCount - FTimeStart;
    if AReadCount = 0 then
      LSpeed := 0
    else
      LSpeed := (AReadCount * 1000) div LTime;

    FOnThreadData(Sender, FThreadNo, LSpeed, FLastDownloaded + AContentLength, FLastDownloaded + AReadCount, Abort, FVisualObject);
  end;
end;


procedure TDownloadThread.ResumeDownload;
begin
  FShouldResume := True;
end;

end.
