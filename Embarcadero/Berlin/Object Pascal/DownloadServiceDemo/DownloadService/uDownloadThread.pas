//---------------------------------------------------------------------------
// Copyright (c) 2016 Embarcadero Technologies, Inc. All rights reserved.  
//
// This software is the copyrighted property of Embarcadero Technologies, Inc. 
// ("Embarcadero") and its licensors. You may only use this software if you 
// are an authorized licensee of Delphi, C++Builder or RAD Studio 
// (the "Embarcadero Products").  This software is subject to Embarcadero's 
// standard software license and support agreement that accompanied your 
// purchase of the Embarcadero Products and is considered a Redistributable, 
// as such term is defined thereunder. Your use of this software constitutes 
// your acknowledgement of your agreement to the foregoing software license 
// and support agreement. 
//---------------------------------------------------------------------------
unit uDownloadThread;

interface

uses
  System.Classes, System.SyncObjs;

type
  TDownloadThreadDataEvent = procedure(const Sender: TObject; ThreadNo, ASpeed: Integer; AContentLength: Int64; AReadCount: Int64; var Abort: Boolean; const VisualObject: TObject) of object;
  TDownloadThreadEndDownloadEvent = procedure(const Sender: TObject; AStatus: Integer) of object;
  TDownloadThread = class(TThread)
  private
    FOnThreadData: TDownloadThreadDataEvent;
    FOnEndDownload: TDownloadThreadEndDownloadEvent;
    FVisualObject: TObject;
    FCanResume: Boolean;
    FShouldResume: Boolean;
    FLastDownloaded: Int64;
    FPauseDownload: Boolean;
    FEvent: TEvent;
    procedure Download(AStartPoint, AEndPoint: Int64);
  protected
    FURL, FFileName: string;
    FStartPoint, FEndPoint: Int64;
    FThreadNo: Integer;
    FTimeStart: Cardinal;

    procedure ReceiveDataEvent(const Sender: TObject; AContentLength: Int64; AReadCount: Int64; var Abort: Boolean);
  public
    constructor Create(const URL, FileName: string; AThreadNo: Integer; StartPoint, EndPoint: Int64);
    destructor Destroy; override;
    procedure Execute; override;
    procedure ResumeDownload;
    procedure PauseDownload;

    property VisualObject: TObject read FVisualObject write FVisualObject;
    property CanResume: Boolean read FCanResume write FCanResume;
    property IsPaused: Boolean read FPauseDownload;
    property OnThreadData: TDownloadThreadDataEvent write FOnThreadData;
    property OnEndDownload: TDownloadThreadEndDownloadEvent write FOnEndDownload;
    property URL: string read FURL;
    property FileName: string read FFileName;
    property ThreadNo: Integer read FThreadNo;
    property Event: TEvent read FEvent write FEvent;
  end;


implementation

uses
  System.Net.URLClient, System.Net.HttpClient, System.SysUtils;

{ TDownloadThread }

constructor TDownloadThread.Create(const URL, FileName: string; AThreadNo: Integer; StartPoint, EndPoint: Int64);
begin
  inherited Create(True);
  FURL := URL;
  FFileName := FileName;
  FThreadNo := AThreadNo;
  FStartPoint := StartPoint;
  FEndPoint := EndPoint;
  FLastDownloaded := StartPoint;
  FShouldResume := False;
  FPauseDownload := False;
end;

destructor TDownloadThread.Destroy;
begin
  inherited;
  FEvent.SetEvent;
end;

procedure TDownloadThread.Execute;
begin
  Download(FStartPoint, FEndPoint);
  while not terminated and FCanResume do
  begin
    Sleep(1);
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
    if Assigned(FOnEndDownload) then
    begin
      FOnEndDownload(Self, LResponse.StatusCode);
      Terminate;
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
