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
unit DownloadServiceUnit;

interface

uses
  System.Android.Service, System.Classes, System.Generics.Collections, System.Notification, System.SyncObjs,
  Androidapi.JNI.GraphicsContentViewText,
  uDownloadThread;

type
  TDownloadServiceDM = class(TAndroidService)
  private
    FDownloadThreads: TList<TDownloadThread>;
    FThreads: TObjectList<TThread>;

    procedure ProcessDownload(const Intent: JIntent; StartId: Integer; const AEvent: TEvent);
    procedure EndDownload(const Sender: TObject; AStatus: Integer);
  published
    NotificationCenter1: TNotificationCenter;

    function AndroidServiceStartCommand(const Sender: TObject; const Intent: JIntent; Flags, StartId: Integer): Integer;
    procedure AndroidServiceCreate(Sender: TObject);
    procedure AndroidServiceDestroy(Sender: TObject);
  end;

var
  DownloadServiceDM: TDownloadServiceDM;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

uses
  System.IOUtils, System.Net.HttpClient, System.SysUtils,
  Androidapi.JNI.App,
  IntentServiceUnit;

procedure TDownloadServiceDM.ProcessDownload(const Intent: JIntent; StartId: Integer; const AEvent: TEvent);
var
  LTIntentService: TIntentServiceHelper;
  LData: TArray<string>;
  URL: string;
  LFileName: string;
  LClient: THTTPClient;
  LResponse: IHTTPResponse;
  LSart: Int64;
  StFile: TFileStream;
  LDownloadThread: TDownloadThread;
  LCanResume: Boolean;
begin
  LTIntentService := TIntentServiceHelper.Create(Intent);

  LData := LTIntentService.Data.Split([Char('|')]);
  if Length(LData) = 2 then
  begin
    URL := LData[0];
    LFileName := LData[1];

    LClient := THTTPClient.Create;
    try
      LResponse := LClient.Head(URL);

      LFileName := TPath.Combine(TPath.GetSharedDownloadsPath, LFileName);

      LCanResume := LClient.CheckDownloadResume(URL);
      LSart := 0;

      if LCanResume and FileExists(LFileName) then
      begin
        StFile := TFileStream.Create(LFileName, fmOpenRead);
        try
          LSart := StFile.Size;
        finally
          StFile.Free;
        end;
      end
      else
      begin
        StFile := TFileStream.Create(LFileName, fmCreate);
        StFile.Free;
      end;

      LDownloadThread := TDownloadThread.Create(URL, LFileName, StartId, LSart, -1);
      LDownloadThread.Event := AEvent;
      LDownloadThread.OnEndDownload := EndDownload;
      FDownloadThreads.Add(LDownloadThread);
      LDownloadThread.Start;
    finally
      LClient.Free;
    end;
  end;
end;

procedure TDownloadServiceDM.AndroidServiceCreate(Sender: TObject);
begin
  if JavaService <> nil then
  begin
    FDownloadThreads := TList<TDownloadThread>.Create;
    FThreads := TObjectList<TThread>.Create;
  end;
end;

procedure TDownloadServiceDM.AndroidServiceDestroy(Sender: TObject);
begin
  FDownloadThreads.Free;
  FThreads.Free;
end;

function TDownloadServiceDM.AndroidServiceStartCommand(const Sender: TObject; const Intent: JIntent; Flags,
  StartId: Integer): Integer;
var
  LThread: TThread;
begin
  LThread := TThread.CreateAnonymousThread(
    procedure
    var
      LEvent: TEvent;
    begin
      LEvent := TEvent.Create;
      ProcessDownload(Intent, StartId, LEvent);
      LEvent.WaitFor(INFINITE);
      JavaService.stopSelf(StartId);
    end);
  LThread.FreeOnTerminate := False;
  FThreads.Add(LThread);
  LThread.Start;

  Result := TJService.JavaClass.START_REDELIVER_INTENT;
end;

procedure TDownloadServiceDM.EndDownload(const Sender: TObject; AStatus: Integer);
var
  MyNotification: TNotification;
  StartId: Integer;
begin
  MyNotification := NotificationCenter1.CreateNotification;
  try
    MyNotification.Name := 'DownloadFinished';
    MyNotification.Title := 'Download Finished';
    MyNotification.AlertBody := TDownloadThread(Sender).FileName;
    NotificationCenter1.PresentNotification(MyNotification);
  finally
    MyNotification.Free;
  end;
  StartId := TDownloadThread(Sender).ThreadNo;
  FDownloadThreads.Remove(TDownloadThread(Sender));
//  JavaService.stopSelf(StartId);
end;

end.
