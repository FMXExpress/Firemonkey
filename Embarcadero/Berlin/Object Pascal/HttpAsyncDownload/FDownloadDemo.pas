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
unit FDownloadDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent, FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  System.ImageList, FMX.ImgList;

type
  TFormDownloadDemo = class(TForm)
    PanelTop: TPanel;
    PanelCenter: TPanel;
    LabelFile: TLabel;
    EditFileName: TEdit;
    BStartDownload: TButton;
    Memo1: TMemo;
    ImageList1: TImageList;
    LabelURL: TLabel;
    EditURL: TEdit;
    LabelGlobalSpeed: TLabel;
    ProgressBarDownload: TProgressBar;
    BStopDownload: TButton;
    procedure BStartDownloadClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure BStartComponentClick(Sender: TObject);
    procedure ReceiveDataEvent(const Sender: TObject; AContentLength: Int64; AReadCount: Int64; var Abort: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FClient: THTTPClient;
    FGlobalStart: Cardinal;
    FAsyncResponse: IHTTPResponse;
    FDownloadStream: TStream;

    procedure DoEndDownload(const AsyncResult: IAsyncResult);
  public
    { Public declarations }
    procedure SampleDownload;
  end;

var
  FormDownloadDemo: TFormDownloadDemo;

implementation

{$R *.fmx}

uses
  System.IOUtils;


procedure TFormDownloadDemo.BStartDownloadClick(Sender: TObject);
begin
  BStartDownload.Enabled := False;
  SampleDownload;
end;

procedure TFormDownloadDemo.ButtonCancelClick(Sender: TObject);
begin
  (Sender as TButton).Enabled := False;
end;

procedure TFormDownloadDemo.BStartComponentClick(Sender: TObject);
begin
  BStartDownload.Enabled := False;
  SampleDownload;
end;

procedure TFormDownloadDemo.DoEndDownload(const AsyncResult: IAsyncResult);
begin
  try
    FAsyncResponse := THTTPClient.EndAsyncHTTP(AsyncResult);
    TThread.Synchronize(nil,
      procedure
      begin
        Memo1.Lines.Add('Download Finished!');
        Memo1.Lines.Add(Format('Status: %d - %s', [FAsyncResponse.StatusCode, FAsyncResponse.StatusText]));
      end);
  finally
    FDownloadStream.Free;
    FDownloadStream := nil;
    BStopDownload.Enabled := False;
    BStartDownload.Enabled := True;
  end;
end;

procedure TFormDownloadDemo.FormCreate(Sender: TObject);
begin
  FClient := THTTPClient.Create;
  FClient.OnReceiveData := ReceiveDataEvent;
end;

procedure TFormDownloadDemo.FormDestroy(Sender: TObject);
begin
  FAsyncResponse := nil;
  FDownloadStream.Free;
  FClient.Free;
end;

procedure TFormDownloadDemo.ReceiveDataEvent(const Sender: TObject; AContentLength, AReadCount: Int64;
  var Abort: Boolean);
var
  LTime: Cardinal;
  LSpeed: Integer;
  LCancel: Boolean;
begin
  LCancel := Abort;
  LTime := TThread.GetTickCount - FGlobalStart;
  LSpeed := (AReadCount * 1000) div LTime;
  TThread.Queue(nil,
    procedure
    begin
      LCancel := not BStopDownload.Enabled;
      ProgressBarDownload.Value := AReadCount;
      LabelGlobalSpeed.Text := Format('Global speed: %d KB/s', [LSpeed div 1024]);
    end);
  Abort := LCancel;
end;

procedure TFormDownloadDemo.SampleDownload;
var
  URL: string;
  LResponse: IHTTPResponse;
  LFileName: string;
  LSize: Int64;
begin
  LFileName := TPath.Combine(TPath.GetDocumentsPath, EditFileName.Text);
  try
    FAsyncResponse := nil;
    URL := EditURL.Text;

    LResponse := FClient.Head(URL);
    LSize := LResponse.ContentLength;
    Memo1.Lines.Add(Format('Head response: %d - %s', [LResponse.StatusCode, LResponse.StatusText]));
    LResponse := nil;

    ProgressBarDownload.Max := LSize;
    ProgressBarDownload.Min := 0;
    ProgressBarDownload.Value := 0;
    LabelGlobalSpeed.Text := 'Global speed: 0 KB/s';

    Memo1.Lines.Add(Format('Downloading: "%s" (%d Bytes) into "%s"' , [EditFileName.Text, LSize, LFileName]));

    // Create the file that is going to be dowloaded
    FDownloadStream := TFileStream.Create(LFileName, fmCreate);
    FDownloadStream.Position := 0;

    FGlobalStart := TThread.GetTickCount;

    // Start the download process
    FAsyncResponse := FClient.BeginGet(DoEndDownload, URL, FDownloadStream);

  finally
    BStopDownload.Enabled := FAsyncResponse <> nil;
    BStartDownload.Enabled := FAsyncResponse = nil;
    FAsyncREsponse := nil;
  end;
end;

end.
