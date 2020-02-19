//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit FDownloadDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent, FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  System.ImageList, FMX.ImgList;

type
  TDownloadThreadDataEvent = procedure(const Sender: TObject; ThreadNo, ASpeed: Integer; AContentLength: Int64; AReadCount: Int64; var Abort: Boolean) of object;
  TDownloadThread = class(TThread)
  private
    FOnThreadData: TDownloadThreadDataEvent;

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

    property OnThreadData: TDownloadThreadDataEvent write FOnThreadData;
  end;

  TFormDownloadDemo = class(TForm)
    PanelTop: TPanel;
    PanelCenter: TPanel;
    LabelFile: TLabel;
    EditFileName: TEdit;
    BStartDownload: TButton;
    Memo1: TMemo;
    ProgressBarPart1: TProgressBar;
    Label1: TLabel;
    Button1: TButton;
    ImageList1: TImageList;
    LabelURL: TLabel;
    EditURL: TEdit;
    LabelGlobalSpeed: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Button4: TButton;
    Label4: TLabel;
    ProgressBarPart4: TProgressBar;
    Panel3: TPanel;
    Button3: TButton;
    Label3: TLabel;
    ProgressBarPart2: TProgressBar;
    Panel4: TPanel;
    Button2: TButton;
    Label2: TLabel;
    ProgressBarPart3: TProgressBar;
    Panel5: TPanel;
    procedure BStartDownloadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    ButtonCancelArray: array [0..3] of TButton;
    ProgressBarArray: array [0..3] of TProgressBar;
    LabelProgressArray: array [0..3] of TLabel;
    [volatile] FAllowFormClose: Boolean;
    [volatile] FClosingForm: Boolean;
    procedure ReceiveThreadDataEvent(const Sender: TObject; ThreadNo, ASpeed: Integer; AContentLength: Int64; AReadCount: Int64; var Abort: Boolean);
  public
  const
    NumThreads: Integer = 4;
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
  (Sender as TButton).Enabled := False;
  FAllowFormClose := False;
  TThread.CreateAnonymousThread(procedure
  begin
    SampleDownload;
  end).Start;
end;

procedure TFormDownloadDemo.ButtonCancelClick(Sender: TObject);
begin
  (Sender as TButton).Enabled := False;
end;

procedure TFormDownloadDemo.FormClose(Sender: TObject; var Action: TCloseAction);
var
  I: Integer;
begin
	Memo1.Lines.Add('Cancelling the download process may take some time.');
	Application.ProcessMessages;

  FClosingForm := True;

  for I := Low(ButtonCancelArray) to High(ButtonCancelArray) do
    ButtonCancelArray[I].Enabled := False;

  while not FAllowFormClose do
  begin
    Application.ProcessMessages;
    Sleep(1);
  end;
end;

procedure TFormDownloadDemo.FormCreate(Sender: TObject);
begin
  FAllowFormClose := True;
  ButtonCancelArray[0] := Button1;
  ButtonCancelArray[1] := Button2;
  ButtonCancelArray[2] := Button3;
  ButtonCancelArray[3] := Button4;
  ProgressBarArray[0] := ProgressBarPart1;
  ProgressBarArray[1] := ProgressBarPart2;
  ProgressBarArray[2] := ProgressBarPart3;
  ProgressBarArray[3] := ProgressBarPart4;
  LabelProgressArray[0] := Label1;
  LabelProgressArray[1] := Label2;
  LabelProgressArray[2] := Label3;
  LabelProgressArray[3] := Label4;
end;

procedure TFormDownloadDemo.ReceiveThreadDataEvent(const Sender: TObject; ThreadNo: Integer; ASpeed: Integer; AContentLength, AReadCount: Int64;
  var Abort: Boolean);
var
  LCad: string;
  LCancel: Boolean;
  LSpeed: Integer;
begin
  LCancel := Abort or FClosingForm;
  if not LCancel then
    TThread.Synchronize(nil,
      procedure
      begin
        LCancel := not ButtonCancelArray[ThreadNo].Enabled;

        ProgressBarArray[ThreadNo].Value := AReadCount;
        LabelProgressArray[ThreadNo].Text := Format('%d KB/s', [ASpeed div 1024]);
      end);
  Abort := LCancel;
end;


procedure TFormDownloadDemo.SampleDownload;
var
  LClient: THTTPClient;
  URL: string;
  LResponse: IHTTPResponse;
  StFile: TFileStream;
  LFileName: string;
  LStart, LEnd, LSize, LFragSize: Int64;
  I: Integer;
  LDownloadThreads: array of TDownloadThread;
  LFinished: Boolean;
  LStartTime, LEndTime: Cardinal;
begin
  LClient := THTTPClient.Create;
  LFileName := TPath.Combine(TPath.GetDocumentsPath, EditFileName.Text);

  TThread.Synchronize(nil, procedure
  begin
    Memo1.Lines.Add('File location = ' + LFileName);
    Memo1.Lines.Add('Downloading ' + URL + ' ...');
    Application.ProcessMessages;
  end);
  try
    URL := EditURL.Text;
    if LClient.CheckDownloadResume(URL) then
    begin
      LResponse := LClient.Head(URL);

      // Get space for the file that is going to be dowloaded
      LSize := LResponse.ContentLength;
      StFile := TFileStream.Create(LFileName, fmCreate);
      try
        STFile.Size := LSize;
      finally
        STFile.Free;
      end;

      // Split the file in four blocks
      LFragSize := LSize div NumThreads;
      LStart := 0;
      LEnd := LStart + LFragSize;

      SetLength(LDownloadThreads, NumThreads);
      for I := 0 to NumThreads - 1 do
      begin
        if FClosingForm then
          Break;

        // Create the Thread
        LDownloadThreads[I] := TDownloadThread.Create(URL, LFileName, I, LStart, LEnd);
        LDownloadThreads[I].OnThreadData := ReceiveThreadDataEvent;

        TThread.Synchronize(nil, procedure
        begin
          // Adjust the ProgressBar Max Value
          if LEnd >= LSize then
          begin
            ProgressBarArray[I].Max := LFragSize - (LEnd - LSize);
            LEnd := LSize;
          end
          else
            ProgressBarArray[I].Max := LFragSize;
          ProgressBarArray[I].Min := 0;
          ProgressBarArray[I].Value := 0;

          ButtonCancelArray[I].Enabled := True;
          LabelProgressArray[I].Text := '0 KB/s';
        end);

        // Update Start and End Values
        LStart := LStart + LFragSize;
        LEnd := LStart + LFragSize;
      end;

      // Start the download process
      LStartTime := TThread.GetTickCount;
      for I := 0 to NumThreads - 1 do
        LDownloadThreads[I].Start;

      // Wait until all threads finish
      LFinished := False;
      while not LFinished and not FClosingForm do
      begin
        LFinished := True;
        for I := 0 to NumThreads - 1 do
          LFinished := LFinished and LDownloadThreads[I].Finished;
      end;

      LEndTime := TThread.GetTickCount - LStartTime;
      TThread.Synchronize(nil, procedure
      begin
        LabelGlobalSpeed.Text := Format('Global Speed: %d KB/s', [((LSize*1000) div LEndTime) div 1024]);
      end);

      // Cleanup Threads
      for I := 0 to NumThreads - 1 do
        LDownloadThreads[I].Free;

    end
    else
    begin
      TThread.Synchronize(nil, procedure
      begin
        Memo1.Lines.Add('Server has NOT resume download feature');
      end);
    end;
  finally
    LClient.Free;
    TThread.Synchronize(nil, procedure
    begin
     BStartDownload.Enabled := True;
    end);
    FAllowFormClose := True;

  end;
end;

{ TDownloadThread }

constructor TDownloadThread.Create(const URL, FileName: string; ThreadNo: Integer; StartPoint, EndPoint: Int64);
begin
  inherited Create(True);
  FURL := URL;
  FFileName := FileName;
  FThreadNo := ThreadNo;
  FStartPoint := StartPoint;
  FEndPoint := EndPoint;
end;

destructor TDownloadThread.Destroy;
begin
  inherited;
end;

procedure TDownloadThread.Execute;
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
      LStream.Seek(FStartPoint, TSeekOrigin.soBeginning);
      LResponse := LHttpClient.GetRange(FURL, FStartPoint, FEndPoint, LStream);
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
  if Assigned(FOnThreadData) then
  begin
    LTime := GetTickCount - FTimeStart;
    if AReadCount = 0 then
      LSpeed := 0
    else
      LSpeed := (AReadCount * 1000) div LTime;

    FOnThreadData(Sender, FThreadNo, LSpeed, AContentLength, AReadCount, Abort);
  end;
end;

end.
