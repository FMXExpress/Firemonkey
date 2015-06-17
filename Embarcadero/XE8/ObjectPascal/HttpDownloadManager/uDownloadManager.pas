unit uDownloadManager;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts, FMX.ListBox, FMX.StdCtrls, FMX.Edit,
  FMX.Controls.Presentation, FMX.ListView.Types, FMX.ListView, FMX.EditBox, FMX.SpinBox, uDownloadThread;

type
  TFrDownloadManager = class(TForm)
    Panel1: TPanel;
    EdUrl: TEdit;
    BtnGetLinks: TButton;
    LbLinks: TListBox;
    BtnExplore: TButton;
    BtnDownload: TButton;
    Panel2: TPanel;
    LbDownloads: TListBox;
    EdFileName: TEdit;
    btnPause: TButton;
    btnRemove: TButton;
    BtnResume: TButton;
    EdDirect: TEdit;
    Label1: TLabel;
    StyleBook1: TStyleBook;
    procedure BtnGetLinksClick(Sender: TObject);
    procedure BtnExploreClick(Sender: TObject);
    procedure ListBoxItem1Painting(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure BtnDownloadClick(Sender: TObject);
    procedure LbLinksChange(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure LbDownloadsChange(Sender: TObject);
    procedure btnPauseClick(Sender: TObject);
    procedure BtnResumeClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure EdDirectChange(Sender: TObject);
  private
    procedure PaintItem(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure ReceiveThreadDataEvent(const Sender: TObject; ThreadNo, ASpeed: Integer; AContentLength,
      AReadCount: Int64; var Abort: Boolean; const VisualObject: TObject);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrDownloadManager: TFrDownloadManager;

implementation

uses
  System.Net.HttpClient, System.RegularExpressions, System.IOUtils, System.Net.UrlClient;

{$R *.fmx}

procedure TFrDownloadManager.ReceiveThreadDataEvent(const Sender: TObject; ThreadNo: Integer; ASpeed: Integer; AContentLength, AReadCount: Int64;
  var Abort: Boolean; const VisualObject: TObject);
var
  LCad: string;
  LCancel: Boolean;
  LSpeed: Integer;
begin
  LCancel := Abort;
  TThread.Synchronize(nil,
    procedure
    begin
      if AContentLength > 0 then
        TListBoxItem(VisualObject).TagFloat := (AReadCount / AContentLength);
      TListBoxItem(VisualObject).ItemData.Text := Format('%s %f %% completed %d/%d KB speed: %d KB/s',
        [TPath.GetFileName(TListBoxItem(VisualObject).ItemData.Detail), TListBoxItem(VisualObject).TagFloat * 100,
        AReadCount shr 10, AContentLength shr 10, ASpeed shr 10]);
      TListBoxItem(VisualObject).Repaint;
      Application.ProcessMessages;
    end);
  Abort := LCancel;
end;

procedure TFrDownloadManager.BtnDownloadClick(Sender: TObject);
var
  LClient: THTTPClient;
  URL: string;
  LResponse: IHTTPResponse;
  StFile: TFileStream;
  LFileName: string;
  LStart, LEnd, LSize, LFragSize: Int64;
  I: Integer;
  LDownloadThread: TDownloadThread;
  LFinished: Boolean;
  LStartTime, LEndTime: Cardinal;
  LItem: TListBoxItem;
begin
  LClient := THTTPClient.Create;
  LFileName := TPath.Combine(TPath.GetDocumentsPath, EdFileName.Text);
  try
    if EdDirect.Text <> '' then
    begin
      URL := EdDirect.Text;
      EdDirect.Text := '';
    end
    else
      URL := LbLinks.Items[LbLinks.ItemIndex];
    LResponse := LClient.Head(URL);

    // Get space for the file that is going to be dowloaded
    LSize := LResponse.ContentLength;
    StFile := TFileStream.Create(LFileName, fmCreate);
    try
      STFile.Size := LSize;
    finally
      STFile.Free;
    end;

    LItem := TListBoxItem.Create(LbDownloads);
    LItem.Parent := LbDownloads;
    LItem.ItemData.Text := LFileName;
    LItem.ItemData.Detail := LFileName;
    LItem.OnPainting := PaintItem;
    LItem.StyledSettings := [];

    // Create the Thread
    LDownloadThread := TDownloadThread.Create(URL, LFileName, LbDownloads.Count - 1, 0, LSize);
    LDownloadThread.VisualObject := LItem;
    LDownloadThread.CanResume := LClient.CheckDownloadResume(URL);
    LDownloadThread.OnThreadData := ReceiveThreadDataEvent;

    LItem.Data := LDownloadThread;

    Application.ProcessMessages;
    LDownloadThread.Start;
  finally
    LClient.Free;
  end;
end;

procedure TFrDownloadManager.BtnExploreClick(Sender: TObject);
begin
  if LbLinks.ItemIndex >= 0 then
  begin
    EdUrl.Text := LbLinks.Selected.Text;
    BtnGetLinksClick(nil);
  end;
end;

procedure TFrDownloadManager.BtnGetLinksClick(Sender: TObject);
const
  ExternalLinkExp = '<A?a?[\s]+[^>]*?href[\s]?=*(.*?)[\"\'']*.*?>([^<]+|.*?)?<\/A?a?>';
var
  LHttpClient: THTTPClient;
  AResponse: IHTTPResponse;
  LStringStream: TStringStream;
  LRegex: TRegEx;
  LMatchCollection: TMatchCollection;
  I: Integer;
  LNewRef: string;
  LFirst: Integer;
  LLast: Integer;
  LUrl: TURI;
begin
  if EdUrl.Text <> '' then
  begin
    LHttpClient := THTTPClient.Create;
    try
      AResponse := LHttpClient.Get(EdUrl.Text);
      LStringStream := TStringStream.Create;
      try
        LStringStream.LoadFromStream(AResponse.ContentStream);
        LRegex := TRegEx.Create(ExternalLinkExp);
        LMatchCollection := LRegex.Matches(LStringStream.DataString);
        LbLinks.Clear;

        for I := 0 to LMatchCollection.Count - 1 do
        begin
          LNewRef := LMatchCollection.Item[I].Value;
          LFirst := LNewRef.IndexOf('"');
          if LFirst >=0 then
          begin
            LLast := LNewRef.IndexOf('"', LFirst + 1);
            LNewRef := LNewRef.Substring(LFirst + 1, LLast - LFirst - 1);
            if not LNewRef.StartsWith('http') then
            begin
              LUrl := TURI.Create(EdUrl.Text);

              if LUrl.Path.EndsWith('htm', true) or LUrl.Path.EndsWith('html', true) then
                LUrl.Path := '';
              if LNewRef.StartsWith('/') then
                LUrl.Path := LUrl.Path + LNewRef
              else
                LUrl.Path := LUrl.Path + '/' + LNewRef;
              LNewRef := LUrl.ToString;
            end;
            LbLinks.items.Add(LNewRef);
          end;
        end;
      finally
        LStringStream.Free;
      end;
    finally
      LHttpClient.Free;
    end;
  end;
end;

procedure TFrDownloadManager.btnPauseClick(Sender: TObject);
var
  LThread: TDownloadThread;
begin
  if LbDownloads.Selected <> nil then
  begin
    LThread := TDownloadThread(LbDownloads.Selected.Data);
    LThread.PauseDownload;
    LbDownloadsChange(LbDownloads);
    LThread := nil;
    LbDownloads.Selected.Repaint;
  end;
end;

procedure TFrDownloadManager.btnRemoveClick(Sender: TObject);
var
  LItem: TListBoxItem;
  LThread: TDownloadThread;
begin
  if LbDownloads.ItemIndex >= 0 then
  begin
    LItem := LbDownloads.Selected;
    LThread := TDownloadThread(LItem.Data);
    LItem.Data := nil;
    LThread.Terminate;
    LThread.Free;
    LItem.Parent := nil;
    LItem.Free;
  end;
end;

procedure TFrDownloadManager.BtnResumeClick(Sender: TObject);
var
  LThread: TDownloadThread;
begin
  if LbDownloads.Selected <> nil then
  begin
    LThread := TDownloadThread(LbDownloads.Selected.Data);
    LThread.ResumeDownload;
    LbDownloadsChange(LbDownloads);
    LThread := nil;
  end;
end;

procedure TFrDownloadManager.EdDirectChange(Sender: TObject);
begin
  EdFileName.Text := TPath.GetFileName(EdDirect.Text);
end;

procedure TFrDownloadManager.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  LComponent: TObject;
  LThread: TDownloadThread;
  I: Integer;
begin
  if LbDownloads.Count > 0 then
    CanClose :=  MessageDlg('There are downloads pending, do you want to cancel them?', TMsgDlgType.mtInformation,[TMsgDlgBtn.mbOK, TMsgDlgBtn.mbCancel],0) = mrOk;

  if CanClose then
  begin
    for I := 0 to LbDownloads.ComponentCount - 1 do
    begin
      LComponent := LbDownloads.Components[I];
      if LComponent is TListBoxItem then
      begin
        LThread := TDownloadThread(TListBoxItem(LComponent).Data);
        if LThread <> nil then
        begin
          TListBoxItem(LComponent).Data := nil;
          LThread.Terminate;
          LThread.Free;
        end;
      end;
    end;
  end;
end;

procedure TFrDownloadManager.LbDownloadsChange(Sender: TObject);
var
  LThread: TDownloadThread;
begin
  if LbDownloads.Selected <> nil then
  begin
    LThread := TDownloadThread(LbDownloads.Selected.Data);
    btnPause.Enabled := LThread.CanResume and not LThread.IsPaused and not LThread.Finished;
    BtnResume.Enabled := LThread.IsPaused and not LThread.Finished;
    LThread := nil;
  end;
end;

procedure TFrDownloadManager.LbLinksChange(Sender: TObject);
begin
  if LbLinks.ItemIndex >= 0 then
    EdFileName.Text := TPath.GetFileName(LbLinks.Items[LbLinks.ItemIndex]);
end;

procedure TFrDownloadManager.ListBoxItem1Painting(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
begin
  PaintItem(Sender, Canvas, ARect);
end;

procedure TFrDownloadManager.PaintItem(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
var
  LCorner: TCorners;
  LBrush: TBrush;
  LRect1, LRect2 : TRectF;
begin
  if TListBoxItem(Sender).Data = nil then exit;
  LCorner := [TCorner.TopLeft, TCorner.TopRight, TCorner.BottomLeft, TCorner.BottomRight];
  LBrush := TBrush.Create(TBrushKind.Solid, TAlphaColorRec.Greenyellow);
  LRect1 := ARect;
  LRect1.Right := LRect1.Left + ((LRect1.Right - LRect1.Left) * TListBoxItem(Sender).TagFloat);
  try
    if TListBoxItem(Sender) = LbDownloads.Selected then
      LBrush.Color := TAlphaColorRec.Lightgrey
    else
      LBrush.Color := TAlphaColorRec.White;
    Canvas.FillRect(ARect, 1, 1, LCorner, 1, LBrush);

    if TDownloadThread(TListBoxItem(Sender).Data).IsPaused then
    begin
      if TListBoxItem(Sender) = LbDownloads.Selected then
        LBrush.Color := TAlphaColorRec.Lightgoldenrodyellow
      else
        LBrush.Color := TAlphaColorRec.Yellow;
    end
    else
    begin
      if TListBoxItem(Sender) = LbDownloads.Selected then
        LBrush.Color := TAlphaColorRec.Lightgreen
      else
        LBrush.Color := TAlphaColorRec.Greenyellow;
    end;
    Canvas.FillRect(Lrect1, 1, 1, LCorner, 1, LBrush);

  finally
    LBrush.Free;
  end;
end;

end.
