unit NewsDetailFrame;

interface

uses
  System.SysUtils,uFuncCommon, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.WebBrowser, uSkinFireMonkeyButton, uSkinFireMonkeyControl,
  uSkinFireMonkeyPanel,
  uUIFunction,
  LoadingFrame,
  uInterfaceData,
  uInterfaceClass,
  FMX.WebBrowserEx,
  uInterfaceCollection;

type
  TFrameNewsDetail = class(TFrame,IFrameHistroyVisibleEvent)
    pnlToolBar: TSkinFMXPanel;
    pnlClient: TSkinFMXPanel;
    btnSync: TSkinFMXButton;
    btnReturn: TSkinFMXButton;
    procedure btnReturnClick(Sender: TObject);
    procedure btnSyncClick(Sender: TObject);
  private
    FWebBrowser: TWebBrowserEx;
    procedure DoWebBrowserDidFinishLoad(Sender:TObject);
    procedure DoShow;
    procedure DoHide;
    { Private declarations }
  public
    News:TNews;
    FrameHistroy:TFrameHistroy;
    constructor Create(AOwner:TComponent);override;
    procedure LoadNews(ANews:TNews);
    { Public declarations }
  end;

var
  GlobalNewsDetailFrame:TFrameNewsDetail;

implementation

{$R *.fmx}

{ TFrameNewsDetail }

procedure TFrameNewsDetail.btnReturnClick(Sender: TObject);
begin
  HideLoadingFrame;

  HideFrame(Self);
  ReturnFrame(FrameHistroy);
end;

procedure TFrameNewsDetail.btnSyncClick(Sender: TObject);
begin
//  ShowLoadingFrame(pnlClient);
//
//  //Ë¢ÐÂ
//  Self.FWebBrowser.Navigate('about:blank');
//  Self.FWebBrowser.Navigate(News.URL);
  LoadNews(News);
end;

constructor TFrameNewsDetail.Create(AOwner: TComponent);
begin
  inherited;
  FWebBrowser:=nil;
end;

procedure TFrameNewsDetail.DoHide;
begin
  HideLoadingFrame;

  if FWebBrowser<>nil then
  begin
    FWebBrowser.Visible:=False;
    FWebBrowser.OnDidFinishLoad:=nil;
    FreeAndNil(FWebBrowser);
  end;
end;

procedure TFrameNewsDetail.DoShow;
begin
  if FWebBrowser<>nil then
  begin
    FWebBrowser.Visible:=False;
    FWebBrowser.OnDidFinishLoad:=nil;
    FreeAndNil(FWebBrowser);
  end;

  FWebBrowser:=TWebBrowserEx.Create(Self);
//  FWebBrowser.Align:=TAlignLayout.alClient;
  FWebBrowser.Parent:=Self.pnlClient;
  FWebBrowser.OnDidFinishLoad:=DoWebBrowserDidFinishLoad;


  FWebBrowser.Width:=1;
  FWebBrowser.Height:=1;
  FWebBrowser.Visible:=True;

  FWebBrowser.Visible:=True;
end;

procedure TFrameNewsDetail.LoadNews(ANews: TNews);
begin
  ShowLoadingFrame(pnlClient);

  News:=ANews;
  Self.FWebBrowser.Navigate(News.URL);
end;

procedure TFrameNewsDetail.DoWebBrowserDidFinishLoad(Sender: TObject);
begin
  FWebBrowser.Align:=TAlignLayout.alClient;
  HideLoadingFrame;
end;


end.
