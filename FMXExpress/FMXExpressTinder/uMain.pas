unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Generics.Collections,   System.Messaging, System.Json, System.DateUtils,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, FMX.StdCtrls, FMX.TMSBitmapContainer, FMX.ListView,
  FMX.Layouts, FMX.Memo, FMX.TMSBaseControl, FMX.TMSTileList, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, Xml.xmldom, Xml.XMLIntf, Xml.adomxmldom,
  Xml.XMLDoc, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client, FMX.Objects,
  FMX.Ani, FMX.Filter.Effects, FMX.Effects, FMX.TMSBitmap, System.Actions,
  FMX.ActnList, FMX.StdActns, FMX.MediaLibrary.Actions, FMX.TMSBarButton,
  FMX.ListBox, FMX.MultiView, System.IniFiles, System.IOUtils, IPPeerClient,
  REST.Client, Data.Bind.Components, Data.Bind.ObjectScope, FMX.Gestures,
  Data.Bind.EngExt, Fmx.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs,
  Fmx.Bind.Editors, Data.Bind.DBScope, Data.Bind.Controls, Fmx.Bind.Navigator,
  FMX.Controls.Presentation, System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent, OpenViewUrl
{$IFDEF ANDROID}
, Androidapi.Helpers, FMX.Platform.Android, Androidapi.JNI.GraphicsContentViewText, FMX.Helpers.Android,
Androidapi.JNI.JavaTypes, AndroidApi.JniBridge, AndroidApi.Jni.App,
AndroidAPI.jni.OS
{$ENDIF}
  ;

type
    TProcParse = procedure (responseContent : string) of object;
    TProcOnCLick = procedure (Sender : TObject) of object;
    TChannel = class
        FURL : string;
        FURLFeed : string;
        FName : string;
        FIndex : Integer;
        FChanged : Boolean;
    end;
  TfMain = class(TForm)
    AniI: TAniIndicator;
    FDmemItem: TFDMemTable;
    FIdChanel: TIntegerField;
    FTitle: TStringField;
    FLink: TStringField;
    FDesk: TStringField;
    FPubDate: TStringField;
    FImage: TStringField;
    FId: TFDAutoIncField;
    TMSBmpContainer: TTMSFMXBitmapContainer;
    FXMLDoc: TXMLDocument;
    tlbHeader: TToolBar;
    lblHeaderLabel: TLabel;
    btnRefresh: TButton;
    btnMainMenu: TButton;
    imgLogo: TImage;
    multviewMain: TMultiView;
    lytMain: TLayout;
    fltnmtnMenu: TFloatAnimation;
    Timer: TTimer;
    rctnglMain: TRectangle;
    rctnglLogin: TRectangle;
    grdpnlytLogin: TGridPanelLayout;
    btnLater: TButton;
    btnLogin: TButton;
    lblAuth: TLabel;
    restClient: TRESTClient;
    restResponse: TRESTResponse;
    restRequest: TRESTRequest;
    lvMenu: TListView;
    rctngl: TRectangle;
    fltnmtnX: TFloatAnimation;
    rctnglNext: TRectangle;
    lytLikeDisLike: TLayout;
    rctnglMouseEvent: TRectangle;
    rctnglHeartLike: TRectangle;
    fltnmtnReset: TFloatAnimation;
    fltnmtnScaleXLike: TFloatAnimation;
    imgNotFound: TImage;
    ColorKeyAlphaEffectImageNotFound: TColorKeyAlphaEffect;
    rctnglHeartDisLike: TRectangle;
    fltnmtnHeart: TFloatAnimation;
    fltnmtnScaleYHeart: TFloatAnimation;
    fltnmtnScaleXDisLike: TFloatAnimation;
    HTTPGet: TNetHTTPClient;

    procedure FormCreate(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure lstMenuItemClick(const Sender: TCustomListBox;
      const Item: TListBoxItem);
    procedure btnLaterClick(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
    procedure restRequestAfterExecute(Sender: TCustomRESTRequest);
    procedure lvMenuItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure TimerTimer(Sender: TObject);
    procedure rctnglMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure rctnglMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure rctnglMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure rctnglMouseLeave(Sender: TObject);
    procedure fltnmtnHeartLikeFinish(Sender: TObject);
    procedure fltnmtnXFinish(Sender: TObject);
    procedure rctnglMouseEventDblClick(Sender: TObject);
    procedure rctnglMouseEventTap(Sender: TObject; const Point: TPointF);

  private
    FTimeStartLoadImages : TDateTime;
    FMouseDown : Boolean;
    FCurrentChannel : TChannel;
    FCurrentImageId : integer;
    FListChannel : TList<TChannel>;
{::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::}
    procedure LogAdd(Msg : string);
    procedure ParseRequest(responseContent : string);
    procedure AddRSSItem(ATitle, ALink, ADesc, AImage, ADate: String; idChannel : Integer = 0);
    procedure TerminateThreadLoadRss(Sender : TObject);
    procedure TerminateThreadLoadImage(Sender : TObject);
    procedure LoadImages(const list : TList<string>);
    procedure LoadImage(aURL : string);
    procedure SetAniIndicator(ani : TAniIndicator; enabled : Boolean);
    procedure ChangeChannel(index : Integer);
    procedure RefreshRss;
    procedure InstallShortcut;
    procedure ShowDialog(avisible : Boolean; Msg : string = ''; time : Integer = 0);
    procedure LoadListViewForArray(listview : TListView; arrayItem : TArray<String>);
    procedure LoadRectangleBitmap(id, idNext : Integer; rect, rectNext : TRectangle);
    procedure LoadRectangleEndSwipe(Sender : TRectangle);
    procedure LoadRectangleEndThreadLoad;
    procedure UpdateAAnimation(aFltnmtn : TFloatAnimation; startV, stopV : Single; startFromCurrent : boolean = true;
         duration : single = 0.1);
    procedure NextImage(Msg : string);
    procedure SetFMouseDown(const value : boolean);
    procedure SetVisibleRectangle(rect : TRectangle; aVisible : boolean);
    procedure ChangeVisLikeDislike(visMain : Boolean = false; visLike : Boolean = false;
         visDislike : Boolean = false; firstLoad : Boolean = false);
    procedure SetOnFinishfltnmtnX(value : TNotifyEvent);
{::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::}
    function LoadRSS(aURL : string; procParse : TProcParse) : Boolean;
    function IsExistsChannelData(index : Integer) : Boolean;
    function SetOrGetUserEmail(email : string; IsGet : boolean = true) : boolean;


  public
  {$IFDEF ANDROID}
    FIntentFacebook : JIntent;
  {$ENDIF}
    FMessageSubscriptionID : integer;
    FToken : string;
    FUserEmail : string;
    FShareMessage, FLikeURL : string;
    FLoading : Boolean;
    cou : integer;
    FImageList : TList<string>;
    FCountLoadedImage : integer;
    thrList : TList<TThread>;
    lastid : integer;
    FMouseDownX : Single;
    FMouseDownY : Single;
    FPrevX : Single;
    FPrevY : Single;
    FChangeX : Single;
    FSwipeToLos : Boolean;
{::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::}
    property MouseDown : Boolean read FMouseDown write SetFMouseDown;
    property OnFinishfltnmtnX: TNotifyEvent write SetOnFinishfltnmtnX;
  end;
const
    FDefaultDuration : Single = 0.5;
    FDefaultMargin : Single = 10;
    FDefaultMove : Single = 100;
    sLike : string = 'Like';
    sDisLike : string = 'DisLike';
    sNextImageNotFound : string = 'Next Image Not Found';
    sTextTextBtnShare : string = 'Share';
    sTextTextBtnLike : string = 'Like';
    idefautIndexChannel : integer = 0;
    idefautRowCount : Integer = 1;
    iCountParallelThread : Integer = 3;
    arrayChannelUrl: TArray<String> = [
        'http://www.fmxexpress.com/feed/'
    ];
    arrayChannelName: TArray<String> = [
        'FMXExpress'
    ];
var
  fMain: TfMain;

implementation

{$R *.fmx}
procedure TfMain.InstallShortcut;
{$IFDEF ANDROID}
var
  ShortcutIntent: JIntent;
  addIntent: JIntent;
  wIconIdentifier : integer;
  wIconResource : JIntent_ShortcutIconResource;
{$ENDIF}
begin
{$IFDEF ANDROID}

  ShortcutIntent := TJIntent.JavaClass.init(SharedActivityContext, SharedActivityContext.getClass);
  ShortcutIntent.setAction(TJIntent.JavaClass.ACTION_MAIN);

  addIntent := TJIntent.Create;
  addIntent.putExtra(TJIntent.JavaClass.EXTRA_SHORTCUT_INTENT, TJParcelable.Wrap((shortcutIntent as ILocalObject).GetObjectID));
  // here we need to cast the intent as it's not done in delphi by default, not like java
  addIntent.putExtra(TJIntent.JavaClass.EXTRA_SHORTCUT_NAME, StringToJString(Application.Title));
  addIntent.setAction(StringToJString('com.android.launcher.action.INSTALL_SHORTCUT'));
  // get icon resource identifier
  wIconIdentifier := SharedActivity.getResources.getIdentifier(StringToJString('ic_launcher'), StringToJString('drawable'), StringToJString('com.legendaryfind.app'));
  // if the app name change, you must change the package name
  wIconResource := TJIntent_ShortcutIconResource.JavaClass.fromContext(SharedActivityContext, wIconIdentifier);
  // set icon for shortcut
  addIntent.putExtra(TJIntent.JavaClass.EXTRA_SHORTCUT_ICON_RESOURCE, TJParcelable.Wrap((wIconResource as ILocalObject).GetObjectID));

  SharedActivityContext.sendBroadcast(addIntent);
{$ENDIF}
end;
{::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::}
procedure TfMain.NextImage(Msg : string);
begin
    LoadRectangleBitmap(FCurrentImageId + 1, FCurrentImageId + 2, rctngl, rctnglNext);
    if TMSBmpContainer.Items.Count < FCurrentImageId+2 then
        Msg := sNextImageNotFound;
end;
{::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::}
procedure TfMain.AddRSSItem(ATitle, ALink, ADesc, AImage, ADate: String;
  idChannel: Integer);
var
    item : TTMSFMXTile;
    IsNeedPost : Boolean;
begin
    try
        LogAdd('AddRSSItem');
        FDmemItem.Filtered := False;
        IsNeedPost := not FDmemItem.Locate('Link;PubDate', VarArrayOf([ALink, ADate]));
        if IsNeedPost then
        begin
            FDmemItem.AppendRecord([idChannel, ATitle, ALink, ADesc, ADate, AImage]);
            if not Assigned(FImageList) then
                FImageList := TList<string>.Create;
            FImageList.Add(AImage);
            FListChannel.Items[idChannel].FChanged := True;
        end;
        FLoading := IsNeedPost;
        Application.ProcessMessages;
    except
        on e : Exception do
            LogAdd('Error AddRSSItem : ' + e.Message);
    end;
end;
{::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::}
procedure TfMain.btnLaterClick(Sender: TObject);
begin
    ShowDialog(False);
end;
{::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::}
procedure TfMain.btnLoginClick(Sender: TObject);
begin
  {$IFDEF ANDROID}
    try
        SharedActivity.startActivityForResult(FIntentFacebook, 0);
    except
        ShowDialog(False);
    end;
  {$ENDIF}
end;
{::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::}
procedure TfMain.btnRefreshClick(Sender: TObject);
begin
    RefreshRss;
end;
{::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::}
procedure TfMain.ChangeChannel(index : Integer);
var
    i : integer;
    channel : TChannel;
begin
    if not Assigned(FListChannel) then
    begin
        FListChannel := TList<TChannel>.Create;
        for i := 0 to Length(arrayChannelUrl) - 1 do
        begin
            channel := TChannel.Create;
            channel.FURL := arrayChannelUrl[i];
            channel.FURLFeed := arrayChannelUrl[i];
            channel.FName := arrayChannelName[i];
            channel.FIndex := i;
            FListChannel.Add(channel);
        end;
    end;
    if index < FListChannel.Count then
    begin
        if not Assigned(FCurrentChannel) or (index <> FCurrentChannel.FIndex) then
        begin
            FCurrentChannel := FListChannel.Items[index];
            FCurrentChannel.FChanged := true;
            RefreshRss;
        end;
    end;
end;
{::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::}
procedure TfMain.ChangeVisLikeDislike(visMain, visLike, visDislike,
  firstLoad: Boolean);
begin
    lytLikeDisLike.Visible := visMain;
    rctnglHeartLike.Visible := visLike;
    rctnglHeartDisLike.Visible := visDislike;
    if firstLoad then
    begin
        fltnmtnHeart.Delay := fltnmtnScaleYHeart.Duration;
    end;
end;
{::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::}
procedure TfMain.fltnmtnHeartLikeFinish(Sender: TObject);
begin
    TRectangle(TFloatAnimation(Sender).Parent).Visible  := false;
end;
{::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::}
procedure TfMain.fltnmtnXFinish(Sender: TObject);
begin
    OnFinishfltnmtnX := nil;
    NextImage(EmptyStr);
    UpdateAAnimation(fltnmtnX, TRectangle(TFloatAnimation(Sender).Parent).Position.X, FDefaultMargin, True, 0.001);
    TRectangle(TFloatAnimation(Sender).Parent).Align := TAlignLayout.Client;
end;
{::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::}
procedure TfMain.FormCreate(Sender: TObject);
begin
    if FDmemItem.Active then
        FDmemItem.Close;
    FDmemItem.Open;
    FLoading := False;
    imgNotFound.Visible := True;
    FCurrentImageId := -1;
    ChangeVisLikeDislike;
    FSwipeToLos := false;
    OnFinishfltnmtnX := nil;
    rctngl.Visible := false;
    rctnglNext.Visible := false;
    LoadListViewForArray(lvMenu, arrayChannelName);
end;
{::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::}
function TfMain.IsExistsChannelData(index: Integer): Boolean;
begin
    FDmemItem.Filtered := False;
    Result := FDmemItem.Locate('IdChanel', index);
end;
{::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::}
procedure TfMain.LoadImage(aURL: string);
var
    thread : TThread;
begin
    try
        if aURL.Trim.IsEmpty then
            Exit;
        if not Assigned(thrList) then
            thrList := TList<TThread>.Create;
        LogAdd('LoadImage : ' + aURL);
        thread :=  TThread.CreateAnonymousThread(
            procedure ()
            var
                bmpItem : TTMSFMXBitmapItem;
                item : TTMSFMXTile;
                url : string;
                stream: TMemoryStream;
            begin
                begin
                    stream:= TMemoryStream.Create;
                    url :=  aURL;
                    HTTPGet.Get(url, stream);
                    //HttpGetBinary(url, stream);
                    stream.Position := 0;
                    TThread.Synchronize (TThread.CurrentThread,
                    procedure ()
                    var
                        tempBitMap : TBitmap;
                    begin
                        tempBitMap := TBitmap.Create;
                        tempBitMap.LoadFromStream(stream);
                        if not tempBitMap.IsEmpty then
                        begin
                            bmpItem := TMSBmpContainer.Items.Add;
                            bmpItem.Bitmap := tempBitMap;//.LoadFromStream(stream);
                            bmpItem.Name := url;
                            LoadRectangleEndThreadLoad;
                        end;
                    end);
                end;
            end);
        thrList.Add(thread);
        thread.NameThreadForDebugging('ThreadLoadImage ¹ : ' + IntToStr(thrList.Count));
        thread.FreeOnTerminate := True;
        thread.OnTerminate :=  self.TerminateThreadLoadImage;
        if thrList.Count < iCountParallelThread then
            thread.Start;
    except
        on e : Exception do
            LogAdd('Error LoadImage : ' + e.Message);
    end;
end;
{::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::}
procedure TfMain.LoadImages(const list: TList<string>);
var
    i, count : integer;
begin
    try
        if not Assigned(list) then
            exit;
        count := list.Count;
        FCountLoadedImage := count;
        FTimeStartLoadImages := Now;
        LogAdd('LoadImages.Count : ' + IntToStr(count));
        for i := 0 to count - 1 do
        begin
            LoadImage(list.Items[i]);
        end;
    except
        on e : Exception do
            LogAdd('Error LoadImage : ' + e.Message);
    end;
end;
{::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::}
procedure TfMain.LoadListViewForArray(listview: TListView;
  arrayItem: TArray<String>);
var
    i : integer;
    item : TListViewItem;
begin
    if not Assigned(listview) then
        exit;
    listview.ClearItems;
    for i := 0 to Length(arrayItem) - 1 do
    begin
        item := listview.Items.Add;
        item.Text := arrayItem[i];
    end;
end;
{::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::}
procedure TfMain.LoadRectangleEndSwipe(Sender : TRectangle);
var
    XtoLos : Single;
    swipe : Boolean;
begin
    if not Assigned(Sender) then
        exit;
    MouseDown := false;
    swipe := Abs(FChangeX) > FDefaultMove;
    if FChangeX < -FDefaultMove then
    begin
        //NextImage(sLike);
        XtoLos := Sender.Width;
        ChangeVisLikeDislike(True, True);
    end;
    if FChangeX > FDefaultMove then
    begin
        //NextImage(sDisLike);
        XtoLos := -Sender.Width;
        ChangeVisLikeDislike(True, False, True);
    end;
    //swipe to los
    if swipe then
    begin
        UpdateAAnimation(fltnmtnX, Sender.Position.X, XtoLos, true, FDefaultDuration);
        OnFinishfltnmtnX := Self.fltnmtnXFinish;
    end
    else
    begin
        UpdateAAnimation(fltnmtnX, Sender.Position.X, FDefaultMargin, True, 0.001);
        Sender.Align := TAlignLayout.Client;
    end;
end;
{::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::}
procedure TfMain.LoadRectangleEndThreadLoad;
begin
    //loading image
    if rctngl.Fill.Bitmap.Bitmap.IsEmpty then
    begin
        LoadRectangleBitmap(FCurrentImageId + 1, FCurrentImageId + 2, rctngl, rctnglNext);
    end
        else
        if rctnglNext.Fill.Bitmap.Bitmap.IsEmpty then
            LoadRectangleBitmap(FCurrentImageId + 1, FCurrentImageId + 1, nil, rctnglNext);
end;
{::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::}
procedure TfMain.LoadRectangleBitmap(id, idNext : Integer; rect, rectNext : TRectangle);     //temp proc
    procedure load(id : Integer; rect : TRectangle; Current : Boolean = True);
    begin
        if not Assigned(rect) then
            exit;
        if (TMSBmpContainer.Items.Count > id) and not TMSBmpContainer.Items[id].Bitmap.IsEmpty then
        begin
            rect.Fill.Bitmap.Bitmap := TMSBmpContainer.Items[id].Bitmap;
            if Current then
                FCurrentImageId := id;
        end
        else
            rect.Fill.Bitmap.Bitmap := nil;
        SetVisibleRectangle(rect, not rect.Fill.Bitmap.Bitmap.IsEmpty);
    end;
begin
    if FCurrentImageId = id then
       exit;
    //on load next image visible = false Like and DisLike
    ChangeVisLikeDislike;
    load(idNext, rectNext, false);
    load(id, rect);
    if Assigned(rctnglNext) then
        rctnglNext.SendToBack;
    if Assigned(rctngl) then
        rctngl.BringToFront;
end;
{::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::}
function TfMain.LoadRSS(aURL: string; procParse: TProcParse): Boolean;
var
    thread : TThread;
begin
    if aURL.Trim.IsEmpty then
    begin
        Result := false;
        exit;
    end;
    if not Assigned(thrList) then
        thrList := TList<TThread>.Create;
    thread :=  TThread.CreateAnonymousThread(
        procedure ()
        var
            st : TStringList;
            stream: TStringStream;
        begin
            stream := TStringStream.Create;
            st := TStringList.Create;
            HTTPGet.Get(aURL, stream);
            stream.Seek(0,TSeekOrigin.soBeginning);
            st.LoadFromStream(stream);
            //HttpGetText(aURL, st);
            TThread.Synchronize (TThread.CurrentThread,
            procedure ()
            begin
                if Assigned(procParse) then
                    procParse(st.Text);
            end);
            FreeAndNil(st);
            FreeAndNil(stream);
        end);
    thrList.Add(thread);
    thread.FreeOnTerminate := True;
    thread.OnTerminate :=  self.TerminateThreadLoadRss;
    thread.Start;
    Result := true;
end;
{::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::}
procedure TfMain.LogAdd(Msg: string);
var
    temp : string;
begin
    temp := Msg;
    //temp procedure
end;
{::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::}
procedure TfMain.lstMenuItemClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
var
    index : integer;
begin
    self.multviewMain.HideMaster;
    if FLoading then
        exit;
    index := Item.Index;
    ChangeChannel(index);
end;
{::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::}
procedure TfMain.ParseRequest(responseContent : string);
var
    ANode: IXmlNode;
    AList, AList2: TList;
    I, II, III: Integer;
    AMeta: TXmlNode;
    AEnum: TXmlNode;
    ATitle, ALink, AType, ADate, ADesc, AImage, AHTML: String;
    countRssChild, countItem : Integer;
begin
  try
    if (not responseContent.Trim.IsEmpty) and (FLoading) then
    begin
        LogAdd('ParseRequest');

        FXMLDoc.LoadFromXML(responseContent);
        FXMLDoc.Active := True;

        ANode := FXMLDoc.ChildNodes.FindNode('rss');
        if not Assigned(ANode) then
            exit;
        if not Assigned(FImageList) then
            FImageList := TList<string>.Create
        else
            FImageList.Clear;

        countRssChild := ANode.ChildNodes.Count;
        for I := 0 to countRssChild - 1 do
        begin

        if (IXMLNode(ANode.ChildNodes[I]).NodeName='channel') then
        begin
          for II := 0 to IXMLNode(ANode.ChildNodes[I]).ChildNodes.Count - 1 do
          begin

            if(IXMLNode(IXMLNode(ANode.ChildNodes[I]).ChildNodes[II]).NodeName='item') then
            begin
              countItem := IXMLNode(IXMLNode(ANode.ChildNodes[I]).ChildNodes[II]).ChildNodes.Count;

              try
                  for III := 0 to countItem - 1 do
                  begin
                    if(IXMLNode(IXMLNode(IXMLNode(ANode.ChildNodes[I]).ChildNodes[II]).ChildNodes[III]).NodeName='title') then
                    begin
                      ATitle := IXMLNode(IXMLNode(IXMLNode(ANode.ChildNodes[I]).ChildNodes[II]).ChildNodes[III]).Text;
                    end;
                    if(IXMLNode(IXMLNode(IXMLNode(ANode.ChildNodes[I]).ChildNodes[II]).ChildNodes[III]).NodeName='link') then
                    begin
                      ALink := IXMLNode(IXMLNode(IXMLNode(ANode.ChildNodes[I]).ChildNodes[II]).ChildNodes[III]).Text;
                    end;
                    if(IXMLNode(IXMLNode(IXMLNode(ANode.ChildNodes[I]).ChildNodes[II]).ChildNodes[III]).NodeName='description') then
                    begin
                      ADesc := IXMLNode(IXMLNode(IXMLNode(ANode.ChildNodes[I]).ChildNodes[II]).ChildNodes[III]).Text;
                    end;
                    if(IXMLNode(IXMLNode(IXMLNode(ANode.ChildNodes[I]).ChildNodes[II]).ChildNodes[III]).NodeName='content:encoded') then
                    begin
                      AHTML := IXMLNode(IXMLNode(IXMLNode(ANode.ChildNodes[I]).ChildNodes[II]).ChildNodes[III]).Text;
                    end;
                    if(IXMLNode(IXMLNode(IXMLNode(ANode.ChildNodes[I]).ChildNodes[II]).ChildNodes[III]).NodeName='pubDate') then
                    begin
                      ADate := IXMLNode(IXMLNode(IXMLNode(ANode.ChildNodes[I]).ChildNodes[II]).ChildNodes[III]).Text;
                    end;
                  end;

                  AImage := Copy(ADesc,Pos('src="',ADesc)+5,Pos('" class="',ADesc)-(Pos('src="',ADesc)+5));

                  AddRSSItem(ATitle, ALink, ADesc, AImage, ADate, FCurrentChannel.FIndex);
                except
                    on e : Exception do
                        LogAdd('Error ParseRequest : ' + e.Message);
                end;
              Application.ProcessMessages;

            end;
          end;
        end;
        end;
    end;
    except
        on e : Exception do
            LogAdd('Error ParseRequest Exit : ' + e.Message);
    end;
end;
{::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::}
procedure TfMain.rctnglMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
    FMouseDownX := X;
    FMouseDownY := Y;
    FPrevX := X;
    FPrevY := Y;
    MouseDown := not rctngl.Fill.Bitmap.Bitmap.IsEmpty and not FSwipeToLos;
    rctngl.Align := TAlignLayout.None;
    FChangeX := 0;
end;
procedure TfMain.rctnglMouseEventDblClick(Sender: TObject);
begin
FDmemItem.RecNo := FCurrentImageId;
OpenURL(FDmemItem['Link']);
end;

procedure TfMain.rctnglMouseEventTap(Sender: TObject; const Point: TPointF);
begin
FDmemItem.RecNo := FCurrentImageId;
OpenURL(FDmemItem['Link']);
end;

{::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::}
procedure TfMain.rctnglMouseLeave(Sender: TObject);
begin
    if MouseDown then
       LoadRectangleEndSwipe(rctngl);
end;
{::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::}
procedure TfMain.rctnglMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
const
    intput : Integer = 1;
begin
    if MouseDown and ((X - FPrevX < - intput) or (X - FPrevX > intput) or (Y - FPrevY < - intput) or (Y - FPrevY > intput)) then
    begin
        UpdateAAnimation(fltnmtnX, rctngl.Position.X, X - FMouseDownX + FDefaultMargin);
        FChangeX := FChangeX + FPrevX - X;
        FPrevX := X;
        FPrevY := Y;
        // -100..-200 Like Opacity 0..100
        //rctnglLike.Opacity := -FChangeX / 100;
        // 100..200 DisLike Opacity 0..100
        //rctnglDisLike.Opacity := FChangeX / 100;
    end;
end;
{::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::}
procedure TfMain.rctnglMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
    LoadRectangleEndSwipe(rctngl);//LoadRectangle(TRectangle(Sender));
end;
{::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::}
procedure TfMain.RefreshRss;
begin
    LogAdd('btnRefreshBTNClick');
    if FLoading then
        exit;
    if not Assigned(FCurrentChannel) then
    begin
        ChangeChannel(idefautIndexChannel);
        exit;
    end;
    FLoading := True;
    if LoadRSS(FCurrentChannel.FURLFeed, ParseRequest)then
    begin
        SetAniIndicator(AniI, True);
    end;
end;
{::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::}
procedure TfMain.restRequestAfterExecute(Sender: TCustomRESTRequest);
var
    Enums: TJSONPairEnumerator;
    sJson : string;
begin
    try
        if not Assigned(restResponse.JSONValue) then
        begin
            Exit;
        end;
        FUserEmail := (restResponse.JSONValue as TJSONObject).GetValue('email').Value;
        SetOrGetUserEmail(FUserEmail, False);
    except

    end;
end;
{::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::}
procedure TfMain.SetAniIndicator(ani: TAniIndicator; enabled: Boolean);
begin
    //FLoading := enabled;
    if not Assigned(ani) then
        exit;
    ani.Enabled := enabled;
    ani.Visible := enabled;
end;
{::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::}
procedure TfMain.SetFMouseDown(const value: boolean);
begin
    FMouseDown := value;
   { if not value then
    begin
        ChangeVisLikeDislike;
    end;     }
end;
{::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::}
procedure TfMain.SetOnFinishfltnmtnX(value: TNotifyEvent);
begin
    fltnmtnX.OnFinish := value;
    FSwipeToLos := Assigned(value);
end;
{::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::}
function TfMain.SetOrGetUserEmail(email: string; IsGet: boolean): boolean;
var
    SettIniFile: TMemIniFile;
    FilePath: String;
    tempEmail: String;
begin
    Result := false;
    {$IFDEF MSWINDOWS}
    FilePath := ExtractFilePath(ParamStr(0));
    {$ELSE}
    FilePath := System.IOUtils.TPath.GetDocumentsPath + System.SysUtils.PathDelim;
    {$ENDIF}

    SettIniFile := TMemIniFile.Create(FilePath + 'settings.ini');
    if IsGet then
    begin
        tempEmail := SettIniFile.ReadString('User','Email',string.Empty);
        Result := not tempEmail.IsEmpty;
        Exit;
    end
    else
    begin
        SettIniFile.WriteString('User','Email',email);
        SettIniFile.UpdateFile;
     end;
    SettIniFile.Free;
    Result := True;
end;
{::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::}
procedure TfMain.SetVisibleRectangle(rect : TRectangle; aVisible : boolean);
begin
    if not Assigned(rect) then
        Exit;
    rect.Visible := aVisible;
    if rect = rctngl then
        imgNotFound.Visible := not aVisible;
end;
{::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::}
procedure TfMain.ShowDialog(avisible : Boolean; Msg : string = ''; time : Integer = 0);
begin
    Self.rctnglMain.Visible := avisible;
    Self.rctnglLogin.Visible := avisible;
    if not Msg.IsEmpty then
        self.lblAuth.Text := Msg;
end;
{::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::}
procedure TfMain.TerminateThreadLoadImage(Sender: TObject);
var
    i, count : integer;
begin
    if not Assigned(FImageList) then
        exit;
    FCountLoadedImage := FCountLoadedImage - 1;
    //off load Indicator
    if FCountLoadedImage = 0 then
    begin
        SetAniIndicator(AniI, False);
        FLoading := false;
    end;
    count :=  thrList.Count - 1;
    for I := 0 to count do
        if thrList.Items[i].Suspended then
        begin
            thrList.Items[i].Start;
            Break;
        end;
    thrList.Remove(TThread(Sender));
end;
{::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::}
procedure TfMain.TerminateThreadLoadRss(Sender: TObject);
begin
    if Assigned(FImageList) and (FImageList.Count > 0) then
        LoadImages(FImageList)
    else
    begin
        LogAdd('TerminateThreadLoadRss : Not Images');
        SetAniIndicator(AniI, false);
    end;
    thrList.Remove(TThread(Sender));
end;
{::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::}
procedure TfMain.TimerTimer(Sender: TObject);
var
    SettIniFile: TMemIniFile;
    FilePath: String;
    CreateShortcut: String;
begin
exit;
    {$IFDEF MSWINDOWS}
    FilePath := ExtractFilePath(ParamStr(0));
    {$ELSE}
    FilePath := System.IOUtils.TPath.GetDocumentsPath + System.SysUtils.PathDelim;
    {$ENDIF}

    SettIniFile := TMemIniFile.Create(FilePath + 'settings.ini');
    CreateShortcut := SettIniFile.ReadString('Settings','Shortcut','No');
    if CreateShortcut='No' then
     begin
      InstallShortcut;
      SettIniFile.WriteString('Settings','Shortcut','Yes');
      SettIniFile.UpdateFile;
     end;
    SettIniFile.Free;
    Timer.Enabled := False;
end;
{::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::}
procedure TfMain.UpdateAAnimation(aFltnmtn: TFloatAnimation; startV,
  stopV: Single; startFromCurrent: boolean; duration : single);
begin
    if not Assigned(aFltnmtn) then
        exit;
    aFltnmtn.Duration := duration;
    aFltnmtn.StartValue := startV;
    aFltnmtn.StopValue := stopV;
    aFltnmtn.StartFromCurrent := startFromCurrent;
    aFltnmtn.Start;
end;
{::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::}
procedure TfMain.lvMenuItemClick(const Sender: TObject;
  const AItem: TListViewItem);
var
    index : integer;
begin
    self.multviewMain.HideMaster;
    if FLoading then
        exit;
    index := AItem.Index;
end;
{::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::}
end.
