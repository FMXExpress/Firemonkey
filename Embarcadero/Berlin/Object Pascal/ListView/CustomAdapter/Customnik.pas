//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit Customnik;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.SyncObjs,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ListView, FMX.ListView.Types, FMX.ListView.Adapters.Base,
  FMX.Controls.Presentation, FMX.StdCtrls, System.Generics.Collections, System.RegularExpressions, FMX.Layouts,
  FMX.Objects;

type
  TCustomAdapter = class;

  TForm3 = class(TForm)
    Label1: TLabel;
    Layout1: TLayout;
    Layout2: TLayout;
    Button1: TButton;
    BackdropSource: TImage;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FListView: TListViewBase;
    FAdapter: TCustomAdapter;
    FStrings: TStringList;
    FTotal: Integer;

    procedure AddItems(const HowMany: Integer);
    procedure PullRefresh(Sender: TObject);
    procedure ButtonClicked(Sender: TObject);
  public const
    //Host = 'http://localhost:8000/data/';
    Host = 'http://i.imgur.com/';
  end;

  TCustomAdapter = class(TAbstractListViewAdapter,
    IListViewAdapter,       // Essential Adapter
    IListViewTextProvider,  // Provides text content for Native Presentation
    IListViewTextButtonProvider // TextButton drawable is needed to pass click events from buttons
    )
  public const
    ThreadPoolSize = 4;     // This many threads load images
  private
    FParent: TListViewBase;
    FStrings: TStringList;
    FBitmaps: TDictionary<Integer, TBitmap>;
    FRegexMonitor: TObject;
    FUriRegex: TRegEx;
    FIdRegex: TRegEx;
    FNameRegex: TRegEx;
    FThreads: TArray<TThread>;
    FRequests: TList<Integer>;
    FBackdropImage: TImage;
    FOnButtonClicked: TNotifyEvent;

    FIndex: Integer;
    FCS: TCriticalSection;
    FSem: TSemaphore;
    FExitRequested: Boolean;
  private
    procedure ImagesLoaded;
    procedure MatchView(const Item: TListItem);
    procedure CreateThreads;
    procedure DestroyThreads;

    procedure AddIndex(Index: Integer);
    function  NextIndex(): Integer; //wait for available, -1 if end requested
    
    function GetName(const Index: Integer): string;
    function GetId(const Index: Integer): string;
    function GetUri(const Index: Integer): string;

    // IListViewTextProvider
    function GetText(const Index: Integer): string;
    function GetIndexTitle(const Index: Integer): string;
    
    procedure ButtonClicked(Sender: TObject);
    procedure SetOnButtonClicked(const Value: TNotifyEvent);

    // IListViewTextButtonProvider
    function GetTextButtonDrawable(const Index: Integer): TListItemTextButton;
  protected
    procedure DoCreateNewViews; override;
    procedure DoResetViews(const APurposes: TListItemPurposes); override;
    procedure DoResetView(const Item: TListItem); override;

    procedure StringListChanging(Sender: TObject);
    procedure StringListChange(Sender: TObject);

    function GetCount: Integer;
    function GetItem(const Index: Integer): TListItem;
    function CreateStubItem: TListItem;
    function IndexOf(const AItem: TListItem): Integer;
    function GetEnumerator: TEnumerator<TListItem>;
    function GetDefaultViewHeight: Integer;
  public
    constructor Create(const Parent: TListViewBase; const AStrings: TStringList);
    destructor Destroy; override;
    property BackdropImage: TImage read FBackdropImage write FBackdropImage;
    property OnButtonClicked: TNotifyEvent read FOnButtonClicked write SetOnButtonClicked;
  end;

var
  Form3: TForm3;

implementation

uses
  IdHTTP;


{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}
{$R *.iPhone47in.fmx IOS}

procedure TForm3.AddItems(const HowMany: Integer);
const
  Cats: array [0..4] of string =
    ('SpCbHBI.jpg',
     'aMlUpJB.jpg',
     'fmXnXWn.png',
     'IWSnWNt.jpg',
     'QgA69dC.png');

  Names: array [0..9] of string =
    ('Molly', 'Charlie', 'Tigger', 'Poppy', 'Oscar', 'Smudge', 'Millie', 'Daisy', 'Max', 'Jasper');

  function NextItemText: string;
  begin
    Result := Format('[%d] [%s] [%s%s]', [FTotal, Names[FTotal mod Length(Names)], Host, Cats[FTotal mod Length(Cats)]]);
    Inc(FTotal);
  end;

var
  I: Integer;
begin
  for I := 0 to HowMany - 1 do
    FStrings.Add(NextItemText);
end;

procedure TForm3.Button1Click(Sender: TObject);
begin
  AddItems(30);
end;

procedure TForm3.ButtonClicked(Sender: TObject);
begin
  ShowMessage(TListItemText(TListItem(TListItemTextButton(Sender).TagObject).View.FindDrawable('title')).Text + ' says hi!');
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  FListView := TPresentedListView.Create(Self);
  FListView.ControlType := TControlType.Platform;
  FStrings := TStringList.Create;
  FAdapter := TCustomAdapter.Create(FListView, FStrings);
  FAdapter.BackdropImage := BackdropSource;
  FAdapter.OnButtonClicked := ButtonClicked;
  FListView.Adapter := FAdapter;

  FListView.Parent := Layout1;
  FListView.Align := TAlignLayout.Client;

  FListView.PullToRefresh := True;
  FListView.PullRefreshWait := True;
  FListView.OnPullRefresh := PullRefresh;
end;

procedure TForm3.FormDestroy(Sender: TObject);
begin
  FListView.Adapter := nil;
  FAdapter.Free;
  FListView.Free;
  FStrings.Free;
end;

procedure TForm3.PullRefresh(Sender: TObject);
begin
  AddItems(30);
end;

{ TCustomAdapter }

constructor TCustomAdapter.Create(const Parent: TListViewBase; const AStrings: TStringList);
begin
  inherited Create;
  FStrings := AStrings;
  FParent := Parent;
  FBitmaps := TDictionary<Integer, TBitmap>.Create;
  FUriRegex := TRegEx.Create('\[(http://.*)\]');
  FNameRegex := TRegEx.Create('\[([A-Za-z]+)\]');
  FIdRegex := TRegEx.Create('\[([0-9]+)\]');
  FRegexMonitor := TObject.Create;

  FRequests := TList<Integer>.Create;
  FCS := TCriticalSection.Create;
  FSem := TSemaphore.Create(nil, 0, 1000, '');
  CreateThreads;

  FStrings.OnChanging := StringListChanging;
  FStrings.OnChange := StringListChange;
end;

destructor TCustomAdapter.Destroy;
var
  Pair: TPair<Integer, TBitmap>;
  I: Integer;
begin
  DestroyThreads;
  FRequests.Free;
  for Pair in FBitmaps do
    Pair.Value.Free;
  for I := 0 to FStrings.Count - 1 do
    FStrings.Objects[I].Free;

  FBitmaps.Free;
  FRegexMonitor.Free;
  inherited;
end;

procedure TCustomAdapter.ButtonClicked(Sender: TObject);
begin
  if Assigned(FOnButtonClicked) then
    FOnButtonClicked(Sender);
end;

procedure TCustomAdapter.DestroyThreads;
var
  I: Integer;
begin
  FExitRequested := True;
  FSem.Release(ThreadPoolSize);
  for I := 0 to High(FThreads) do
  begin
    FThreads[I].WaitFor;
    FThreads[I].Free;
  end;
end;

procedure TCustomAdapter.DoCreateNewViews;
begin
end;

procedure TCustomAdapter.DoResetViews(const APurposes: TListItemPurposes);
var
  I: Integer;
begin
  for I := 0 to FStrings.Count - 1 do
    ResetView(TListItem(FStrings.Objects[I]));
  ItemsResize;
end;

procedure TCustomAdapter.DoResetView(const Item: TListItem);
var
  BitmapDrawable: TListItemImage;
  BackdropDrawable: TListItemImage;
  TextDrawable: TListItemText;
  TextButton: TListItemTextButton;
begin
  if Item.View.Count = 0 then
  begin
    BitmapDrawable := TListItemImage.Create(Item);
    BitmapDrawable.Name := 'bitmap';
    BitmapDrawable.OwnsBitmap := False;
    BitmapDrawable.Bitmap := nil;
    BitmapDrawable.Align := TListItemAlign.Trailing;
    BitmapDrawable.ScalingMode := TImageScalingMode.StretchWithAspect;

    BackdropDrawable := TListItemImage.Create(Item);
    BackdropDrawable.Name := 'backdrop';
    BackdropDrawable.OwnsBitmap := False;
    BackdropDrawable.Bitmap := FbackdropImage.Bitmap;
    BackdropDrawable.VertAlign := TListItemAlign.Trailing;
    BackdropDrawable.Align := TListItemAlign.Trailing;
    BackdropDrawable.ScalingMode := TImageScalingMode.Stretch;
    BackdropDrawable.Opacity := 0.25;
    BackdropDrawable.Height := 65;

    TextDrawable := TListItemText.Create(Item);
    TextDrawable.Name := 'title';
    TextDrawable.Text := GetName(Item.Index).ToUpper;
    TextDrawable.Height := 80;
    TextDrawable.Font.Size := 48;
    TextDrawable.TextColor := TAlphaColorRec.Bisque;
    TextDrawable.SelectedTextColor := TAlphaColorRec.White;
    TextDrawable.PlaceOffset.X := 10;
    TextDrawable.PlaceOffset.Y := 10;

    TextDrawable := TListItemText.Create(Item);
    TextDrawable.Name := 'blurb';
    TextDrawable.Text := GetId(Item.Index);
    TextDrawable.Font.Size := 16;
    TextDrawable.TextColor := TAlphaColorRec.White;
    TextDrawable.SelectedTextColor := TAlphaColorRec.White;
    TextDrawable.Align := TListItemAlign.Leading;
    TextDrawable.VertAlign := TListItemAlign.Trailing;
    TextDrawable.WordWrap := True;
    TextDrawable.Height := 60;
    TextDrawable.PlaceOffset.X := 10;

    TextButton := TListItemTextButton.Create(Item);
    TextButton.Name := 'button';
    TextButton.Text := '。。。';
    TextButton.Align := TListItemAlign.Trailing;
    TextButton.VertAlign := TListItemAlign.Trailing;
    TextButton.Width := 48;
    TextButton.Height := 16;
    TextButton.PlaceOffset.Y := -TextDrawable.Height + TextDrawable.Height/2 - TextButton.Height/2;
    TextButton.PlaceOffset.X := -10;
    TextButton.OnSelect := ButtonClicked;
    TextButton.TagObject := Item; //Owner has been removed, using TagObject to store my parent.

    AddIndex(Item.Index);
  end
  else
    MatchView(Item);
end;

function TCustomAdapter.CreateStubItem: TListItem;
var
  TextDrawable: TListItemText;
begin
  Result := TListItem.Create(FParent.Adapter, FParent);
  Result.Height := GetDefaultViewHeight;
  Result.Index := 0;

  TextDrawable := TListItemText.Create(Result);
  TextDrawable.Text := 'Pull to load data';
end;

procedure TCustomAdapter.ImagesLoaded;
var
  Pair: TPair<Integer, TBitmap>;
  Item: TListItem;
  BitmapDrawable: TListItemImage;
  TextDrawable: TListItemText;
begin
  for Pair in FBitmaps do
  begin
    Item := TListItem(FStrings.Objects[Pair.Key]);
    BitmapDrawable := TListItemImage(Item.View.FindDrawable('bitmap'));
    if (BitmapDrawable <> nil) and (BitmapDrawable.Bitmap = nil) then
      BitmapDrawable.Bitmap := Pair.Value;
    TextDrawable := TListItemText(Item.View.FindDrawable('blurb'));
    if TextDrawable <> nil then
    begin
      TextDrawable.Text :=
        Format('%s is %dx%d and has ordinal number %d', [GetName(Pair.Key), Pair.Value.Width, Pair.Value.Height, Pair.Key]);
    end;

    MatchView(Item);
  end;

  FParent.StopPullRefresh;

  ItemsResize;
  ItemsInvalidate;
end;

procedure TCustomAdapter.MatchView(const Item: TListItem);
var
  BitmapDrawable: TListItemImage;
  BackdropDrawable: TListItemImage;
  TextDrawable: TListItemText;
  Aspect: Single;
  Width: Single;
begin
  BitmapDrawable := TListItemImage(Item.View.FindDrawable('bitmap'));
  if (BitmapDrawable <> nil) and (BitmapDrawable.Bitmap <> nil) then
  begin
    Width := FParent.Width - FParent.ItemSpaces.Left - FParent.ItemSpaces.Right;
    BitmapDrawable.Width := Width;
    Aspect := Width / BitmapDrawable.Bitmap.Width;
    Item.Height := Round(BitmapDrawable.Bitmap.Height * Aspect + 0.5);

    TextDrawable := TListItemText(Item.View.FindDrawable('blurb'));
    TextDrawable.Width := Width - 100;

    BackdropDrawable := TListItemImage(Item.View.FindDrawable('backdrop'));
    BackdropDrawable.Width := Width;
    BackdropDrawable.PlaceOffset.Y := -10;
  end;
end;

function TCustomAdapter.GetCount: Integer;
begin
  Result := FStrings.Count
end;

function TCustomAdapter.GetDefaultViewHeight: Integer;
begin
  Result := 33;
end;

function TCustomAdapter.GetEnumerator: TEnumerator<TListItem>;
begin
  Result := nil;
end;

function TCustomAdapter.GetItem(const Index: Integer): TListItem;
  function CreateItem(const Index: Integer): TListItem;
  begin
    Result := TListItem.Create(FParent.Adapter, FParent);
    Result.Height := GetDefaultViewHeight;
    Result.Index := Index;
  end;
begin
  Result := TListItem(FStrings.Objects[Index]);
  if Result = nil then
  begin
    Result := CreateItem(Index);
    FStrings.Objects[Index] := Result;
    ResetView(Result);
  end;
end;

function TCustomAdapter.GetId(const Index: Integer): string;
var
  M: TMatch;
begin
  Result := string.Empty;
  M := FIdRegex.Match(FStrings[Index]);
  if M.Success then
    Result := M.Groups[1].Value;
end;

function TCustomAdapter.GetName(const Index: Integer): string;
var
  M: TMatch;
begin
  Result := string.Empty;
  M := FNameRegex.Match(FStrings[Index]);
  if M.Success then
    Result := M.Groups[1].Value;
end;

function TCustomAdapter.GetUri(const Index: Integer): string;
var
  M: TMatch;
begin
  TMonitor.Enter(FRegexMonitor);
  Result := string.Empty;
  M := FUriRegex.Match(FStrings[Index]);
  if M.Success then
  begin
    Result := M.Groups[0].Value;
    Result := M.Groups[1].Value;
  end;
  TMonitor.Exit(FRegexMonitor);
end;

function TCustomAdapter.IndexOf(const AItem: TListItem): Integer;
begin
  Result := -1;
end;

procedure TCustomAdapter.StringListChanging(Sender: TObject);
begin
  ItemsMayChange;
end;

procedure TCustomAdapter.SetOnButtonClicked(const Value: TNotifyEvent);
begin
  FOnButtonClicked := Value;
end;

procedure TCustomAdapter.StringListChange(Sender: TObject);
begin
  ItemsCouldHaveChanged;
  ItemsResize;
  ItemsInvalidate;
end;

procedure TCustomAdapter.AddIndex(Index: Integer);
begin
  try
    FCS.Acquire;
    FRequests.Add(Index);
    FSem.Release; //Increase available
  finally
    FCS.Release
  end;
end;

function TCustomAdapter.NextIndex(): Integer; //wait for available, -1 if end requested
begin
  FSem.Acquire;
  if FExitRequested then
    exit(-1);
  try
    FCS.Acquire;
    Result := FIndex;
    Inc(FIndex);
  finally
    FCS.Release
  end;
end;

procedure TCustomAdapter.CreateThreads;
var
  I: Integer;
begin
  SetLength(FThreads, ThreadPoolSize);
  for I := 0 to Length(FThreads) - 1 do
  begin
    FThreads[I] := TThread.CreateAnonymousThread(procedure
      var
        Http: TIdHTTP;
        Stream: TBytesStream;
        Bitmap: TBitmap;
        Index: Integer;
        URI: string;
      begin
        Http:= TIdHTTP.Create(nil);
        Stream := TBytesStream.Create;
        Index := NextIndex;
        while Index <> -1 do
        begin
          URI := GetUri(Index);
          try
            Http.Get(URI, Stream);
            TThread.Synchronize(nil, procedure begin
              try
                Bitmap := TBitmap.CreateFromStream(Stream);
                if (Bitmap <> nil) and (Bitmap.Width > 0) and (Bitmap.Height > 0) then
                  FBitmaps.Add(Index, Bitmap);
              except
              end;
            end);
            if (Bitmap <> nil) and (Bitmap.Width > 0) and (Bitmap.Height > 0) then
              TThread.Queue(nil, procedure begin
                ImagesLoaded;
              end);
            //WriteLn(Format('Fetching item %d finished', [Index]));
          except
            TThread.Queue(nil, procedure begin
              TListItemText(TListItem(FStrings.Objects[Index]).View.FindDrawable('blurb')).Text := 'Load Error, retrying...';
              ItemsInvalidate;
            end);
            AddIndex(Index); // push back so it's picked up later
            TThread.CurrentThread.Sleep(Random(100) + 100);
          end;
          Index := NextIndex;
        end;
        Http.Free;
        Stream.Free;
      end);
    FThreads[I].FreeOnTerminate := False;
    FThreads[I].Start;
  end;
end;

function TCustomAdapter.GetIndexTitle(const Index: Integer): string;
begin
  Result := '';
end;

function TCustomAdapter.GetText(const Index: Integer): string;
begin
  Result := GetName(Index);
end;

function TCustomAdapter.GetTextButtonDrawable(const Index: Integer): TListItemTextButton;
begin
  Result := TListItemTextButton(TListItem(FStrings.Objects[Index]).View.FindDrawable('button'));
end;

end.
