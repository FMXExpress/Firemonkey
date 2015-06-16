unit UTwitterDemo;

interface

uses
  FMX.Forms,
  FMX.TMSCloudURLShortener, FMX.TMSCloudBase, FMX.TMSCloudTwitter,
  FMX.Controls, FMX.Dialogs, FMX.Objects, FMX.StdCtrls, FMX.Edit, FMX.Grid,
  FMX.TMSCloudListView, FMX.Memo, FMX.TMSCloudImage, FMX.Layouts, FMX.ListBox,
  FMX.TabControl, System.Classes, FMX.Types, SysUtils, FMX.TMSCloudBaseFMX,
  FMX.TMSCloudCustomTwitter;

type
  TForm1 = class(TForm)
StyleBook1: TStyleBook;

    GroupBox1: TGroupBox;
    PageControl1: TTabControl;
    TabSheet1: TTabItem;
    TabSheet2: TTabItem;
    GroupBox2: TGroupBox;
    ListBox1: TListBox;
    ListBox2: TListBox;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    TMSFMXCloudCloudImage1: TTMSFMXCloudImage;
    GroupBox3: TGroupBox;
    Label2: TLabel;
    Memo1: TMemo;
    Button3: TButton;
    Button4: TButton;
    Button2: TButton;
    Button5: TButton;
    Panel1: TPanel;
    Button1: TButton;
    SpeedButton3: TSpeedButton;
    Label1: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    OpenDialog1: TOpenDialog;
    TMSFMXCloudTwitter1: TTMSFMXCloudTwitter;
    CloudListView1: TTMSFMXCloudListView;
    Label4: TLabel;
    edURL: TEdit;
    btURL: TButton;
    Label10: TLabel;
    TweetSizeLbl: TLabel;
    Button7: TButton;
    TMSFMXCloudURLShortener1: TTMSFMXCloudURLShortener;
    btRemove: TButton;
    Image1: TImage;
    TMSFMXCloudCloudImage2: TTMSFMXCloudImage;
    Label11: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure TMSFMXCloudTwitter1ReceivedAccessToken(Sender: TObject);
    procedure FillFollowers;
    procedure FillFriends;
    procedure LoadTweets;
    procedure Button2Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ToggleControls;
    procedure ProfileDetails(Profile: TTWitterProfile);
    procedure ListBox1Click(Sender: TObject);
    procedure ListBox2Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure btURLClick(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure btRemoveClick(Sender: TObject);
    procedure CloudListView1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Connected: boolean;
    procedure UpdateTweetSize;
  end;

var
  Form1: TForm1;

implementation

{$R *.FMX}

// PLEASE USE A VALID INCLUDE FILE THAT CONTAINS THE APPLICATION KEY & SECRET
// FOR THE CLOUD STORAGE SERVICES YOU WANT TO USE
// STRUCTURE OF THIS .INC FILE SHOULD BE
//
// const
//  TwitterAppkey = 'xxxxxxxxx';
//  TwitterAppSecret = 'yyyyyyyy';

{$I APPIDS.INC}

procedure TForm1.TMSFMXCloudTwitter1ReceivedAccessToken(Sender: TObject);
begin
  TMSFMXCloudTwitter1.SaveTokens;
  TMSFMXCloudTwitter1.GetAccountInfo;
  Connected := true;
  ToggleControls;
end;

procedure TForm1.FillFollowers;
var
  i: integer;
begin
  TMSFMXCloudTwitter1.GetFollowers;
  TMSFMXCloudTwitter1.GetProfileListInfo(TMSFMXCloudTwitter1.Followers);

  ListBox1.Items.Clear;
  for i := 0 to TMSFMXCloudTwitter1.Followers.Count - 1 do
  begin
    listbox1.Items.AddObject(TMSFMXCloudTwitter1.Followers.Items[i].Name, TMSFMXCloudTwitter1.Followers.Items[i]);
  end;
end;

procedure TForm1.FillFriends;
var
  i: integer;
begin
  TMSFMXCloudTwitter1.GetFriends;
  TMSFMXCloudTwitter1.GetProfileListInfo(TMSFMXCloudTwitter1.Friends);

  ListBox2.Items.Clear;
  for i := 0 to TMSFMXCloudTwitter1.Friends.Count - 1 do
  begin
    listbox2.Items.AddObject(TMSFMXCloudTwitter1.Friends.Items[i].Name, TMSFMXCloudTwitter1.Friends.Items[i]);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Connected := false;
  ToggleControls;
  Button5.Enabled := false;
  Button2.Enabled := false;
  UpdateTweetSize;
  CloudListView1.ColumnByIndex(1).Width := 500;
end;

procedure TForm1.ListBox1Click(Sender: TObject);
begin
  ListBox2.ItemIndex := -1;
  ProfileDetails(ListBox1.Items.Objects[ListBox1.ItemIndex] as TTWitterProfile);
end;

procedure TForm1.ListBox2Click(Sender: TObject);
begin
  ListBox1.ItemIndex := -1;
  ProfileDetails(ListBox2.Items.Objects[ListBox2.ItemIndex] as TTWitterProfile);
end;

procedure TForm1.LoadTweets;
var
  i: integer;
  li: TListItem;
  TwitterStatus: TTWitterStatus;
begin
  if CloudListView1.Items.Count = 0 then
    TMSFMXCloudTwitter1.GetStatuses(10)
  else
  begin
    TWitterStatus := CloudListView1.Items[CloudListView1.Items.Count - 1].Data;
    TMSFMXCloudTwitter1.GetStatuses(10, -1, TwitterStatus.ID);
  end;

  for i := CloudListView1.Items.Count to TMSFMXCloudTwitter1.Statuses.Count - 1 do
  begin
    li := CloudListView1.Items.Add;
    li.Text  := TMSFMXCloudTwitter1.Statuses.Items[i].User.Name;
    li.SubItems.Add(TMSFMXCloudTwitter1.Statuses.Items[i].Text);
    li.SubItems.Add(FormatDateTime('dd/mm/yyyy hh:nn',TMSFMXCloudTwitter1.Statuses.Items[i].CreatedAt));
    li.Data := TMSFMXCloudTwitter1.Statuses.Items[i];
  end;

  Button5.Enabled := false;
end;

procedure TForm1.Memo1Change(Sender: TObject);
begin
  UpdateTweetSize;
end;

procedure TForm1.ProfileDetails(Profile: TTWitterProfile);
begin
  TMSFMXCloudCloudImage1.URL := Profile.ImageURL;
  Label1.Text  := IntToStr(Profile.FriendsCount);
  Label3.Text  := IntToStr(Profile.FollowersCount);
  Label4.Text  := Profile.Description;
  Label5.Text  := Profile.Location;
  Button2.Text  := 'Message to "' + Profile.Name + '"';
  Button2.Enabled := true;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  FillFollowers;
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
begin
  FillFriends;
end;

procedure TForm1.SpeedButton3Click(Sender: TObject);
begin
  CloudListView1.Items.Clear;
  LoadTweets;
  Button7.Enabled := true;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  acc: boolean;
begin
  TMSFMXCloudTwitter1.App.Key := TwitterAppkey;
  TMSFMXCloudTwitter1.App.Secret := TwitterAppSecret;

  if TMSFMXCloudTwitter1.App.Key <> '' then
  begin
    TMSFMXCloudTwitter1.PersistTokens.Key := ExtractFilePath(ParamStr(0)) + 'twitter.ini';
    TMSFMXCloudTwitter1.PersistTokens.Section := 'tokens';
    TMSFMXCloudTwitter1.LoadTokens;

    acc := TMSFMXCloudTwitter1.TestTokens;

    if not acc then
    begin
      TMSFMXCloudTwitter1.RefreshAccess;
      TMSFMXCloudTwitter1.DoAuth;
      TMSFMXCloudTwitter1.GetAccountInfo;
    end
    else
    begin
      connected := true;
      ToggleControls;
    end;
  end
  else
    ShowMessage('Please provide a valid application ID for the client component');
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if ListBox1.ItemIndex > 0 then
    TMSFMXCloudTwitter1.DirectMessage(Memo1.Text, IntToStr((ListBox1.Items.Objects[ListBox1.ItemIndex] as TTWitterProfile).ID))
  else if ListBox2.ItemIndex > 0 then
    TMSFMXCloudTwitter1.DirectMessage(Memo1.Text, IntToStr((ListBox2.Items.Objects[ListBox2.ItemIndex] as TTWitterProfile).ID));
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  s:string;
begin
  s := Memo1.Lines.Text;
  if length(s) > 140 then
  begin
    ShowMessage('Length of message exceeds Twitter limit of 140 chars');
  end
  else
    TMSFMXCloudTwitter1.Tweet(s);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    TMSFMXCloudTwitter1.TweetWithMedia(Memo1.Lines.Text, opendialog1.FileName);
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  Status: TTWitterStatus;
begin
  if CloudListView1.ItemIndex > 0 then
  begin
    Status := CloudListView1.Items[CloudListView1.ItemIndex].Data;
    TMSFMXCloudTwitter1.ReTweet(Status.ID);
  end;
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  LoadTweets;
end;

procedure TForm1.CloudListView1Change(Sender: TObject);
var
  TwitterStatus: TTWitterStatus;
begin
  if CloudListView1.ItemIndex >= 0 then
  begin
    Button5.Enabled := true;
    TwitterStatus := CloudListView1.Items[CloudListView1.ItemIndex].Data;
    TMSFMXCloudCloudImage2.URL := TwitterStatus.MediaURL;
  end;
end;

procedure TForm1.btRemoveClick(Sender: TObject);
begin
  TMSFMXCloudTwitter1.ClearTokens;
  Connected := false;
  ToggleControls;
end;

procedure TForm1.btURLClick(Sender: TObject);
begin
  Memo1.Lines.Add(TMSFMXCloudURLShortener1.ShortenURL(edURL.Text));
end;

procedure TForm1.ToggleControls;
begin
  GroupBox1.Enabled := Connected;
  GroupBox3.Enabled := Connected;
  SpeedButton1.Enabled := Connected;
  SpeedButton2.Enabled := Connected;
  SpeedButton3.Enabled := Connected;
  Button3.Enabled := Connected;
  Button4.Enabled := Connected;
  Button7.Enabled := Connected;
  Memo1.Enabled := Connected;
  edURL.Enabled := Connected;
  btURL.Enabled := Connected;
  btRemove.Enabled := Connected;
  Button1.Enabled := not Connected;
end;

procedure TForm1.UpdateTweetSize;
begin
  TweetSizeLbl.Text  := inttostr(length(memo1.Lines.Text))+' of 140 chars';
end;

end.
