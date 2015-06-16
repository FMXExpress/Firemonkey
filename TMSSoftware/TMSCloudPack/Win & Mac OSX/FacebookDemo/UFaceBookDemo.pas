unit UFaceBookDemo;

interface

uses
  FMX.Forms, SysUtils, FMX.TMSCloudBase, FMX.TMSCloudFacebook, FMX.Controls, FMX.Dialogs,
  FMX.TabControl, FMX.Grid, FMX.TMSCloudListView, FMX.Edit, FMX.StdCtrls,
  FMX.Objects, FMX.TMSCloudImage, FMX.Layouts, FMX.ListBox, System.Classes,
  FMX.Types, FMX.TMSCloudBaseFMX, FMX.TMSCloudCustomFacebook, System.Rtti,
  FMX.Controls.Presentation, IOUtils;

type

  TForm1 = class(TForm)
StyleBook1: TStyleBook;

    TMSFMXCloudFacebook1: TTMSFMXCloudFacebook;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Button1: TButton;
    GroupBox2: TGroupBox;
    ListBox1: TListBox;
    SpeedButton1: TSpeedButton;
    GroupBox3: TGroupBox;
    Edit2: TEdit;
    Edit1: TEdit;
    Label3: TLabel;
    Label1: TLabel;
    Edit3: TEdit;
    Button3: TButton;
    Button2: TButton;
    Label2: TLabel;
    SpeedButton2: TSpeedButton;
    GroupBox1: TGroupBox;
    TMSFMXCloudImage1: TTMSFMXCloudImage;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label4: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    lbGender: TLabel;
    lbWebsite: TLabel;
    lbUpdated: TLabel;
    Label11: TLabel;
    lbName: TLabel;
    CloudListView1: TTMSFMXCloudListView;
    Button4: TButton;
    Image1: TImage;
    btRemove: TButton;
    PageControl1: TTabControl;
    Likes: TTabItem;
    Comments: TTabItem;
    Label13: TLabel;
    edComment: TEdit;
    btPostComment: TButton;
    btShowLikes: TButton;
    lvLikes: TTMSFMXCloudListView;
    lvComments: TTMSFMXCloudListView;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure ToggleControls;
    procedure LoadFeed;
    procedure LoadComments;
    procedure LoadLikes;
    procedure ShowProfile(Profile: TFacebookProfile);
    procedure TMSFMXCloudFacebook1ReceivedAccessToken(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure btRemoveClick(Sender: TObject);
    procedure btPostCommentClick(Sender: TObject);
    procedure btShowLikesClick(Sender: TObject);
    procedure CloudListView1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Connected: boolean;
    InitLV: boolean;
    InitLVC: boolean;
    FeedOffset: integer;
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
//  FacebookAppkey = 'xxxxxxxxx';
//  FacebookAppSecret = 'yyyyyyyy';

{$I APPIDS.INC}

procedure TForm1.btPostCommentClick(Sender: TObject);
var
  FeedItem: TFacebookFeedItem;
begin
  if (CloudListView1.ItemIndex >= 0) and (edComment.Text <> '') then
  begin
    FeedItem := CloudListView1.Items[CloudListView1.ItemIndex].Data;

    TMSFMXCloudFacebook1.PostComment(FeedItem.ID, edComment.Text);
    LoadFeed;
  end
  else
  begin
    ShowMessage('Please make sure a feed item is selected and a Comment text has been entered.');
  end;
end;

procedure TForm1.btRemoveClick(Sender: TObject);
//var
//  dr: string;
begin
  TMSFMXCloudFacebook1.Logout;
  TMSFMXCloudFacebook1.ClearTokens;
  Connected := false;
  ToggleControls;
//  dr := ExtractFileDir(ParamStr(0)) + '\cache';
//  if TDirectory.Exists(dr) then
//    TDirectory.Delete(dr, True);
end;

procedure TForm1.btShowLikesClick(Sender: TObject);
var
  i, j, k: integer;
  strlikes: string;
begin
  if InitLVC then
    Exit;

  if (CloudListView1.ItemIndex >= 0) and (lvComments.ItemIndex >= 0) then
  begin
    i := CloudListView1.ItemIndex;
    j := lvComments.ItemIndex;

    TMSFMXCloudFacebook1.GetLikes(TMSFMXCloudFacebook1.Profile.Feed[i].Comments[j]);

    if TMSFMXCloudFacebook1.Profile.Feed[i].Comments[j].Likes.Count > 0 then
    begin
      strlikes := 'Likes:' + #13;
      for k := 0 to TMSFMXCloudFacebook1.Profile.Feed[i].Comments[j].Likes.Count - 1 do
        strlikes := strlikes + TMSFMXCloudFacebook1.Profile.Feed[i].Comments[j].Likes[k].FullName + #13;
    end
    else
      strlikes := 'No Likes found for this Comment.';

    ShowMessage(strlikes);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  acc: boolean;
begin
  TMSFMXCloudFacebook1.App.Key := FacebookAppkey;
  TMSFMXCloudFacebook1.App.Secret := FacebookAppSecret;

  if TMSFMXCloudFacebook1.App.Key <> '' then
  begin
    TMSFMXCloudFacebook1.PersistTokens.Key := ExtractFilePath(ParamStr(0)) + 'facebook.ini';
    TMSFMXCloudFacebook1.PersistTokens.Section := 'tokens';
    TMSFMXCloudFacebook1.LoadTokens;

    acc := TMSFMXCloudFacebook1.TestTokens;

    if not acc then
    begin
      TMSFMXCloudFacebook1.RefreshAccess;
      acc := TMSFMXCloudFacebook1.TestTokens;
      if not acc then
        TMSFMXCloudFacebook1.DoAuth;
    end
    else
    begin
      Connected := true;
      ToggleControls;
      TMSFMXCloudFacebook1.GetUserInfo;
    end;
  end
  else
    ShowMessage('Please provide a valid application ID for the client component');
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  ImageID: string;
begin

  if OpenDialog1.Execute then
  begin
     ImageID := TMSFMXCloudFacebook1.PostImage(Edit2.Text, OpenDialog1.FileName);
     Edit3.Text := TMSFMXCloudFacebook1.GetImageURL(ImageID);
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  TMSFMXCloudFacebook1.Post(Edit2.Text, Edit1.Text, Edit3.Text);
  LoadFeed;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  FeedOffset := FeedOffset + 10;
  LoadFeed;
end;

procedure TForm1.TMSFMXCloudFacebook1ReceivedAccessToken(Sender: TObject);
begin
  TMSFMXCloudFacebook1.SaveTokens;
  Connected := true;
  ToggleControls;
  TMSFMXCloudFacebook1.GetUserInfo;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Connected := false;
  FeedOffset := 0;
  ToggleControls;
end;

procedure TForm1.ShowProfile(Profile: TFacebookProfile);
begin
  lbName.Text  := Profile.FirstName + ' ' + Profile.LastName;
  Label5.Text  := Profile.BirthDay;
  Label6.Text  := Profile.Location.Name;
  if Profile.Gender = fbMale then
    lbGender.Text  := 'Male'
  else
    lbGender.Text  := 'Female';
  lbWebsite.Text  := Profile.Website;
  lbUpdated.Text  := DateTimeToStr(Profile.UpdatedTime);
  TMSFMXCloudImage1.URL := Profile.ImageURL;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
var
  I: Integer;
begin
  TMSFMXCloudFacebook1.GetFriends;
  for I := 0 to TMSFMXCloudFacebook1.FriendList.Count - 1 do
    ListBox1.Items.AddObject(TMSFMXCloudFacebook1.FriendList.Items[I].FullName, TMSFMXCloudFacebook1.FriendList.Items[I]);
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
begin
  FeedOffset := 0;
  LoadFeed;
end;

procedure TForm1.ListBox1Click(Sender: TObject);
var
  Profile: TFacebookProfile;
begin
  Profile := ListBox1.Items.Objects[ListBox1.ItemIndex] as TFacebookProfile;
  Profile := TMSFMXCloudFacebook1.FriendList.Find(Profile.ID);

  TMSFMXCloudFacebook1.GetProfileInfo(Profile.ID, Profile);
  ShowProfile(Profile);
end;

procedure TForm1.LoadComments;
var
  cm: TFacebookComment;
  li: TListItem;
  i, j: integer;
begin
  if CloudListView1.ItemIndex >= 0 then
  begin
    InitLV := true;
    lvComments.Items.Clear;
    i := CloudListView1.ItemIndex;

    TMSFMXCloudFacebook1.GetComments(TMSFMXCloudFacebook1.Profile.Feed[i]);

    for j := 0 to TMSFMXCloudFacebook1.Profile.Feed[i].Comments.Count - 1 do
    begin
      li := lvComments.Items.Add;
      cm := TMSFMXCloudFacebook1.Profile.Feed[i].Comments[j];

      li.Text  := IntToStr(j + 1);
      li.SubItems.Add(cm.Text);
      li.SubItems.Add(cm.User.FullName);
      li.SubItems.Add(FormatDateTime('dd/mm/yyyy hh:nn',cm.CreatedTime));
      li.Data := cm;
    end;
    InitLV := false;
  end;
end;

procedure TForm1.LoadLikes;
var
  li: TListItem;
  i, j: integer;
begin
  if CloudListView1.ItemIndex >= 0 then
  begin
    lvLikes.Items.Clear;
    i := CloudListView1.ItemIndex;

    for j := 0 to TMSFMXCloudFacebook1.Profile.Feed[i].Likes.Count - 1 do
    begin
      li := lvLikes.Items.Add;
      li.Text  := TMSFMXCloudFacebook1.Profile.Feed[i].Likes[j].FullName;
      li.Data := TMSFMXCloudFacebook1.Profile.Feed[i].Likes[j];
    end;
  end;
end;

procedure TForm1.CloudListView1Change(Sender: TObject);
begin
  if InitLV then
    Exit;
  LoadComments;
  LoadLikes;
end;

procedure TForm1.LoadFeed;
var
  I: integer;
  Profile: TFacebookProfile;
  li: TListItem;

begin
  if FeedOffset = 0 then
    CloudListView1.Items.Clear;

  Profile := TMSFMXCloudFacebook1.Profile;

  TMSFMXCloudFacebook1.GetFeed(Profile,10,FeedOffset);
//  TMSFMXCloudFacebook1.GetFeed(Profile,100);

  InitLV := true;

  for I := FeedOffset to profile.Feed.Count - 1 do
  begin
//    TMSFMXCloudFacebook1.GetLikes(Profile.Feed[i]);

    li := CloudListView1.Items.Add;
    li.Text  := IntToStr(i + 1);

//    if (Profile.Feed[i].Likes.Find(Profile.ID) <> nil) then
//        li.IsChecked  := true;

    li.SubItems.Add(profile.Feed[i].User.FullName);
    if profile.Feed[i].Text <> '' then
      li.SubItems.Add(profile.Feed[i].Text)
    else
      li.SubItems.Add(profile.Feed[i].Story);
    li.SubItems.Add(FormatDateTime('dd/mm/yyyy hh:nn',profile.Feed[i].CreatedTime));
    li.Data := profile.Feed[i];
  end;
  InitLV := false;
end;

procedure TForm1.ToggleControls;
begin
  GroupBox1.Enabled := Connected;
  GroupBox3.Enabled := Connected;
  SpeedButton1.Enabled := Connected;
  SpeedButton2.Enabled := Connected;
  ListBox1.Enabled := Connected;
  Button2.Enabled := Connected;
  Button3.Enabled := Connected;
  Edit1.Enabled := Connected;
  Edit2.Enabled := Connected;
  Edit3.Enabled := Connected;
  btRemove.Enabled := Connected;
  lvLikes.Enabled := Connected;
  PageControl1.Enabled := Connected;
  Button1.Enabled := not Connected;
end;


end.
