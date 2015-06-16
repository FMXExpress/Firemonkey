unit UInstagramDemo;

interface

uses
  FMX.TMSCloudBase, FMX.TMSCloudInstagram, FMX.Controls, FMX.Dialogs,
  FMX.ListBox, FMX.Edit, FMX.Grid, FMX.TMSCloudListView, FMX.StdCtrls,
  FMX.Objects, FMX.TMSCloudImage, FMX.TabControl, FMX.Layouts, System.Classes,
  FMX.Types, FMX.Forms, SysUtils, FMX.TMSCloudBaseFMX,
  FMX.TMSCloudCustomInstagram;

type
  TForm1 = class(TForm)
StyleBook1: TStyleBook;

    SaveDialog1: TSaveDialog;
    Panel1: TPanel;
    btConnect: TButton;
    gbFollowing: TGroupBox;
    lbFollowing: TListBox;
    gbMedia: TGroupBox;
    lbMedia: TListBox;
    Label8: TLabel;
    imgMediaPreview: TTMSFMXCloudImage;
    btGetFollowing: TButton;
    btGetUser: TButton;
    PageControl1: TTabControl;
    TabSheet1: TTabItem;
    TabSheet2: TTabItem;
    Label1: TLabel;
    lbLocation: TLabel;
    lbFilter: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    lbCreated: TLabel;
    lbLink: TLabel;
    Label9: TLabel;
    lvComments: TTMSFMXCloudListView;
    lbLikes: TListBox;
    lbLikesCount: TLabel;
    chkLike: TCheckBox;
    edComment: TEdit;
    btPost: TButton;
    lbTags: TListBox;
    Label10: TLabel;
    btFollowers: TButton;
    btDelete: TButton;
    gbSearch: TGroupBox;
    edSearchUsers: TEdit;
    btSearch: TButton;
    cbSearch: TComboBox;
    Label11: TLabel;
    lbMediaProfile: TLabel;
    btMediaProfileDetails: TButton;
    btMediaLocation: TButton;
    PageControl2: TTabControl;
    TabSheet3: TTabItem;
    imgUser: TTMSFMXCloudImage;
    lbInfo: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    Label4: TLabel;
    lbBio: TLabel;
    lbWebsite: TLabel;
    lbUsername: TLabel;
    btFollow: TButton;
    btUnfollow: TButton;
    btMediaNext: TButton;
    Image1: TImage;
    btRemove: TButton;
    btGetFeed: TButton;
    btDownload: TButton;
    TTMSFMXCloudInstagram1: TTMSFMXCloudInstagram;
    procedure btConnectClick(Sender: TObject);
    procedure btGetUserClick(Sender: TObject);
    procedure btGetFollowingClick(Sender: TObject);
    procedure lbMediaClick(Sender: TObject);
    procedure lbFollowingClick(Sender: TObject);
    procedure btDownloadClick(Sender: TObject);
    procedure chkLikeClick(Sender: TObject);
    procedure btPostClick(Sender: TObject);
    procedure btSearchClick(Sender: TObject);
    procedure btFollowClick(Sender: TObject);
    procedure btUnfollowClick(Sender: TObject);
    procedure btFollowersClick(Sender: TObject);
    procedure btDeleteClick(Sender: TObject);
    procedure lbUsernameClick(Sender: TObject);
    procedure lbLinkClick(Sender: TObject);
    procedure lbWebsiteClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btMediaProfileDetailsClick(Sender: TObject);
    procedure btMediaLocationClick(Sender: TObject);
    procedure btMediaNextClick(Sender: TObject);
    procedure btRemoveClick(Sender: TObject);
    procedure btGetFeedClick(Sender: TObject);
    procedure TTMSFMXCloudInstagram1ReceivedAccessToken(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Connected: boolean;
    CurrentUserID: string;
    NextMaxTagID: string;
    PagingFunction: string;
    procedure ToggleControls;
    procedure GetPhotoInfo;
    procedure SetPagingFunction(FunctionName: string);
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
//  InstagramAppkey = 'xxxxxxxxx';
//  InstagramAppSecret = 'yyyyyyyy';

{$I APPIDS.INC}

procedure TForm1.btConnectClick(Sender: TObject);
var
  acc: boolean;
begin
  TTMSFMXCloudInstagram1.App.Key := InstagramAppKey;
  TTMSFMXCloudInstagram1.App.Secret := InstagramAppSecret;


  if TTMSFMXCloudInstagram1.App.Key <> '' then
  begin
    TTMSFMXCloudInstagram1.PersistTokens.Key := ExtractFilePath(ParamStr(0)) + 'instagram.ini';
    TTMSFMXCloudInstagram1.PersistTokens.Section := 'tokens';
    TTMSFMXCloudInstagram1.LoadTokens;

    acc := TTMSFMXCloudInstagram1.TestTokens;

    if not acc then
    begin
      TTMSFMXCloudInstagram1.RefreshAccess;
      acc := TTMSFMXCloudInstagram1.TestTokens;
      if not acc then
        TTMSFMXCloudInstagram1.DoAuth;
    end
    else
    begin
      Connected := true;
      ToggleControls;
    end;
  end
  else
    ShowMessage('Please provide a valid application ID for the client component');
end;

procedure TForm1.btGetUserClick(Sender: TObject);
var
  I: Integer;
begin
  TTMSFMXCloudInstagram1.Users.Clear;
  lbFollowing.Items.Clear;
  TTMSFMXCloudInstagram1.GetProfile('', TTMSFMXCloudInstagram1.Profile);
  lbFollowing.Items.Add(TTMSFMXCloudInstagram1.Profile.FullName);

  TTMSFMXCloudInstagram1.GetMediaByUser('', 0, '', '');
  lbMedia.Items.Clear;
  for I := 0 to TTMSFMXCloudInstagram1.Photos.Count - 1 do
  begin
    if TTMSFMXCloudInstagram1.Photos[I].Caption  <> '' then
      lbMedia.Items.Add(TTMSFMXCloudInstagram1.Photos[I].Caption)
    else
      lbMedia.Items.Add('Photo ' + IntToStr(I + 1));
  end;
end;

procedure TForm1.btMediaLocationClick(Sender: TObject);
var
  I: integer;
  loc: TInstagramLocation;
begin
  if lbmedia.ItemIndex >= 0 then
  begin
    loc := TTMSFMXCloudInstagram1.Photos[lbMedia.ItemIndex].Location;

    TTMSFMXCloudInstagram1.Photos.Clear;
    TTMSFMXCloudInstagram1.GetMediaByLocation(loc.Latitude, loc.Longitude);

    lbMedia.Items.Clear;
    for I := 0 to TTMSFMXCloudInstagram1.Photos.Count - 1 do
      lbMedia.Items.Add(TTMSFMXCloudInstagram1.Photos[I].Caption);
  end;
end;

procedure TForm1.btMediaNextClick(Sender: TObject);
var
  I: integer;
  User: TInstagramUser;
  Tag: TInstagramTag;
begin
  if lbMedia.Items.Count > 0 then
  begin
    if PagingFunction = 'user' then
    begin
      User := TTMSFMXCloudInstagram1.Users[lbFollowing.ItemIndex].User;
      btMediaNext.Enabled := TTMSFMXCloudInstagram1.GetMediaByUser(User.ID,20,TTMSFMXCloudInstagram1.Photos[TTMSFMXCloudInstagram1.Photos.Count - 1].ID,'');
    end
    else if PagingFunction = 'tag' then
    begin
      Tag := TTMSFMXCloudInstagram1.Tags[lbFollowing.ItemIndex];
      NextMaxTagID := TTMSFMXCloudInstagram1.GetMediaByTag(Tag.Summary,NextMaxTagID, '');

      if NextMaxTagID = EmptyStr then
        btMediaNext.Enabled := false
      else
        btMediaNext.Enabled := true;
    end
    else if PagingFunction = 'feed' then
    begin
      btMediaNext.Enabled := TTMSFMXCloudInstagram1.GetFeed(20,TTMSFMXCloudInstagram1.Photos[TTMSFMXCloudInstagram1.Photos.Count - 1].ID,'');
    end;

    lbMedia.Items.Clear;
    for I := 0 to TTMSFMXCloudInstagram1.Photos.Count - 1 do
    begin
      if TTMSFMXCloudInstagram1.Photos[I].Caption  <> '' then
        lbMedia.Items.Add(TTMSFMXCloudInstagram1.Photos[I].Caption)
      else
        lbMedia.Items.Add('Photo ' + IntToStr(I + 1));
    end;

  end;
end;

procedure TForm1.btMediaProfileDetailsClick(Sender: TObject);
var
  User: TInstagramUser;
begin
  if lbMedia.ItemIndex >= 0 then
  begin
    TTMSFMXCloudInstagram1.GetProfile(TTMSFMXCloudInstagram1.Photos[lbMedia.ItemIndex].From.ID, TTMSFMXCloudInstagram1.Photos[lbMedia.ItemIndex].From);
    User := TTMSFMXCloudInstagram1.Photos[lbMedia.ItemIndex].From;

    CurrentUserID := User.ID;
    imgUser.URL := User.ProfilePicture;
    lbUsername.Text  := User.UserName;
    lbBio.Text  := User.Bio;
    lbWebsite.Text  := User.Website;
    lbInfo.Text  := 'Photos: ' + IntToStr(User.Photos)
      + ' Followers: ' + IntToStr(User.Followers)
      + ' Following: ' + IntToStr(User.Following);
  end;
end;

procedure TForm1.btPostClick(Sender: TObject);
begin
  if (lbMedia.ItemIndex > -1) and (edComment.Text <> '') then
    TTMSFMXCloudInstagram1.PostComment(TTMSFMXCloudInstagram1.Photos[lbMedia.ItemIndex].ID, edComment.Text)
end;

procedure TForm1.btRemoveClick(Sender: TObject);
begin
  TTMSFMXCloudInstagram1.ClearTokens;
  Connected := false;
  ToggleControls;
end;

procedure TForm1.btSearchClick(Sender: TObject);
var
  I: Integer;
begin
  if edSearchUsers.Text <> '' then
  begin
    TTMSFMXCloudInstagram1.Users.Clear;
    TTMSFMXCloudInstagram1.Tags.Clear;

    if cbSearch.ItemIndex = 0 then
    begin
      lbMedia.Items.Clear;
      imgMediaPreview.URL := '';
      lbLocation.Text  := '';
      lbFilter.Text  := '';
      lbLink.Text  := '';
      lbCreated.Text  := '';

      TTMSFMXCloudInstagram1.SearchUsers(edSearchUsers.Text);

      lbFollowing.Items.Clear;
      for I := 0 to TTMSFMXCloudInstagram1.Users.Count - 1 do
      begin
        lbFollowing.Items.Add(TTMSFMXCloudInstagram1.Users[I].User.UserName);
      end;
    end
    else
    begin
      lbMedia.Items.Clear;
      imgMediaPreview.URL := '';
      lbLocation.Text  := '';
      lbFilter.Text  := '';
      lbLink.Text  := '';
      lbCreated.Text  := '';

      TTMSFMXCloudInstagram1.SearchTags(edSearchUsers.Text);

      lbFollowing.Items.Clear;
      for I := 0 to TTMSFMXCloudInstagram1.Tags.Count - 1 do
      begin
        lbFollowing.Items.Add(TTMSFMXCloudInstagram1.Tags[I].Summary + ' (' + IntToStr(TTMSFMXCloudInstagram1.Tags[I].MediaCount) + ')');
      end;
    end;
  end;
end;

procedure TForm1.btUnfollowClick(Sender: TObject);
begin
  if lbFollowing.ItemIndex > -1 then
  begin
    TTMSFMXCloudInstagram1.UnFollow(TTMSFMXCloudInstagram1.Users[lbFollowing.ItemIndex].User.ID);
  end;
end;

procedure TForm1.chkLikeClick(Sender: TObject);
begin
  if lbMedia.ItemIndex > -1 then
  begin
    if chkLike.IsChecked  then
      TTMSFMXCloudInstagram1.Like(TTMSFMXCloudInstagram1.Photos[lbMedia.ItemIndex].ID)
    else
      TTMSFMXCloudInstagram1.UnLike(TTMSFMXCloudInstagram1.Photos[lbMedia.ItemIndex].ID);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Connected := false;
  ToggleControls;
  NextMaxTagID := EmptyStr;
  PagingFunction := EmptyStr;
  btMediaNext.Enabled := false;
  imgMediaPreview.URL := '';
  lbLocation.Text  := '';
  lbFilter.Text  := '';
  lbLink.Text  := '';
  lbCreated.Text  := '';
  lbinfo.Text  := '';
  lbBio.Text  := '';
  lbWebsite.Text  := '';
  lbUsername.Text  := '';
  lbInfo.Text  := '';
  lbMediaProfile.Text  := '';

end;

procedure TForm1.btGetFeedClick(Sender: TObject);
var
  I: integer;
begin
  btMediaNext.Enabled := TTMSFMXCloudInstagram1.GetFeed();
  SetPagingFunction('feed');

  if TTMSFMXCloudInstagram1.Photos.Count > 0 then
  begin
    lbMedia.Items.Clear;
    for I := 0 to TTMSFMXCloudInstagram1.Photos.Count - 1 do
    begin
      if TTMSFMXCloudInstagram1.Photos[I].Caption  <> '' then
        lbMedia.Items.Add(TTMSFMXCloudInstagram1.Photos[I].Caption)
      else
        lbMedia.Items.Add('Photo ' + IntToStr(I + 1));
    end;

    lbMedia.ItemIndex := 0;
    GetPhotoInfo;
  end;
end;

procedure TForm1.btGetFollowingClick(Sender: TObject);
var
  I: Integer;
begin
  lbMedia.Items.Clear;
  imgMediaPreview.URL := '';
  lbLocation.Text  := '';
  lbFilter.Text  := '';
  lbLink.Text  := '';
  lbCreated.Text  := '';

  TTMSFMXCloudInstagram1.GetFollowing;
  lbFollowing.Items.Clear;
  for I := 0 to TTMSFMXCloudInstagram1.Users.Count - 1 do
  begin
    lbFollowing.Items.Add(TTMSFMXCloudInstagram1.Users[I].User.FullName);
  end;
end;

procedure TForm1.btFollowersClick(Sender: TObject);
var
  I: Integer;
begin
  lbMedia.Items.Clear;
  imgMediaPreview.URL := '';
  lbLocation.Text  := '';
  lbFilter.Text  := '';
  lbLink.Text  := '';
  lbCreated.Text  := '';

  TTMSFMXCloudInstagram1.GetFollowers;
  lbFollowing.Items.Clear;
  for I := 0 to TTMSFMXCloudInstagram1.Users.Count - 1 do
  begin
    lbFollowing.Items.Add(TTMSFMXCloudInstagram1.Users[I].User.FullName);
  end;
end;

procedure TForm1.btDeleteClick(Sender: TObject);
begin
  if (lvComments.ItemIndex > -1) and (lbMedia.ItemIndex > -1) then
    TTMSFMXCloudInstagram1.DeleteComment(TTMSFMXCloudInstagram1.Photos[lbMedia.ItemIndex].ID, TTMSFMXCloudInstagram1.Photos[lbMedia.ItemIndex].Comments[lvComments.ItemIndex].ID)
end;

procedure TForm1.btDownloadClick(Sender: TObject);
begin
  if lbMedia.ItemIndex > -1 then
  begin
    SaveDialog1.FileName := TTMSFMXCloudInstagram1.Photos[lbMedia.ItemIndex].ID + '.jpg';
    if (SaveDialog1.Execute) then
    begin
      TTMSFMXCloudInstagram1.DownloadPhoto(SaveDialog1.FileName, TTMSFMXCloudInstagram1.Photos[lbMedia.ItemIndex].Images.StandardRes.URL);
    end;
  end;
end;

procedure TForm1.btFollowClick(Sender: TObject);
begin
  if lbFollowing.ItemIndex > -1 then
  begin
    TTMSFMXCloudInstagram1.Follow(TTMSFMXCloudInstagram1.Users[lbFollowing.ItemIndex].User.ID);
  end;
end;

procedure TForm1.lbMediaClick(Sender: TObject);
begin
  GetPhotoInfo;
end;

procedure TForm1.lbUsernameClick(Sender: TObject);
begin
  ShowMessage('http://www.instagram.com/' + lbUsername.Text);
end;

procedure TForm1.lbWebsiteClick(Sender: TObject);
begin
  ShowMessage(lbWebsite.Text);
end;

procedure TForm1.SetPagingFunction(FunctionName: string);
begin
  if btMediaNext.Enabled then
    PagingFunction := FunctionName
  else
    PagingFunction := EmptyStr;
end;

procedure TForm1.TTMSFMXCloudInstagram1ReceivedAccessToken(Sender: TObject);
begin
  TTMSFMXCloudInstagram1.SaveTokens;
  Connected := true;
  ToggleControls;
end;

procedure TForm1.ToggleControls;
begin
  btConnect.Enabled := not connected;
  btRemove.Enabled := connected;
  lbFollowing.Enabled := connected;
  lbMedia.Enabled := connected;
  btDownload.Enabled := connected;
  btSearch.Enabled := connected;
  lvComments.Enabled := connected;
  cbSearch.Enabled := connected;
  edSearchUsers.Enabled := connected;
  btGetUser.Enabled := connected;
  btGetFollowing.Enabled := connected;
  btDownload.Enabled := connected;
  btFollowers.Enabled := connected;
  btFollow.Enabled := connected;
  btUnfollow.Enabled := connected;
  btMediaProfileDetails.Enabled := connected;
  btMediaLocation.Enabled := connected;
  chkLike.Enabled := connected;
  PageControl1.Enabled := connected;
  PageControl2.Enabled := connected;
  btGetFeed.Enabled := connected;
end;

procedure TForm1.GetPhotoInfo;
var
  ph: TInstagramPhoto;
  I: Integer;
  li: TListItem;
begin
  if lbMedia.ItemIndex > -1 then
  begin
    ph := TTMSFMXCloudInstagram1.Photos[lbMedia.ItemIndex];
    imgMediaPreview.URL := ph.Images.Thumbnail.URL;
    lbLocation.Text  := ph.Location.Summary;
    lbFilter.Text  := ph.Filter;
    lbLink.Text  := ph.Link;
    lbCreated.Text  := DateTimeToStr(ph.CreatedTime);
    lbLikesCount.Text  := 'Likes: (' + IntToStr(ph.LikesCount) + ')';
    lbMediaProfile.Text  := ph.From.FullName;
    chkLike.IsChecked  := ph.UserLikes;

    lbTags.Items.Clear;
    for I := 0 to ph.Tags.Count - 1 do
      lbTags.Items.Add(ph.Tags[I]);

    lbLikes.Items.Clear;
    for I := 0 to ph.Likes.Count - 1 do
      lbLikes.Items.Add(ph.Likes[I].User.FullName);

    lvComments.Columns[0].Caption  := 'Text (' + IntToStr(ph.CommentsCount) + ')';
    lvComments.Items.Clear;
    for I := 0 to ph.Comments.Count - 1 do
    begin
      li := lvComments.Items.Add;
      li.Text  := ph.Comments[I].Text;
      li.SubItems.Add(ph.Comments[I].From.FullName);
      li.SubItems.Add(DateTimeToStr(ph.Comments[I].CreatedTime));
    end;
  end;
end;

procedure TForm1.lbFollowingClick(Sender: TObject);
var
  I: integer;
  User: TInstagramUser;
begin
  if lbFollowing.ItemIndex > -1 then
  begin

    if TTMSFMXCloudInstagram1.Tags.Count > 0 then
    begin
      NextMaxTagID := TTMSFMXCloudInstagram1.GetMediaByTag(TTMSFMXCloudInstagram1.Tags[lbFollowing.ItemIndex].Summary);

      if NextMaxTagID = EmptyStr then
        btMediaNext.Enabled := false
      else
        btMediaNext.Enabled := true;

      SetPagingFunction('tag');
    end
    else
    begin
      if TTMSFMXCloudInstagram1.Users.Count > 0 then
      begin
        User := TTMSFMXCloudInstagram1.Users[lbFollowing.ItemIndex].User;
        TTMSFMXCloudInstagram1.GetProfile(User.ID, User);
      end
      else
        User := TTMSFMXCloudInstagram1.Profile;

      CurrentUserID := User.ID;
      imgUser.URL := User.ProfilePicture;
      lbUsername.Text  := User.UserName;
      lbBio.Text  := User.Bio;
      lbWebsite.Text  := User.Website;
      lbInfo.Text  := 'Photos: ' + IntToStr(User.Photos)
        + ' Followers: ' + IntToStr(User.Followers)
        + ' Following: ' + IntToStr(User.Following);

      btMediaNext.Enabled := TTMSFMXCloudInstagram1.GetMediaByUser(User.ID, 20, '', '');

      SetPagingFunction('user');
    end;

    lbMedia.Items.Clear;
    for I := 0 to TTMSFMXCloudInstagram1.Photos.Count - 1 do
    begin
      if TTMSFMXCloudInstagram1.Photos[I].Caption  <> '' then
        lbMedia.Items.Add(TTMSFMXCloudInstagram1.Photos[I].Caption)
      else
        lbMedia.Items.Add('Photo ' + IntToStr(I + 1));
    end;

    if TTMSFMXCloudInstagram1.Photos.Count > 0 then
    begin
      lbMedia.ItemIndex := 0;
      GetPhotoInfo;
    end;

  end;
end;

procedure TForm1.lbLinkClick(Sender: TObject);
begin
  ShowMessage(lbLink.Text);
end;

end.
