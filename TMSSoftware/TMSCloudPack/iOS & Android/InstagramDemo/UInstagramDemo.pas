unit UInstagramDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TMSCloudBase,
  FMX.TMSCloudInstagram, FMX.StdCtrls, FMX.Objects,
  FMX.Layouts, FMX.ListBox, FMX.TMSCloudImage, FMX.ListView.Types, FMX.ListView,
  FMX.TabControl, FMX.Edit, IOUtils, FMX.TMSCloudBaseFMX,
  FMX.TMSCloudCustomInstagram;

type
  TForm1017 = class(TForm)
    TMSFMXCloudInstagram1: TTMSFMXCloudInstagram;
    ToolBar1: TToolBar;
    ConnectBtn: TButton;
    DisconnectBtn: TButton;
    Button1: TButton;
    TMSFMXCloudImage1: TTMSFMXCloudImage;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    ListView1: TListView;
    ListView2: TListView;
    Button2: TButton;
    Label1: TLabel;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    edSearchUsers: TEdit;
    Button3: TButton;
    ListView3: TListView;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    TMSFMXCloudImage2: TTMSFMXCloudImage;
    Button4: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ConnectBtnClick(Sender: TObject);
    procedure DisconnectBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TMSFMXCloudInstagram1ReceivedAccessToken(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ListView1Change(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ListView3Change(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
    Connected: Boolean;
    FCurrentUser: TInstagramUser;
    procedure GetUserProfile;
    procedure GetPhotoInfo;
    procedure GetUserInfo;
    procedure LoadFeed;
  public
    { Public declarations }
  end;

var
  Form1017: TForm1017;

implementation

{$R *.fmx}

// PLEASE USE A VALID INCLUDE FILE THAT CONTAINS THE APPLICATION KEY & SECRET
// FOR THE CLOUD STORAGE SERVICES YOU WANT TO USE
// STRUCTURE OF THIS .INC FILE SHOULD BE
//
// const
//  InstagramAppkey = 'xxxxxxxxx';
//  InstagramAppSecret = 'yyyyyyyy';

{$I APPIDS.INC}

procedure TForm1017.Button1Click(Sender: TObject);
begin
  if not Connected then
  begin
    ShowMessage('Please authenticate first');
    Exit;
  end;

  LoadFeed;
end;

procedure TForm1017.Button2Click(Sender: TObject);
begin
  if not Connected then
  begin
    ShowMessage('Please authenticate first');
    Exit;
  end;

  if ListView1.ItemIndex > -1 then
    ShowMessage(TMSFMXCloudInstagram1.Photos[ListView1.ItemIndex].Link);
end;

procedure TForm1017.Button3Click(Sender: TObject);
var
  I: Integer;
begin
  if not Connected then
  begin
    ShowMessage('Please authenticate first');
    Exit;
  end;

  if edSearchUsers.Text <> '' then
  begin
    TMSFMXCloudInstagram1.Users.Clear;
    TMSFMXCloudInstagram1.Tags.Clear;

    TMSFMXCloudInstagram1.SearchUsers(edSearchUsers.Text);

    ListView3.Items.Clear;
    for I := 0 to TMSFMXCloudInstagram1.Users.Count - 1 do
      ListView3.Items.Add.Text := TMSFMXCloudInstagram1.Users[I].User.UserName;
  end;
end;

procedure TForm1017.Button4Click(Sender: TObject);
var
  I: integer;
begin
  if ListView1.Items.Count > 0 then
  begin
    Button4.Enabled := TMSFMXCloudInstagram1.GetFeed(20,TMSFMXCloudInstagram1.Photos[TMSFMXCloudInstagram1.Photos.Count - 1].ID,'');

    ListView1.Items.Clear;
    for I := 0 to TMSFMXCloudInstagram1.Photos.Count - 1 do
    begin
      if TMSFMXCloudInstagram1.Photos[I].Caption <> '' then
        ListView1.Items.Add.Text := TMSFMXCloudInstagram1.Photos[I].Caption
      else
        ListView1.Items.Add.Text := 'Photo ' + IntToStr(I + 1);
    end;

    ListView1.ItemIndex := 0;
    GetPhotoInfo;
  end;
end;

procedure TForm1017.ConnectBtnClick(Sender: TObject);
var
  acc: boolean;
begin
  FCurrentUser := TInstagramUser.Create;
  TMSFMXCloudInstagram1.App.Key := InstagramAppkey;
  TMSFMXCloudInstagram1.App.Secret := InstagramAppSecret;

  if TMSFMXCloudInstagram1.App.Key <> '' then
  begin
    TMSFMXCloudInstagram1.PersistTokens.Key := TPath.GetDocumentsPath + '/Instagram.ini';
    TMSFMXCloudInstagram1.PersistTokens.Section := 'tokens';
    TMSFMXCloudInstagram1.LoadTokens;

    acc := TMSFMXCloudInstagram1.TestTokens;

    if not acc then
    begin
      TMSFMXCloudInstagram1.RefreshAccess;
      acc := TMSFMXCloudInstagram1.TestTokens;
      if not acc then
        TMSFMXCloudInstagram1.DoAuth;
    end
    else
    begin
      Connected := true;
      GetUserProfile;
    end;
  end
  else
    ShowMessage('Please provide a valid application ID for the client component');
end;

procedure TForm1017.DisconnectBtnClick(Sender: TObject);
begin
  if Assigned(FCurrentUser) then
  begin
    FCurrentUser.Free;
    FCurrentUser := nil;
  end;
  TMSFMXCloudInstagram1.LogOut;
  TMSFMXCloudInstagram1.ClearTokens;
  TMSFMXCloudImage1.URL := '';
  TMSFMXCloudImage2.URL := '';
  Label1.Text := '';
  Connected := false;
  edSearchUsers.Text := '';
  ListView1.Items.Clear;
  ListView2.Items.Clear;
  ListView3.Items.Clear;
  Label3.Text := '';
  Label4.Text := '';
  Label6.Text := '';
  Label8.Text := '';
  Label10.Text := '';
  Label13.Text := '';
  label20.Text := '';
  Label16.Text := '';
  Label19.Text := '';
  Label17.Text := '';
end;

procedure TForm1017.FormCreate(Sender: TObject);
begin
  Connected := false;
end;

procedure TForm1017.FormDestroy(Sender: TObject);
begin
  if Assigned(FCurrentUser) then
  begin
    FCurrentUser.Free;
    FCurrentUser := nil;
  end;
end;

procedure TForm1017.GetPhotoInfo;
var
  ph: TInstagramPhoto;
  I: Integer;
  li: TListViewITem;
begin
  if ListView1.ItemIndex > -1 then
  begin
    ph := TMSFMXCloudInstagram1.Photos[ListView1.ItemIndex];
    TMSFMXCloudImage1.URL := ph.Images.Thumbnail.URL;
    Label3.Text := ph.Location.Summary;
    Label4.Text := ph.Filter;
    Label6.Text := ph.Link;
    Label8.Text := DateTimeToStr(ph.CreatedTime);
    Label10.Text := IntToStr(ph.LikesCount);
    Label13.Text := ph.From.FullName;


    ListView2.Items.Clear;
    for I := 0 to ph.Comments.Count - 1 do
    begin
      li := ListView2.Items.Add;
      li.Detail := ph.Comments[I].From.FullName;
      li.Text := ph.Comments[I].Text;
    end;
  end;
end;

procedure TForm1017.GetUserInfo;
var
  User: TInstagramUser;
begin
  if ListView3.ItemIndex > -1 then
  begin
    if TMSFMXCloudInstagram1.Users.Count > 0 then
    begin
      User := TMSFMXCloudInstagram1.Users[ListView3.ItemIndex].User;
      TMSFMXCloudInstagram1.GetProfile(User.ID, User);
    end
    else
      User := TMSFMXCloudInstagram1.Profile;

    TMSFMXCloudImage2.URL := User.ProfilePicture;
    Label20.Text := User.UserName;
    Label16.Text := User.Bio;
    Label19.Text := User.Website;
    Label17.Text := 'Photos: ' + IntToStr(User.Photos)
      + ' Followers: ' + IntToStr(User.Followers)
      + ' Following: ' + IntToStr(User.Following);

  end;
end;

procedure TForm1017.GetUserProfile;
begin
  TMSFMXCloudInstagram1.GetProfile('', FCurrentUser);
  if FCurrentUser.UserName <> '' then
    Label1.Text := 'Welcome ' + FCurrentUser.UserName;
end;

procedure TForm1017.ListView1Change(Sender: TObject);
begin
  GetPhotoInfo;
end;

procedure TForm1017.ListView3Change(Sender: TObject);
begin
  GetUserInfo;
end;

procedure TForm1017.LoadFeed;
var
  I: integer;
begin
  Button4.Enabled := TMSFMXCloudInstagram1.GetFeed;

  if TMSFMXCloudInstagram1.Photos.Count > 0 then
  begin
    ListView1.Items.Clear;
    for I := 0 to TMSFMXCloudInstagram1.Photos.Count - 1 do
    begin
      if TMSFMXCloudInstagram1.Photos[I].Caption <> '' then
        ListView1.Items.Add.Text := TMSFMXCloudInstagram1.Photos[I].Caption
      else
        ListView1.Items.Add.Text := 'Photo ' + IntToStr(I + 1);
    end;

    ListView1.ItemIndex := 0;
  end;
end;

procedure TForm1017.TMSFMXCloudInstagram1ReceivedAccessToken(Sender: TObject);
begin
  TMSFMXCloudInstagram1.SaveTokens;
  Connected := true;
  GetUserProfile;
end;

end.
