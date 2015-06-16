unit UFaceBookDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TMSCloudBase, FMX.TMSCloudFacebook, FMX.Layouts, FMX.ListBox, FMX.Objects,
  FMX.TMSCloudImage, FMX.Edit, FMX.TMSCloudBaseFMX,
  FMX.TMSCloudCustomFacebook, iOUtils;

type
  TForm82 = class(TForm)
    ToolBar1: TToolBar;
    Button1: TButton;
    Button2: TButton;
    TMSFMXCloudFaceBook1: TTMSFMXCloudFaceBook;
    Button3: TButton;
    ListBox1: TListBox;
    Button4: TButton;
    btRemove: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    lbName: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lbGender: TLabel;
    lbWebsite: TLabel;
    lbUpdated: TLabel;
    TMSFMXCloudImage1: TTMSFMXCloudImage;
    ListBox2: TListBox;
    Button5: TButton;
    TMSFMXCloudImage2: TTMSFMXCloudImage;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ListBox1Change(Sender: TObject);
    procedure btRemoveClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure TMSFMXCloudFaceBook1ReceivedAccessToken(Sender: TObject);
    procedure ListBox2Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Connected: boolean;
    InitLV: boolean;
    InitLVC: boolean;
    FeedOffset: integer;
    procedure ToggleControls;
    procedure LoadFeed;
    procedure LoadComments;
    procedure LoadLikes;
    procedure ShowProfile(Profile: TFacebookProfile);
  end;

var
  Form82: TForm82;

implementation

{$R *.fmx}


// PLEASE USE A VALID INCLUDE FILE THAT CONTAINS THE APPLICATION KEY & SECRET
// FOR THE CLOUD STORAGE SERVICES YOU WANT TO USE
// STRUCTURE OF THIS .INC FILE SHOULD BE
//
// const
//  FacebookAppkey = 'xxxxxxxxx';
//  FacebookAppSecret = 'yyyyyyyy';

{$I APPIDS.INC}

procedure TForm82.Button1Click(Sender: TObject);
var
  acc: boolean;
begin
  TMSFMXCloudFacebook1.App.Key := FacebookAppkey;
  TMSFMXCloudFacebook1.App.Secret := FacebookAppSecret;

  if TMSFMXCloudFacebook1.App.Key <> '' then
  begin
    TMSFMXCloudFacebook1.PersistTokens.Key := TPath.GetDocumentsPath + '/facebook.ini';
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

procedure TForm82.Button2Click(Sender: TObject);
var
  ImageID: string;
begin
  if MessageDlg('Do you want to upload a sample file?', TMsgDlgType.mtConfirmation,[TMsgDlgBtn.mbYes,TMsgDlgBtn.mbNo],0) = mrYes then
  begin
    ImageID := TMSFMXCloudFacebook1.PostImage(Edit2.Text, TPath.GetDocumentsPath + '/sample.jpg');
    Edit3.Text := TMSFMXCloudFacebook1.GetImageURL(ImageID);
  end;
end;

procedure TForm82.Button3Click(Sender: TObject);
begin
  TMSFMXCloudFacebook1.Post(Edit2.Text, Edit1.Text, Edit3.Text);
  LoadFeed;
end;

procedure TForm82.Button4Click(Sender: TObject);
var
  I: Integer;
begin
  TMSFMXCloudFacebook1.GetFriends;
  ListBox1.BeginUpdate;
  for I := 0 to TMSFMXCloudFacebook1.FriendList.Count - 1 do
    ListBox1.Items.AddObject(TMSFMXCloudFacebook1.FriendList.Items[I].FullName, TMSFMXCloudFacebook1.FriendList.Items[I]);
  ListBox1.EndUpdate;
end;

procedure TForm82.Button5Click(Sender: TObject);
begin
  FeedOffset := 0;
  LoadFeed;
end;

procedure TForm82.btRemoveClick(Sender: TObject);
begin
  TMSFMXCloudFacebook1.ClearTokens;
  Connected := false;
  ToggleControls;
end;

procedure TForm82.FormCreate(Sender: TObject);
begin
  {$IFDEF IOS}
  TFile.Copy(ExtractFilePath(Paramstr(0)) + 'sample.jpg', TPath.GetDocumentsPath + '/sample.jpg', True);
  {$ENDIF}
  Connected := false;
  FeedOffset := 0;
  ToggleControls;
end;

procedure TForm82.ListBox1Change(Sender: TObject);
var
  Profile: TFacebookProfile;
begin
  Profile := ListBox1.Items.Objects[ListBox1.ItemIndex] as TFacebookProfile;
  Profile := TMSFMXCloudFacebook1.FriendList.Find(Profile.ID);

  TMSFMXCloudFacebook1.GetProfileInfo(Profile.ID, Profile);
  ShowProfile(Profile);
end;

procedure TForm82.ListBox2Change(Sender: TObject);
var
  fb: TFaceBookFeedItem;
begin
  if Assigned(ListBox2.Selected) then
  begin
    fb := (ListBox2.Selected.Data as TFacebookFeedItem);
    TMSFMXCloudImage2.URL := fb.ImageURL;
    Label13.Text := fb.Caption;
    Label14.Text := fb.Summary;
    Label16.Text := fb.Description;
    Label19.Text := fb.Story;
    Label20.Text := fb.Link;
  end;
end;

procedure TForm82.LoadComments;
begin

end;

procedure TForm82.LoadFeed;
var
  I: integer;
  Profile: TFacebookProfile;
  str: String;
begin
  if FeedOffset = 0 then
    ListBox2.Items.Clear;

  Profile := TMSFMXCloudFaceBook1.Profile;

  TMSFMXCloudFaceBook1.GetFeed(Profile,10,FeedOffset);

  InitLV := True;

  ListBox2.BeginUpdate;
  for I := FeedOffset to profile.Feed.Count - 1 do
  begin
    str := profile.Feed[i].Caption;
    if str = '' then
      str := profile.Feed[i].Text;
    if str = '' then
      str := profile.Feed[i].ID;

    ListBox2.Items.AddObject(str, profile.Feed[i]);
  end;
  ListBox2.EndUpdate;
  InitLV := false;
end;

procedure TForm82.LoadLikes;
begin

end;

procedure TForm82.ShowProfile(Profile: TFacebookProfile);
begin
  lbName.Text := Profile.FirstName + ' ' + Profile.LastName;
  Label5.Text := Profile.BirthDay;
  Label6.Text := Profile.Location.Name;
  if Profile.Gender = fbMale then
    lbGender.Text := 'Male'
  else
    lbGender.Text := 'Female';
  lbWebsite.Text := Profile.Website;
  lbUpdated.Text := DateTimeToStr(Profile.UpdatedTime);
  TMSFMXCloudImage1.URL := Profile.ImageURL;
end;

procedure TForm82.TMSFMXCloudFaceBook1ReceivedAccessToken(Sender: TObject);
begin
  TMSFMXCloudFaceBook1.SaveTokens;
  Connected := true;
  ToggleControls;
  TMSFMXCloudFaceBook1.GetUserInfo;
end;

procedure TForm82.ToggleControls;
begin
  Button4.Enabled := Connected;
  Button5.Enabled := Connected;
  ListBox1.Enabled := Connected;
  ListBox2.Enabled := Connected;
  Button2.Enabled := Connected;
  Button3.Enabled := Connected;
  Edit1.Enabled := Connected;
  Edit2.Enabled := Connected;
  Edit3.Enabled := Connected;
  btRemove.Enabled := Connected;
  Button1.Enabled := not Connected;
end;

end.
