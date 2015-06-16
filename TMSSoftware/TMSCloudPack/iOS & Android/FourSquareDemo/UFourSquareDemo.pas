unit UFourSquareDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TMSCloudBase,
  FMX.TMSCloudFourSquare, FMX.TMSCloudXUtil, FMX.StdCtrls, FMX.Objects,
  FMX.TMSCloudImage, FMX.Layouts, FMX.ListBox, FMX.Edit, FMX.TreeView;

type
  TForm1 = class(TForm)
    TMSFMXCloudFourSquare1: TTMSFMXCloudFourSquare;
    ToolBar1: TToolBar;
    btConnect: TButton;
    btDisconnect: TButton;
    lbUserName: TLabel;
    lbEmail: TLabel;
    lbCity: TLabel;
    lbBio: TLabel;
    lbPhone: TLabel;
    lbFriends: TLabel;
    lbCheckins: TLabel;
    lbGender: TLabel;
    ciUser: TTMSFMXCloudImage;
    Panel1: TPanel;
    Line1: TLine;
    btGetCheckIns: TButton;
    ListBox2: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    lbKeyword: TLabel;
    lbCat: TLabel;
    lbLocation: TLabel;
    ListBox3: TListBox;
    edKeyword: TEdit;
    edSearch: TEdit;
    btSearch: TButton;
    lbUserFriends: TLabel;
    lbUserCheckins: TLabel;
    Label3: TLabel;
    TreeView1: TTreeView;
    lbCategories: TLabel;
    lbResult: TLabel;
    btCheckIn: TButton;
    edCheckin: TEdit;
    TMSFMXCloudImage1: TTMSFMXCloudImage;
    procedure btConnectClick(Sender: TObject);
    procedure TMSFMXCloudFourSquare1ReceivedAccessToken(Sender: TObject);
    procedure btDisconnectClick(Sender: TObject);
    procedure btGetCheckInsClick(Sender: TObject);
    procedure btSearchClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TreeView1Change(Sender: TObject);
    procedure ListBox3Change(Sender: TObject);
    procedure btCheckInClick(Sender: TObject);
    procedure ListBox2Change(Sender: TObject);

  private
    { Private declarations }
    Connected: Boolean;
    CategoryList: TStringList;
    CategoryIDList: TStringList;
    VenueImage: integer;
  public
    { Public declarations }
    procedure ToggleControls;
    procedure FillUserProfile(Profile: TFourSquareUserProfile = nil);
    procedure FillTreeView(ATreeView: TTreeView);
    procedure GetVenues(CategoryID: string = '');
    procedure FillCategories;
    procedure SelectCategory;
    procedure LoadCheckIns;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

// PLEASE USE A VALID INCLUDE FILE THAT CONTAINS THE APPLICATION KEY & SECRET
// FOR THE CLOUD STORAGE SERVICES YOU WANT TO USE
// STRUCTURE OF THIS .INC FILE SHOULD BE
//
// const
//  FourSquareAppkey = 'xxxxxxxxx';
//  FourSquareAppSecret = 'yyyyyyyy';

{$I APPIDS.INC}

procedure TForm1.FillCategories;
begin
  if TMSFMXCloudFourSquare1.Categories.Count = 0 then
    TMSFMXCloudFourSquare1.GetCategories;

  FillTreeView(TreeView1);
end;

procedure TForm1.FillTreeView(ATreeView: TTreeView);

  procedure AddFolder(ParentNode: TTreeViewItem; Cats: TFourSquareCategories);
  var
    tn: TTreeViewItem;
    i: integer;
  begin
    for i := 0 to Cats.Count - 1 do
    begin
      tn := TTreeViewItem.Create(ParentNode);
      tn.TagString := Cats.Items[i].ID;
      tn.Text := Cats.Items[i].Summary;
      if Assigned(ParentNode) then
        ParentNode.AddObject(tn)
      else
        ATreeView.AddObject(tn);

      if Assigned(Cats.Items[i].SubCategories) then
        if (Cats.Items[i].SubCategories.Count > 0) then
          AddFolder(tn, Cats.Items[i].SubCategories);
    end;
  end;

begin
  if Assigned(ATreeView) then
  begin
    ATreeView.Clear;
    AddFolder(nil, TMSFMXCloudFourSquare1.Categories);
    if ATreeView.Count > 0 then
    begin
      ATreeView.Selected := ATreeView.Items[0];
    end;
  end;
end;

procedure TForm1.btGetCheckInsClick(Sender: TObject);
begin
  LoadCheckins;
end;

procedure TForm1.btSearchClick(Sender: TObject);
begin
  if CategoryIDList.Count = 0 then
    Exit;

  GetVenues(CategoryIDList[0]);
end;

procedure TForm1.btCheckInClick(Sender: TObject);
begin
  if ListBox3.ItemIndex > -1 then
    TMSFMXCloudFourSquare1.CheckIn(TMSFMXCloudFourSquare1.Venues[ListBox3.ItemIndex].ID, edCheckIn.Text);

  LoadCheckIns;
end;

procedure TForm1.btConnectClick(Sender: TObject);
var
  acc: boolean;
begin
  TMSFMXCloudFourSquare1.App.Key := FourSquare_AppKey;
  TMSFMXCloudFourSquare1.App.Secret := FourSquare_AppSecret;

  if TMSFMXCloudFourSquare1.App.Key <> '' then
  begin
    TMSFMXCloudFourSquare1.PersistTokens.Key := XGetDocumentsDirectory + '/foursquare.ini';
    TMSFMXCloudFourSquare1.PersistTokens.Section := 'tokens';
    TMSFMXCloudFourSquare1.LoadTokens;

    acc := TMSFMXCloudFourSquare1.TestTokens;

    if not acc then
    begin
      TMSFMXCloudFourSquare1.RefreshAccess;
      acc := TMSFMXCloudFourSquare1.TestTokens;
      if not acc then
        TMSFMXCloudFourSquare1.DoAuth;
    end
    else
    begin
      Connected := true;
      ToggleControls;
    end;

    if Connected then
    begin
      FillUserProfile;
      FillCategories;
    end;
  end
  else
    ShowMessage('Please provide a valid application ID for the client component');
end;

procedure TForm1.btDisconnectClick(Sender: TObject);
begin
  TMSFMXCloudFourSquare1.ClearTokens;
  Connected := false;
  ToggleControls;
end;

procedure TForm1.FillUserProfile(Profile: TFourSquareUserProfile = nil);
begin
  if Profile = nil then
    Profile := TMSFMXCloudFourSquare1.UserProfile;

  TMSFMXCloudFourSquare1.GetUserProfile(Profile);

  lbUserName.Text := Profile.FirstName + ' ' + Profile.LastName;
  lbEmail.Text := Profile.Email;
  lbCity.Text := Profile.HomeCity;
  lbBio.Text := Profile.Bio;
  lbPhone.Text := Profile.PhoneNumber;
  lbUserFriends.Text := IntToStr(Profile.FriendsCount);
  lbUserCheckins.Text := IntToStr(Profile.CheckInsCount);
  ciUser.URL := Profile.ImageURL;

  if Profile.Gender = fgFemale then
    lbGender.Text := 'Female'
  else if Profile.Gender = fgMale then
    lbGender.Text := 'Male'
  else
    lbGender.Text := '';
end;



procedure TForm1.FormCreate(Sender: TObject);
begin
  VenueImage := -1;
  ToggleControls;
  CategoryList := TStringList.Create;
  CategoryIDList := TStringList.Create;
end;

procedure TForm1.GetVenues(CategoryID: string);
var
  I: integer;
  err: string;
begin
  if (edSearch.Text <> EmptyStr) or (edKeyword.Text <> EmptyStr) then
  begin
    err := TMSFMXCloudFourSquare1.GetNearbyVenues(0, 0, edSearch.Text, edKeyword.Text, CategoryID, 20);

    if err <> EmptyStr then
      ShowMessage(err)
    else
    begin
      lbResult.Text := TMSFMXCloudFourSquare1.Location.Summary + ', ' + TMSFMXCloudFourSquare1.Location.CountryCode;

      ListBox3.Items.Clear;
      for I := 0 to TMSFMXCloudFourSquare1.Venues.Count - 1 do
      begin
        ListBox3.Items.AddObject(TMSFMXCloudFourSquare1.Venues[I].Summary, TMSFMXCloudFourSquare1.Venues[I]);
      end;
    end;
  end;
end;

procedure TForm1.ListBox2Change(Sender: TObject);
begin
  if Assigned(ListBox2.Selected) then
    ShowMessage((listbox2.Selected.Data as TFourSquareCheckIn).Comment);
end;

procedure TForm1.ListBox3Change(Sender: TObject);
begin
  if Assigned(ListBox3.Selected) then
  begin
    btCheckIn.Text := 'Check-in "'+ ListBox3.Selected.Text+'"';
    TMSFMXCloudFourSquare1.GetPhotos((ListBox3.Selected.Data as TFourSquareVenue), 1, 0);
    if (ListBox3.Selected.Data as TFourSquareVenue).Photos.Count > 0 then
      TMSFMXCloudImage1.URL := (ListBox3.Selected.Data as TFourSquareVenue).Photos[0].ImageURL
    else
      TMSFMXCloudImage1.URL := '';
  end;
end;

procedure TForm1.LoadCheckIns;
var
  I: integer;
begin
  TMSFMXCloudFourSquare1.GetCheckIns;

  ListBox2.Clear;
  for I := 0 to TMSFMXCloudFourSquare1.UserProfile.CheckIns.Count - 1 do
    ListBox2.Items.AddObject(TMSFMXCloudFourSquare1.UserProfile.CheckIns[I].Venue.Summary + ' ' + DateToStr(TMSFMXCloudFourSquare1.UserProfile.CheckIns[I].Created), TMSFMXCloudFourSquare1.UserProfile.CheckIns[I]);
end;

procedure TForm1.SelectCategory;
begin
  CategoryList.Clear;
  CategoryIDList.Clear;
  lbCategories.Text := 'Selected Category: ';
  if Assigned(TreeView1.Selected) then
  begin
    CategoryList.Add(TreeView1.Selected.Text);
    CategoryIDList.Add(TreeView1.Selected.TagString);
    lbCategories.Text := 'Selected Category: ' + TreeView1.Selected.Text;
  end;
end;

procedure TForm1.TMSFMXCloudFourSquare1ReceivedAccessToken(Sender: TObject);
begin
  TMSFMXCloudFourSquare1.SaveTokens;
  Connected := true;
  FillUserProfile;
  FillCategories;
  ToggleControls;
end;

procedure TForm1.ToggleControls;
begin
  btConnect.Enabled := not connected;
  btDisconnect.Enabled := connected;
  edKeyword.Enabled := connected;
  edSearch.Enabled := connected;
  btSearch.Enabled := connected;
  btCheckIn.Enabled := connected;
  edCheckIn.Enabled := connected;
  btGetCheckIns.Enabled := connected;
  ListBox2.Enabled := connected;
  ListBox3.Enabled := connected;
  TreeView1.Enabled := connected;
end;

procedure TForm1.TreeView1Change(Sender: TObject);
begin
  SelectCategory;
end;

end.
