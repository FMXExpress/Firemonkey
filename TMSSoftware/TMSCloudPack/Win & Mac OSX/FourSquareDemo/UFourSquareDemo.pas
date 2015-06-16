unit UFourSquareDemo;

interface

uses
  FMX.Forms, FMX.Messages, FMX.TMSCloudBase, FMX.TMSCloudFourSquare, FMX.Controls, FMX.TabControl,
  FMX.Objects, FMX.TMSCloudImage, FMX.Grid, FMX.Layouts, FMX.TMSCloudListView, UITypes,
  FMX.Edit, FMX.StdCtrls, System.Classes, FMX.Types, FMX.TreeView, FMX.Dialogs, SysUtils, UCategories,
  FMX.TMSCloudBaseFMX, FMX.TMSCloudCustomFourSquare, iOUtils;

type
  TForm1 = class(TForm)
StyleBook1: TStyleBook;

    Panel1: TPanel;
    btConnect: TButton;
    btRemove: TButton;
    Panel2: TPanel;
    GroupBox3: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label1: TLabel;
    lbResult: TLabel;
    Label8: TLabel;
    lbCategories: TLabel;
    edKeyword: TEdit;
    edSearch: TEdit;
    btSearch: TButton;
    btCategories: TButton;
    btDelCat: TButton;
    lvResults: TTMSFMXCloudListView;
    GroupBox2: TGroupBox;
    lbUserName: TLabel;
    ciUser: TTMSFMXCloudImage;
    lbEmail: TLabel;
    lbCity: TLabel;
    lbGender: TLabel;
    lbBio: TLabel;
    lbPhone: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    lbUserFriends: TLabel;
    lbUserCheckins: TLabel;
    btGetCheckIns: TButton;
    lvCheckIns: TTMSFMXCloudListView;
    Image1: TImage;
    GroupBox1: TGroupBox;
    lbPhotoCount: TLabel;
    TMSFMXCloudCloudImage1: TTMSFMXCloudImage;
    ciCategoryIcon: TTMSFMXCloudImage;
    lbVenueName: TLabel;
    lbVenueAddress: TLabel;
    Label4: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    lbUsers: TLabel;
    lbCheckIns: TLabel;
    lbHereNow: TLabel;
    lbRating: TLabel;
    Label6: TLabel;
    lbWebsite: TLabel;
    PageControl1: TTabControl;
    TabSheet1: TTabItem;
    lvVenue: TTMSFMXCloudListView;
    TabSheet2: TTabItem;
    lvTips: TTMSFMXCloudListView;
    TabSheet3: TTabItem;
    lvSpecials: TTMSFMXCloudListView;
    btPrevImage: TButton;
    btNextImage: TButton;
    edCheckIn: TEdit;
    btSimilar: TButton;
    btPhotos: TButton;
    btTips: TButton;
    btCheckIn: TButton;
    btCategory: TButton;
    TMSFMXCloudFourSquare1: TTMSFMXCloudFourSquare;
    procedure btConnectClick(Sender: TObject);
    procedure btGetCheckInsClick(Sender: TObject);
    procedure btSearchClick(Sender: TObject);
    procedure btCheckInClick(Sender: TObject);
    procedure TTMSFMXCloudFourSquare1ReceivedAccessToken(Sender: TObject);
    procedure btTipsClick(Sender: TObject);
    procedure btPhotosClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btNextImageClick(Sender: TObject);
    procedure btPrevImageClick(Sender: TObject);
    procedure btSimilarClick(Sender: TObject);
    procedure btRemoveClick(Sender: TObject);
    procedure btCategoryClick(Sender: TObject);
    procedure btCategoriesClick(Sender: TObject);
    procedure btDelCatClick(Sender: TObject);
    procedure lbWebsiteClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lvResultsChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Connected: boolean;
    VenueImage: integer;
    CategoryList: TStringList;
    CategoryIDList: TStringList;
    procedure ToggleControls;
    procedure FillUserProfile(Profile: TFourSquareUserProfile = nil);
    procedure GetVenues(CategoryID: string = '');
    procedure FillTreeView(AtreeView: TTreeView);
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
//  FourSquareAppkey = 'xxxxxxxxx';
//  FourSquareAppSecret = 'yyyyyyyy';

{$I APPIDS.INC}

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

procedure TForm1.TTMSFMXCloudFourSquare1ReceivedAccessToken(Sender: TObject);
begin
  TMSFMXCloudFourSquare1.SaveTokens;
  Connected := true;
  ToggleControls;
end;

procedure TForm1.btConnectClick(Sender: TObject);
var
  acc: boolean;
begin
  TMSFMXCloudFourSquare1.OnReceivedAccessToken := TTMSFMXCloudFourSquare1ReceivedAccessToken;
  TMSFMXCloudFourSquare1.App.Key := FourSquare_AppKey;
  TMSFMXCloudFourSquare1.App.Secret := FourSquare_AppSecret;

  if TMSFMXCloudFourSquare1.App.Key <> '' then
  begin
    TMSFMXCloudFourSquare1.PersistTokens.Key := ExtractFilePath(ParamStr(0)) + 'foursquare.ini';
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
      FillUserProfile;

  end
  else
    ShowMessage('Please provide a valid application ID for the client component');
end;

procedure TForm1.btGetCheckInsClick(Sender: TObject);
var
  I: integer;
  li: TListItem;
begin
  TMSFMXCloudFourSquare1.GetCheckIns;

  lvCheckIns.Items.Clear;
  for I := 0 to TMSFMXCloudFourSquare1.UserProfile.CheckIns.Count - 1 do
  begin
    li := lvCheckIns.Items.Add;
    li.Text  := TMSFMXCloudFourSquare1.UserProfile.CheckIns[I].Venue.Summary;
    li.SubItems.Add(DateToStr(TMSFMXCloudFourSquare1.UserProfile.CheckIns[I].Created));
    li.SubItems.Add(TMSFMXCloudFourSquare1.UserProfile.CheckIns[I].Comment);
  end;
end;

procedure TForm1.btNextImageClick(Sender: TObject);
begin
  VenueImage := VenueImage + 1;
  if TMSFMXCloudFourSquare1.Venues[lvResults.ItemIndex].Photos.Count > VenueImage then
  begin
    TMSFMXCloudCloudImage1.URL := TMSFMXCloudFourSquare1.Venues[lvResults.ItemIndex].Photos[VenueImage].ImageURL;
    lbPhotoCount.Text  := IntToStr(VenueImage+1) + '/' + IntToStr(TMSFMXCloudFourSquare1.Venues[lvResults.ItemIndex].Photos.Count);
  end;
end;

procedure TForm1.FillUserProfile(Profile: TFourSquareUserProfile);
begin
  if Profile = nil then
    Profile := TMSFMXCloudFourSquare1.UserProfile;

  TMSFMXCloudFourSquare1.GetUserProfile(Profile);

  lbUserName.Text  := Profile.FirstName + ' ' + Profile.LastName;
  lbEmail.Text  := Profile.Email;
  lbCity.Text  := Profile.HomeCity;
  lbBio.Text  := Profile.Bio;
  lbPhone.Text  := Profile.PhoneNumber;
  lbUserFriends.Text  := IntToStr(Profile.FriendsCount);
  lbUserCheckins.Text  := IntToStr(Profile.CheckInsCount);
  ciUser.URL := Profile.ImageURL;

  if Profile.Gender = fgFemale then
    lbGender.Text  := 'Female'
  else if Profile.Gender = fgMale then
    lbGender.Text  := 'Male'
  else
    lbGender.Text  := '';
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  CategoryList.Free;
  CategoryIDList.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  VenueImage := -1;
  ToggleControls;
  CategoryList := TStringList.Create;
  CategoryIDList := TStringList.Create;
  lvResults.ColumnByIndex(1).Width := 300;
end;

procedure TForm1.GetVenues(CategoryID: string);
var
  I: integer;
  err: string;
  li: TListItem;
begin
  if (edSearch.Text <> EmptyStr) or (edKeyword.Text <> EmptyStr) then
  begin
    err := TMSFMXCloudFourSquare1.GetNearbyVenues(0, 0, edSearch.Text, edKeyword.Text, CategoryID, 20);

    if err <> EmptyStr then
      ShowMessage(err)
    else
    begin
      lbResult.Text  := TMSFMXCloudFourSquare1.Location.Summary + ', ' + TMSFMXCloudFourSquare1.Location.CountryCode;

      lvResults.Items.Clear;
      for I := 0 to TMSFMXCloudFourSquare1.Venues.Count - 1 do
      begin
        li := lvResults.Items.Add;
        li.Text  := TMSFMXCloudFourSquare1.Venues[I].Summary;
        li.SubItems.Add(TMSFMXCloudFourSquare1.Venues[I].Address);
      end;
    end;
  end;
end;

procedure TForm1.lbWebsiteClick(Sender: TObject);
begin
  if (Connected) and (lbwebsite.Text  <> EmptyStr) then
    ShowMessage(lbwebsite.Text);
end;

procedure TForm1.lvResultsChange(Sender: TObject);
var
  fv: TFourSquareVenue;
  i: integer;
  li: TListItem;
  hours: string;
begin
  lvTips.Items.Clear;
  if lvResults.ItemIndex >= 0 then
  begin
    TMSFMXCloudCloudImage1.URL := '';
    lbPhotoCount.Text  := '0/0';
    fv := TMSFMXCloudFourSquare1.Venues[lvResults.ItemIndex];
    TMSFMXCloudFourSquare1.GetVenue(fv);

    lbRating.Text  := FloatToStr(fv.Rating);
    lbVenueName.Text  := fv.Summary;
    lbVenueAddress.Text  := fv.Address;

    hours := fv.Hours.Status;
    if fv.Hours.IsOpen = fiOpen then
      hours := hours + ' (Open)'
    else if fv.Hours.IsOpen = fiClosed then
      hours := hours + ' (Closed)';

    if fv.CrossStreet <> '' then
      lbVenueAddress.Text  := lbVenueAddress.Text  + ' (' + fv.CrossStreet + ')';

    lbCheckIns.Text  := IntToStr(fv.CheckInsCount);
    lbUsers.Text  := IntToStr(fv.UsersCount);
    lbWebsite.Text  := fv.Website;
    lbHereNow.Text  := IntToStr(fv.HereNowCount);

    lvVenue.Items[0].Text := hours;
    lvVenue.Items[0].SubItems[0] := fv.City;
    lvVenue.Items[0].SubItems[1] := fv.PostalCode;
    lvVenue.Items[0].SubItems[2] := fv.Country;
    lvVenue.Items[0].SubItems[3] := fv.Phone;

    PageControl1.Tabs[1].Text  := 'Tips (' + IntToStr(fv.TipCount) + ')';
    PageControl1.Tabs[2].Text  := 'Specials (' + IntToStr(fv.Specials.Count) + ')';

    if fv.Categories.Count > 0 then
    begin
      ciCategoryIcon.URL := fv.Categories[0].IconURL;
      lvVenue.Items[0].SubItems[4] := fv.Categories[0].Summary;
    end;

    VenueImage := 0;
    if fv.Photos.Count > 0 then
    begin
      TMSFMXCloudCloudImage1.URL := fv.Photos[0].ImageURL;
      lbPhotoCount.Text  := IntToStr(VenueImage+1) + '/' + IntToStr(fv.Photos.Count);
    end;

    lvSpecials.Items.Clear;
    for I := 0 to fv.Specials.Count - 1 do
    begin
      li := lvSpecials.Items.Add;
      li.Text  := fv.Specials[I].Title;
      li.SubItems.Add(fv.Specials[I].Summary);
    end;
  end;
end;

procedure TForm1.btCategoriesClick(Sender: TObject);
var
  F: TForm2;
begin
  if TMSFMXCloudFourSquare1.Categories.Count = 0 then
    TMSFMXCloudFourSquare1.GetCategories;

  F := TForm2.Create(Self);
  FillTreeView(F.TreeView1);

  if F.ShowModal = mrOk then
  begin
    if Assigned(F.TreeView1.Selected) then
    begin
      CategoryList.Add(TFourSquareCategoryItem(F.TreeView1.Selected.Data.AsObject).Summary);
      CategoryIDList.Add(TFourSquareCategoryItem(F.TreeView1.Selected.Data.AsObject).ID);

      lbCategories.Text  := StringReplace(CategoryList.CommaText, '&', '&&', [rfReplaceAll]);
    end;
  end;

  F.Release;
end;

procedure TForm1.btCategoryClick(Sender: TObject);
begin
  if lvResults.ItemIndex >= 0 then
  begin
    GetVenues(TMSFMXCloudFourSquare1.Venues[lvResults.ItemIndex].Categories[0].ID);
  end;
end;

procedure TForm1.btCheckInClick(Sender: TObject);
begin
  if lvResults.ItemIndex >= 0 then
    TMSFMXCloudFourSquare1.CheckIn(TMSFMXCloudFourSquare1.Venues[lvResults.ItemIndex].ID, edCheckIn.Text);
end;

procedure TForm1.btDelCatClick(Sender: TObject);
begin
  if CategoryList.Count > 0 then
  begin
    CategoryList.Delete(CategoryList.Count - 1);
    CategoryIDList.Delete(CategoryIDList.Count - 1);
    lbCategories.Text  := StringReplace(CategoryList.CommaText, '&', '&&', [rfReplaceAll]);
  end;
end;

procedure TForm1.btPhotosClick(Sender: TObject);
begin
  if (lvResults.ItemIndex >= 0) then
  begin
    TMSFMXCloudFourSquare1.GetPhotos(TMSFMXCloudFourSquare1.Venues[lvResults.ItemIndex], 50);
    VenueImage := 0;

    if TMSFMXCloudFourSquare1.Venues[lvResults.ItemIndex].Photos.Count > 0 then
    begin
      TMSFMXCloudCloudImage1.URL := TMSFMXCloudFourSquare1.Venues[lvResults.ItemIndex].Photos[0].ImageURL;
      lbPhotoCount.Text  := IntToStr(VenueImage+1) + '/' + IntToStr(TMSFMXCloudFourSquare1.Venues[lvResults.ItemIndex].Photos.Count);
    end;
  end;
end;

procedure TForm1.btPrevImageClick(Sender: TObject);
begin
  VenueImage := VenueImage - 1;
  if (TMSFMXCloudFourSquare1.Venues[lvResults.ItemIndex].Photos.Count > VenueImage) and (VenueImage >= 0) then
  begin
    TMSFMXCloudCloudImage1.URL := TMSFMXCloudFourSquare1.Venues[lvResults.ItemIndex].Photos[VenueImage].ImageURL;
    lbPhotoCount.Text  := IntToStr(VenueImage+1) + '/' + IntToStr(TMSFMXCloudFourSquare1.Venues[lvResults.ItemIndex].Photos.Count);
  end;
end;

procedure TForm1.btRemoveClick(Sender: TObject);
begin
  TMSFMXCloudFourSquare1.ClearTokens;
  Connected := false;
  ToggleControls;
end;

procedure TForm1.btSearchClick(Sender: TObject);
begin
  GetVenues(CategoryIDList.CommaText);
end;

procedure TForm1.btSimilarClick(Sender: TObject);
var
  I: integer;
  li: TListItem;
begin
    if lvResults.ItemIndex >= 0 then
    begin
      TMSFMXCloudFourSquare1.GetSimilarVenues(TMSFMXCloudFourSquare1.Venues[lvResults.ItemIndex].ID);

      lvResults.Items.Clear;
      for I := 0 to TMSFMXCloudFourSquare1.Venues.Count - 1 do
      begin
        li := lvResults.Items.Add;
        li.Text  := TMSFMXCloudFourSquare1.Venues[I].Summary;
        li.SubItems.Add(TMSFMXCloudFourSquare1.Venues[I].Address);
      end;
    end;
end;

procedure TForm1.btTipsClick(Sender: TObject);
var
  I: Integer;
  li: TListItem;
  item: TFourSquareVenue;
begin
  PageControl1.TabIndex := 1;
  if (PageControl1.TabIndex = 1) and (lvResults.ItemIndex >= 0) then
  begin
    item := TMSFMXCloudFourSquare1.Venues[lvResults.ItemIndex];

    if (item.TipCount > 0)
    and (item.Tips.Count = 0 )then
    begin
      TMSFMXCloudFourSquare1.GetTips(item);

      for I := 0 to item.Tips.Count - 1 do
      begin
        li := lvTips.Items.Add;
        li.Text  := item.Tips[I].Summary;
        li.SubItems.Add(DateToStr(item.Tips[I].Created));
        li.SubItems.Add(item.Tips[I].User.FirstName + ' ' + item.Tips[I].User.LastName);
        li.SubItems.Add(IntToStr(item.Tips[I].LikesCount));
        li.SubItems.Add(BoolToStr(item.Tips[I].Liked));
      end;
    end;
  end;
end;

procedure TForm1.ToggleControls;
begin
  btConnect.Enabled := not connected;
  btRemove.Enabled := connected;
  edKeyword.Enabled := connected;
  edSearch.Enabled := connected;
  btSearch.Enabled := connected;
  btPrevImage.Enabled := connected;
  btNextImage.Enabled := connected;
  btSimilar.Enabled := connected;
  btPhotos.Enabled := connected;
  btTips.Enabled := connected;
  btCheckIn.Enabled := connected;
  edCheckIn.Enabled := connected;
  PageControl1.Enabled := connected;
  lvVenue.Enabled := connected;
  btCategory.Enabled := connected;
  btCategories.Enabled := connected;
  btDelCat.Enabled := connected;
  btGetCheckIns.Enabled := connected;
  lvResults.Enabled := connected;
  lbWebsite.Enabled := connected;
end;

end.
