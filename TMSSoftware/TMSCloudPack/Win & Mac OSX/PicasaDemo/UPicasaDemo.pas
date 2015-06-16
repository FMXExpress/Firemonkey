unit UPicasaDemo;

interface

uses
  FMX.Forms, FMX.TMSCloudBase, FMX.TMSCloudPicasa, FMX.Controls,
  FMX.Dialogs, FMX.Grid, FMX.Layouts, FMX.TMSCloudListView, FMX.Objects, SysUtils,
  FMX.TMSCloudImage, FMX.StdCtrls, FMX.Edit, System.Classes, FMX.Types, UITypes,
  FMX.TMSCloudBaseFMX, FMX.TMSCloudCustomGoogle, FMX.TMSCloudGoogleFMX,
  FMX.TMSCloudCustomPicasa;


type
  TForm5 = class(TForm)
StyleBook1: TStyleBook;

    TMSFMXCloudPicasa1: TTMSFMXCloudPicasa;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    btConnect: TButton;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    edPhotoDescription: TEdit;
    btUpload: TButton;
    lvPhotos: TTMSFMXCloudListView;
    btDeletePhoto: TSpeedButton;
    btUpdatePhoto: TButton;
    edPhotoTags: TEdit;
    Label4: TLabel;
    lbPhotoCount: TLabel;
    btPrev: TButton;
    btNext: TButton;
    SaveDialog1: TSaveDialog;
    btDownload: TButton;
    edLatitude: TEdit;
    edLongitude: TEdit;
    Label6: TLabel;
    Label7: TLabel;
    Image1: TImage;
    edSearch: TEdit;
    btSearch: TButton;
    GroupBox2: TGroupBox;
    lvAlbums: TTMSFMXCloudListView;
    edAlbumDescription: TEdit;
    Label3: TLabel;
    Label2: TLabel;
    btAlbums: TButton;
    btCreateAlbum: TButton;
    btUploadFolder: TButton;
    edAlbumTitle: TEdit;
    GroupBox1: TGroupBox;
    TMSFMXCloudCloudImage1: TTMSFMXCloudImage;
    btRemove: TButton;
    lvComments: TTMSFMXCloudListView;
    TMSFMXCloudCloudImage2: TTMSFMXCloudImage;
    btComments: TButton;
    Label5: TLabel;
    lbAuthor: TLabel;
    btDeleteAlbum: TSpeedButton;
    procedure btConnectClick(Sender: TObject);
    procedure TMSFMXCloudPicasa1ReceivedAccessToken(Sender: TObject);
    procedure btAlbumsClick(Sender: TObject);
    procedure btUploadClick(Sender: TObject);
    procedure btCreateAlbumClick(Sender: TObject);
    procedure btDeleteAlbumClick(Sender: TObject);
    procedure btDeletePhotoClick(Sender: TObject);
    procedure btUpdatePhotoClick(Sender: TObject);
    procedure btSearchClick(Sender: TObject);
    procedure btNextClick(Sender: TObject);
    procedure btPrevClick(Sender: TObject);
    procedure btDownloadClick(Sender: TObject);
    procedure btCommentsClick(Sender: TObject);
    procedure btUploadFolderClick(Sender: TObject);
    procedure btRemoveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lvAlbumsChange(Sender: TObject);
    procedure lvPhotosChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    connected: boolean;
    PageIndex: integer;
    procedure ToggleControls;
    procedure GetAlbums;
    procedure GetPhotos;
    procedure FillPhotos(Photos: TPicasaPhotos);
    procedure FillComments(Photo: TPicasaPhoto);
    procedure DoSearch;
  end;

var
  Form5: TForm5;

implementation

{$R *.FMX}

// PLEASE USE A VALID INCLUDE FILE THAT CONTAINS THE APPLICATION KEY & SECRET
// FOR THE CLOUD STORAGE SERVICES YOU WANT TO USE
// STRUCTURE OF THIS .INC FILE SHOULD BE
//
// const
//  GAppkey = 'xxxxxxxxx';
//  GAppSecret = 'yyyyyyyy';

{$I APPIDS.INC}

procedure TForm5.TMSFMXCloudPicasa1ReceivedAccessToken(Sender: TObject);
begin
  TMSFMXCloudPicasa1.SaveTokens;
  Connected := true;
  ToggleControls;
end;


procedure TForm5.btAlbumsClick(Sender: TObject);
begin
  GetAlbums;
end;

procedure TForm5.btCommentsClick(Sender: TObject);
begin
  if lvPhotos.ItemIndex >= 0 then
  begin
    TMSFMXCloudPicasa1.GetComments(TPicasaPhoto(lvPhotos.Selected.Data));
    FillComments(TPicasaPhoto(lvPhotos.Selected.Data));
  end;
end;

procedure TForm5.btConnectClick(Sender: TObject);
var
  acc: boolean;
begin
  TMSFMXCloudPicasa1.App.Key := GAppkey;
  TMSFMXCloudPicasa1.App.Secret := GAppSecret;

  if TMSFMXCloudPicasa1.App.Key <> '' then
  begin
    TMSFMXCloudPicasa1.PersistTokens.Key := ExtractFilePath(Paramstr(0)) + 'picasa.ini';
    TMSFMXCloudPicasa1.PersistTokens.Section := 'tokens';
    TMSFMXCloudPicasa1.LoadTokens;

    acc := TMSFMXCloudPicasa1.TestTokens;

    if not acc then
    begin
      TMSFMXCloudPicasa1.RefreshAccess;
      TMSFMXCloudPicasa1.DoAuth;
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

procedure TForm5.btCreateAlbumClick(Sender: TObject);
var
  Album: TPicasaAlbum;
begin
  Album := TMSFMXCloudPicasa1.Albums.Add;
  Album.Title := edAlbumTitle.Text;
  Album.Summary := edAlbumDescription.Text;
  TMSFMXCloudPicasa1.CreateAlbum(Album);
end;

procedure TForm5.btUpdatePhotoClick(Sender: TObject);
var
  Photo: TPicasaPhoto;
begin
  if Assigned(lvAlbums.Selected) and Assigned(lvPhotos.Selected) then
  begin
    Photo := TMSFMXCloudPicasa1.Albums[lvAlbums.ItemIndex].Photos[lvPhotos.ItemIndex];
    Photo.Summary := edPhotoDescription.Text;
    Photo.Tags.CommaText := edPhotoTags.Text;
    Photo.Latitude := StrToFloat(edLatitude.Text);
    Photo.Longitude := StrToFloat(edLongitude.Text);
    TMSFMXCloudPicasa1.UpdatePhoto(Photo);
  end;
end;

procedure TForm5.btUploadClick(Sender: TObject);
begin
  if (OpenDialog1.Execute) and Assigned(lvAlbums.Selected) then
  begin
    TMSFMXCloudPicasa1.UploadPhoto(TMSFMXCloudPicasa1.Albums[lvAlbums.ItemIndex],
      OpenDialog1.FileName, edPhotoDescription.Text, edPhotoTags.Text,
      StrToFloat(StringReplace(edLatitude.Text, '.', FormatSettings.DecimalSeparator, [rfIgnoreCase])),
      StrToFloat(StringReplace(edLongitude.Text, '.', FormatSettings.DecimalSeparator, [rfIgnoreCase])));
  end;
end;

procedure TForm5.btUploadFolderClick(Sender: TObject);
begin
  if edAlbumTitle.Text = '' then
  begin
    ShowMessage('No title set for album');
    Exit;
  end;

  if (OpenDialog1.Execute) then
  begin
    TMSFMXCloudPicasa1.AddFolderToAlbum(OpenDialog1.FileName, edAlbumTitle.Text, edAlbumDescription.Text);
    TMSFMXCloudPicasa1.GetAlbums;
  end;
end;

procedure TForm5.FillComments(Photo: TPicasaPhoto);
var
  I: integer;
  li: TListItem;
begin
  lvComments.Items.Clear;
  if Photo.Comments.Count > 0 then
  begin
    for I := 0 to Photo.Comments.Count - 1 do
    begin
      li := lvComments.Items.Add;
      li.Data := Photo.Comments[i];
      li.Text  := Photo.Comments[i].Author.FullName;
      li.SubItems.Add(Photo.Comments[i].Text);
    end;
  end
  else
    ShowMessage('There are no comments for this photo.');
end;

procedure TForm5.FillPhotos(Photos: TPicasaPhotos);
var
  I: integer;
  li: TListItem;
begin
  if Photos.Count > 0 then
  begin
    lbPhotoCount.Text  := 'Results: '
      + IntToStr((PageIndex * 10) + 1)
      + ' to ' + IntToStr((PageIndex * 10) + Photos.Count);
  end
  else
    lbPhotoCount.Text  := 'Results: 0';

  lvPhotos.Items.Clear;
  TMSFMXCloudCloudImage1.URL := '';
  TMSFMXCloudCloudImage2.URL := '';
  lbAuthor.Text  := '';

  for I := 0 to Photos.Count - 1 do
  begin
    li := lvPhotos.Items.Add;
    li.Data := Photos[i];
    li.Text  := Photos[i].FileName;
    li.SubItems.Add(Photos[i].Summary);
  end;
end;

procedure TForm5.FormCreate(Sender: TObject);
begin
  ToggleControls;
  TMSFMXCloudPicasa1.Albums.Clear;
  btPrev.Enabled := false;
  btNext.Enabled := false;
  lvAlbums.ColumnByIndex(1).Width := 300;
  lvPhotos.ColumnByIndex(1).Width := 300;
  lvComments.ColumnByIndex(1).Width := 180;
end;

procedure TForm5.GetAlbums;
var
  I: Integer;
  li: TListItem;
begin
  TMSFMXCloudPicasa1.Albums.Clear;
  TMSFMXCloudPicasa1.GetAlbums;

  lvAlbums.Items.Clear;
  for I := 0 to TMSFMXCloudPicasa1.Albums.Count - 1 do
  begin
    li := lvAlbums.Items.Add;
    li.Data := TMSFMXCloudPicasa1.Albums[i];
    li.Text  := TMSFMXCloudPicasa1.Albums[i].Title;
    li.SubItems.Add(TMSFMXCloudPicasa1.Albums[i].Summary);
  end;
end;

procedure TForm5.GetPhotos;
begin
  if lvAlbums.ItemIndex >= 0 then
  begin
    TMSFMXCloudPicasa1.Albums[lvAlbums.ItemIndex].Photos.Clear;
    TMSFMXCloudPicasa1.Albums[lvAlbums.ItemIndex].GetPhotos;
    FillPhotos(TMSFMXCloudPicasa1.Albums[lvAlbums.ItemIndex].Photos);
  end;
end;

procedure TForm5.lvAlbumsChange(Sender: TObject);
begin
  if Assigned(lvAlbums.Selected) then
  begin
    edAlbumTitle.Text := TMSFMXCloudPicasa1.Albums[lvAlbums.ItemIndex].Title;
    edAlbumDescription.Text := TMSFMXCloudPicasa1.Albums[lvAlbums.ItemIndex].Summary;
    TMSFMXCloudCloudImage2.URL := '';
    edPhotoDescription.Text := '';
    edPhotoTags.Text := '';
    GetPhotos;
    TMSFMXCloudCloudImage1.URL := TMSFMXCloudPicasa1.Albums[lvAlbums.ItemIndex].ImageURL;
  end;
end;

procedure TForm5.lvPhotosChange(Sender: TObject);
var
  pic: TPicasaPhoto;
begin
  if lvPhotos.ItemIndex >= 0 then
  begin
    pic := TPicasaPhoto(lvPhotos.Selected.Data);
    edPhotoDescription.Text := pic.Summary;
    edPhotoTags.Text := pic.Tags.CommaText;
    edLatitude.Text := FloatToStr(pic.Latitude);
    edLongitude.Text := FloatToStr(pic.Longitude);
    TMSFMXCloudCloudImage1.URL := '';
    TMSFMXCloudCloudImage2.URL := '';
    TMSFMXCloudCloudImage1.URL := pic.ImageURL;
    TMSFMXCloudCloudImage2.URL := pic.ThumbnailURL;
    lbAuthor.Text  := 'Author: ' + pic.Author.NickName;
    lvComments.Items.Clear;
  end;
end;

procedure TForm5.btDeleteAlbumClick(Sender: TObject);
var
  buttonSelected: integer;
begin
  if Assigned(lvAlbums.Selected) then
  begin
    buttonSelected := MessageDlg('Are you sure you want to delete the selected album?', TMsgDlgType.mtConfirmation, mbOKCancel, 0);

    if buttonSelected = mrOk then
    begin
      lvPhotos.Items.Clear;
      TMSFMXCloudCloudImage1.URL := '';
      TMSFMXCloudCloudImage2.URL := '';
      TMSFMXCloudPicasa1.DeleteAlbum(TMSFMXCloudPicasa1.Albums[lvAlbums.ItemIndex]);
      GetAlbums;
    end;
  end
  else
  begin
    ShowMessage('No album selected.');
  end;
end;

procedure TForm5.btDeletePhotoClick(Sender: TObject);
var
  buttonSelected: integer;
begin
  if Assigned(lvAlbums.Selected) and Assigned(lvPhotos.Selected) then
  begin
    buttonSelected := MessageDlg('Are you sure you want to delete the selected photo?', TMsgDlgType.mtConfirmation, mbOKCancel, 0);

    if buttonSelected = mrOk then
    begin
      TMSFMXCloudCloudImage1.URL := '';
      TMSFMXCloudCloudImage2.URL := '';
      TMSFMXCloudPicasa1.DeletePhoto(TMSFMXCloudPicasa1.Albums[lvAlbums.ItemIndex].Photos[lvPhotos.ItemIndex]);
      GetPhotos;
    end;
  end
  else
  begin
    ShowMessage('No photo selected.');
  end;
end;

procedure TForm5.btDownloadClick(Sender: TObject);
var
  ph: TPicasaPhoto;
begin
  if Assigned(lvPhotos.Selected) then
  begin
    ph := TPicasaPhoto(lvPhotos.Selected.Data);
    SaveDialog1.FileName := ph.FileName;
    if (SaveDialog1.Execute) then
      TMSFMXCloudPicasa1.DownloadPhoto(SaveDialog1.FileName, ph);
  end;
end;

procedure TForm5.btNextClick(Sender: TObject);
begin
  PageIndex := PageIndex + 1;
  btPrev.Enabled := true;
  DoSearch;
end;

procedure TForm5.btPrevClick(Sender: TObject);
begin
  if PageIndex > 0 then
  begin
    PageIndex := PageIndex - 1;
    if PageIndex = 0 then
      btPrev.Enabled := false;
    DoSearch;
  end
end;

procedure TForm5.btRemoveClick(Sender: TObject);
begin
  TMSFMXCloudPicasa1.ClearTokens;
  Connected := false;
  ToggleControls;
end;

procedure TForm5.btSearchClick(Sender: TObject);
begin
  if edSearch.Text <> '' then
  begin
    PageIndex := 0;
    btPrev.Enabled := false;
    DoSearch;
  end;
end;

procedure TForm5.DoSearch;
var
  EnableNextButton: boolean;
begin
  TMSFMXCloudPicasa1.SearchPhotos(edSearch.Text, EnableNextButton, PageIndex);
  FillPhotos(TMSFMXCloudPicasa1.SearchResults);
  btNext.Enabled := EnableNextButton;
end;


procedure TForm5.ToggleControls;
begin
  btConnect.Enabled := not connected;
  btRemove.Enabled := connected;
  btUpload.Enabled := connected;
  btDeletePhoto.Enabled := connected;
  btUpdatePhoto.Enabled := connected;
  btDownload.Enabled := connected;
  btSearch.Enabled := connected;
  btAlbums.Enabled := connected;
  btCreateAlbum.Enabled := connected;
  btDeleteAlbum.Enabled := connected;
  btUploadFolder.Enabled := connected;
  btComments.Enabled := connected;
  edPhotoDescription.Enabled := connected;
  edPhotoTags.Enabled := connected;
  edLatitude.Enabled := connected;
  edLongitude.Enabled := connected;
  edSearch.Enabled := connected;
  edAlbumDescription.Enabled := connected;
  edAlbumTitle.Enabled := connected;
  lvPhotos.Enabled := connected;
  lvAlbums.Enabled := connected;
  lvComments.Enabled := connected;
end;

end.
