unit UFaceBookAlbumDemo;

interface

uses
  FMX.Forms, FMX.TMSCloudBase, SysUtils, FMX.TMSCloudFacebook, FMX.Controls, FMX.Dialogs,
  FMX.Edit, FMX.Grid, FMX.Layouts, FMX.TMSCloudListView, FMX.Objects,
  FMX.TMSCloudImage, FMX.StdCtrls, System.Classes, FMX.Types,
  FMX.TMSCloudBaseFMX, FMX.TMSCloudCustomFacebook;

type
  TForm1 = class(TForm)
    StyleBook1: TStyleBook;
    TMSFMXCloudFacebook1: TTMSFMXCloudFacebook;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Button1: TButton;
    GroupBox3: TGroupBox;
    SpeedButton2: TSpeedButton;
    Label1: TLabel;
    TMSFMXCloudCloudImage1: TTMSFMXCloudImage;
    CloudListView1: TTMSFMXCloudListView;
    CloudListView2: TTMSFMXCloudListView;
    btAdd: TButton;
    edAdd: TEdit;
    Image1: TImage;
    btRemove: TButton;
    procedure Button1Click(Sender: TObject);
    procedure ToggleControls;
    procedure LoadAlbums;
    procedure LoadPictures;
    procedure Init;
    procedure TMSFMXCloudFacebook1ReceivedAccessToken(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btAddClick(Sender: TObject);
    procedure btRemoveClick(Sender: TObject);
    procedure CloudListView2Change(Sender: TObject);
    procedure CloudListView1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Connected: boolean;
    LVInit: boolean;
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

procedure TForm1.btAddClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
     TMSFMXCloudFacebook1.PostImage(edAdd.Text, OpenDialog1.FileName, CloudListView2.Items[CloudListView2.ItemIndex].Data);
     LoadPictures;
  end;
end;

procedure TForm1.btRemoveClick(Sender: TObject);
begin
  TMSFMXCloudFacebook1.ClearTokens;
  Connected := false;
  ToggleControls;
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
      TMSFMXCloudFacebook1.DoAuth;
    end
    else
      Init;
  end
  else
    ShowMessage('Please provide a valid application ID for the client component');
end;

procedure TForm1.CloudListView1Change(Sender: TObject);
var
  Picture: TFacebookPicture;
begin
  if LVInit then
   exit;

  if CloudListView1.ItemIndex >= 0 then
  begin
    Picture := CloudListView1.Items[CloudListView1.ItemIndex].Data;
    TMSFMXCloudCloudImage1.URL := Picture.ImageURL;
  end;
end;

procedure TForm1.CloudListView2Change(Sender: TObject);
begin
  if LVInit then
    exit;

  LoadPictures;
end;

procedure TForm1.TMSFMXCloudFacebook1ReceivedAccessToken(Sender: TObject);
begin
  TMSFMXCloudFacebook1.SaveTokens;
  Init;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  LVInit := false;
  Connected := false;
  ToggleControls;
end;

procedure TForm1.Init;
begin
  Connected := true;
  ToggleControls;
  TMSFMXCloudFacebook1.GetUserInfo;
  LoadAlbums;
end;



procedure TForm1.LoadAlbums;
var
  I: integer;
  Profile: TFacebookProfile;
  li: TListItem;
begin
  LVInit := true;
  TMSFMXCloudFacebook1.GetAlbums(TMSFMXCloudFacebook1.Profile);
  CloudListView2.Items.Clear;

  Profile := TMSFMXCloudFacebook1.Profile;
  for I := 0 to Profile.Albums.Count - 1 do
  begin
    li := CloudListView2.Items.Add;
    li.Text  := Profile.Albums[I].Title;
    li.SubItems.Add(DateToStr(Profile.Albums[I].UpdatedTime));
    li.Data := Profile.Albums[I];
  end;
  LVInit := false;
end;

procedure TForm1.LoadPictures;
var
  AlbumItem: TFacebookAlbum;
  li: TListItem;
  I: Integer;
begin
  if CloudListView2.ItemIndex >= 0 then
  begin
    LVInit := true;
    edAdd.Enabled := true;
    btAdd.Enabled := true;

    AlbumItem := CloudListView2.Items[CloudListView2.ItemIndex].Data;

    TMSFMXCloudCloudImage1.URL := TMSFMXCloudFacebook1.GetImageURL(AlbumItem.CoverPhotoID);

    CloudListView1.Items.Clear;
    TMSFMXCloudFacebook1.GetPictures(AlbumItem);

    for I := 0 to AlbumItem.Pictures.Count - 1 do
    begin
      li := CloudListView1.Items.Add;
      li.Text  := AlbumItem.Pictures[I].Caption;
      if li.Text  = '' then
        li.Text  := 'No Description';
      li.SubItems.Add(DateToStr(AlbumItem.Pictures[I].CreatedTime));
      li.Data := AlbumItem.Pictures[I];
    end;
    LVInit := false;
  end;
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
begin
  LoadAlbums;
end;

procedure TForm1.ToggleControls;
begin
  GroupBox3.Enabled := Connected;
  SpeedButton2.Enabled := Connected;
  CloudListView2.Enabled := Connected;
  CloudListView1.Enabled := Connected;
  btRemove.Enabled := Connected;
  Button1.Enabled := not Connected;
end;


end.
