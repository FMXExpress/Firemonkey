unit UFaceBookAlbumDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.ListView.Types, FMX.Edit, FMX.ListView, FMX.TMSCloudBase,
  FMX.TMSCloudFacebook, FMX.Layouts, FMX.ListBox, FMX.TMSCloudImage, FMX.Objects,
  FMX.TMSCloudBaseFMX, FMX.TMSCloudCustomFacebook, IOUtils;

type
  TForm82 = class(TForm)
    ToolBar1: TToolBar;
    Button1: TButton;
    btRemove: TButton;
    TMSFMXCloudFaceBook1: TTMSFMXCloudFaceBook;
    Panel1: TPanel;
    btnLoad: TButton;
    ListView1: TListBox;
    btAdd: TButton;
    edAdd: TEdit;
    ListView2: TListBox;
    Label1: TLabel;
    TMSFMXCloudImage1: TTMSFMXCloudImage;
    procedure Button1Click(Sender: TObject);
    procedure btRemoveClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btAddClick(Sender: TObject);
    procedure TMSFMXCloudFaceBook1ReceivedAccessToken(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListView1Change(Sender: TObject);
    procedure ListView2Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Connected: boolean;
    LVInit: boolean;
    procedure ToggleControls;
    procedure LoadAlbums;
    procedure LoadPictures;
    procedure Init;
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
      TMSFMXCloudFacebook1.DoAuth;
    end
    else
      Init;
  end
  else
    ShowMessage('Please provide a valid application ID for the client component');
end;

procedure TForm82.btRemoveClick(Sender: TObject);
begin
  TMSFMXCloudFacebook1.ClearTokens;
  Connected := false;
  ToggleControls;
end;

procedure TForm82.btnLoadClick(Sender: TObject);
begin
  LoadAlbums;
end;

procedure TForm82.btAddClick(Sender: TObject);
var
  fn: string;
begin
  if MessageDlg('Do you want to upload a sample file?', TMsgDlgType.mtConfirmation,[TMsgDlgBtn.mbYes,TMsgDlgBtn.mbNo],0) = mrYes then
  begin
    fn := TPath.GetDocumentsPath + '/sample.jpg';
    TMSFMXCloudFaceBook1.PostImage(edAdd.Text, fn, TFaceBookAlbum(ListView1.Items.Objects[ListView1.ItemIndex]));
    LoadPictures;
  end;
end;

procedure TForm82.FormCreate(Sender: TObject);
begin
  {$IFDEF IOS}
  TFile.Copy(ExtractFilePath(Paramstr(0)) + 'sample.jpg', TPath.GetDocumentsPath + '/sample.jpg', True);
  {$ENDIF}
  LVInit := false;
  Connected := false;
  ToggleControls;
end;

procedure TForm82.Init;
begin
  Connected := true;
  ToggleControls;
  TMSFMXCloudFaceBook1.GetUserInfo;
  LoadAlbums;
end;

procedure TForm82.ListView1Change(Sender: TObject);
begin
  if LVInit then
    exit;

  LoadPictures;
end;

procedure TForm82.ListView2Change(Sender: TObject);
var
  Picture: TFacebookPicture;
begin
  if LVInit then
   exit;

  if ListView2.ItemIndex >= 0 then
  begin
    Picture := TFacebookPicture(ListView2.Items.Objects[ListView2.ItemIndex]);
    TMSFMXCloudImage1.URL := Picture.ImageURL;
  end;
end;


procedure TForm82.LoadAlbums;
var
  I: integer;
  Profile: TFacebookProfile;
begin
  LVInit := true;
  TMSFMXCloudFacebook1.GetAlbums(TMSFMXCloudFacebook1.Profile);
  ListView1.Items.Clear;

  Profile := TMSFMXCloudFacebook1.Profile;
  ListView1.BeginUpdate;
  for I := 0 to Profile.Albums.Count - 1 do
    ListView1.Items.AddObject(Profile.Albums[I].Title, Profile.Albums[I]);
  ListView1.EndUpdate;
  LVInit := false;
end;

procedure TForm82.LoadPictures;
var
  AlbumItem: TFacebookAlbum;
  I: Integer;
  str: String;
begin
  if ListView1.ItemIndex >= 0 then
  begin
    LVInit := true;
    edAdd.Enabled := true;
    btAdd.Enabled := true;

    AlbumItem := TFaceBookAlbum(ListView1.Items.Objects[ListView1.ItemIndex]);

    TMSFMXCloudImage1.URL := TMSFMXCloudFacebook1.GetImageURL(AlbumItem.CoverPhotoID);

    ListView2.Items.Clear;
    TMSFMXCloudFacebook1.GetPictures(AlbumItem);

    ListView2.BeginUpdate;
    for I := 0 to AlbumItem.Pictures.Count - 1 do
    begin
      str := AlbumItem.Pictures[I].Caption;
      if str = '' then
        str := AlbumItem.Pictures[I].ID;

      ListView2.Items.AddObject(str, AlbumItem.Pictures[I]);
    end;
    ListView2.EndUpdate;
    LVInit := false;
  end;
end;

procedure TForm82.TMSFMXCloudFaceBook1ReceivedAccessToken(Sender: TObject);
begin
  TMSFMXCloudFacebook1.SaveTokens;
  Init;
end;

procedure TForm82.ToggleControls;
begin
  Panel1.Enabled := Connected;
  btnLoad.Enabled := Connected;
  ListView2.Enabled := Connected;
  ListView1.Enabled := Connected;
  btRemove.Enabled := Connected;
  Button1.Enabled := not Connected;
end;

end.
