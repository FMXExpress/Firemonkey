unit UPicasaDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TMSCloudBase, FMX.TMSCloudPicasa,
  FMX.Layouts, FMX.ListBox, FMX.Objects, FMX.TMSCloudImage, FMX.TMSCloudBaseFMX,
  FMX.TMSCloudCustomGoogle, FMX.TMSCloudGoogleFMX, FMX.TMSCloudCustomPicasa, IOUtils;

type
  TForm82 = class(TForm)
    ToolBar1: TToolBar;
    Button1: TButton;
    Button2: TButton;
    TMSFMXCloudPicasa1: TTMSFMXCloudPicasa;
    Button3: TButton;
    lvAlbums: TListBox;
    lvPhotos: TListBox;
    TMSFMXCloudImage1: TTMSFMXCloudImage;
    Label1: TLabel;
    Label2: TLabel;
    Line1: TLine;
    Label3: TLabel;
    Label4: TLabel;
    lblDescription: TLabel;
    Label5: TLabel;
    lblTags: TLabel;
    lblLocation: TLabel;
    Label7: TLabel;
    TMSFMXCloudImage2: TTMSFMXCloudImage;
    Label8: TLabel;
    lblAuthor: TLabel;
    lbComments: TListBox;
    Label6: TLabel;
    SpeedButton8: TSpeedButton;
    Image8: TImage;
    SpeedButton3: TSpeedButton;
    Image3: TImage;
    Rectangle1: TRectangle;
    procedure Button1Click(Sender: TObject);
    procedure TMSFMXCloudPicasa1ReceivedAccessToken(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure lvAlbumsChange(Sender: TObject);
    procedure lvPhotosChange(Sender: TObject);
    procedure lbCommentsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton8Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
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
  Form82: TForm82;

implementation

{$R *.fmx}

// PLEASE USE A VALID INCLUDE FILE THAT CONTAINS THE APPLICATION KEY & SECRET
// FOR THE CLOUD STORAGE SERVICES YOU WANT TO USE
// STRUCTURE OF THIS .INC FILE SHOULD BE
//
// const
//  GAppkey = 'xxxxxxxxx';
//  GAppSecret = 'yyyyyyyy';

{$I APPIDS.INC}

procedure TForm82.Button1Click(Sender: TObject);
var
  acc: boolean;
begin
  TMSFMXCloudPicasa1.App.Key := GAppkey;
  TMSFMXCloudPicasa1.App.Secret := GAppSecret;

  if TMSFMXCloudPicasa1.App.Key <> '' then
  begin
    TMSFMXCloudPicasa1.PersistTokens.Key := TPath.GetDocumentsPath +  '/picasa.ini';
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

procedure TForm82.Button2Click(Sender: TObject);
begin
  TMSFMXCloudPicasa1.ClearTokens;
  Connected := false;
  ToggleControls;
end;

procedure TForm82.Button3Click(Sender: TObject);
begin
  GetAlbums;
end;

procedure TForm82.DoSearch;
begin

end;

procedure TForm82.FillComments(Photo: TPicasaPhoto);
var
  I: integer;
begin
  lbComments.Clear;
  TMSFMXCloudPicasa1.GetComments(Photo);
  lbComments.BeginUpdate;
  if Photo.Comments.Count > 0 then
  begin
    for I := 0 to Photo.Comments.Count - 1 do
      lbComments.Items.AddObject(Photo.Comments[i].Author.FullName, Photo.Comments[i])
  end;
  lbComments.EndUpdate;
end;

procedure TForm82.FillPhotos(Photos: TPicasaPhotos);
var
  I: integer;
begin
  lvPhotos.Items.Clear;
  TMSFMXCloudImage1.URL := '';

  lvPhotos.BeginUpdate;
  for I := 0 to Photos.Count - 1 do
    lvPhotos.Items.AddObject(Photos[I].FileName, Photos[I]);
  lvPhotos.EndUpdate;
end;

procedure TForm82.FormCreate(Sender: TObject);
begin
  {$IFDEF IOS}
  TFile.Copy(ExtractFilePath(Paramstr(0)) + 'sample.jpg', TPath.GetDocumentsPath + '/sample.jpg', True);
  {$ENDIF}
  connected := False;
  ToggleControls;
end;

procedure TForm82.GetAlbums;
var
  I: Integer;
begin
  TMSFMXCloudPicasa1.Albums.Clear;
  TMSFMXCloudPicasa1.GetAlbums;

  lvAlbums.BeginUpdate;
  lvAlbums.Items.Clear;
  for I := 0 to TMSFMXCloudPicasa1.Albums.Count - 1 do
    lvAlbums.Items.AddObject(TMSFMXCloudPicasa1.Albums[i].Title, TMSFMXCloudPicasa1.Albums[i]);
  lvAlbums.EndUpdate;

  lvAlbums.ItemIndex := 0;
  GetPhotos;
end;

procedure TForm82.GetPhotos;
begin
  if lvAlbums.ItemIndex >= 0 then
  begin
    TMSFMXCloudPicasa1.Albums[lvAlbums.ItemIndex].Photos.Clear;
    TMSFMXCloudPicasa1.Albums[lvAlbums.ItemIndex].GetPhotos;
    FillPhotos(TMSFMXCloudPicasa1.Albums[lvAlbums.ItemIndex].Photos);
  end;
end;

procedure TForm82.lbCommentsChange(Sender: TObject);
begin
  if Assigned(lbComments.Selected) then
    ShowMessage(TPicasaComment(lbComments.Selected.Data).Text);
end;

procedure TForm82.lvAlbumsChange(Sender: TObject);
begin
  if Assigned(lvAlbums.Selected) then
    GetPhotos;
end;

procedure TForm82.lvPhotosChange(Sender: TObject);
var
  pic: TPicasaPhoto;
begin
  if Assigned(lvPhotos.Selected.Data) then
  begin
    pic := TPicasaPhoto(lvPhotos.Selected.Data);
    lblDescription.Text := pic.Summary;
    lblTags.Text := pic.Tags.CommaText;
    lblLocation.Text := FloatToStr(pic.Latitude) + ' ' + FloatToStr(pic.Longitude);
    TMSFMXCloudImage1.URL := '';
    TMSFMXCloudImage2.URL := '';
    TMSFMXCloudImage1.URL := pic.ImageURL;
    TMSFMXCloudImage2.URL := pic.ThumbnailURL;
    lblAuthor.Text := 'Author: ' + pic.Author.NickName;
    FillComments(pic);
  end;
end;

procedure TForm82.SpeedButton3Click(Sender: TObject);
begin
  if Assigned(lvAlbums.Selected) then
  begin
    TMSFMXCloudPicasa1.UploadPhoto(TMSFMXCloudPicasa1.Albums[lvAlbums.ItemIndex],
      TPath.GetDocumentsPath + '/sample.jpg', 'test', 'tags');

    ShowMessage('Sample Picture has been uploaded');
    GetAlbums;
  end;
end;

procedure TForm82.SpeedButton8Click(Sender: TObject);
var
  ph: TPicasaPhoto;
begin
  if Assigned(lvPhotos.Selected) then
  begin
    ph := TPicasaPhoto(lvPhotos.Selected.Data);
    TMSFMXCloudPicasa1.DownloadPhoto(TPath.GetDocumentsPath + '/'+ph.FileName, ph);
    ShowMessage('Picture ' + ph.FileName + ' has been downloaded' );
  end;
end;

procedure TForm82.TMSFMXCloudPicasa1ReceivedAccessToken(Sender: TObject);
begin
  TMSFMXCloudPicasa1.SaveTokens;
  Connected := true;
  ToggleControls;
end;

procedure TForm82.ToggleControls;
begin
  SpeedButton8.Enabled := connected;
  SpeedButton3.Enabled := Connected;
  Button1.Enabled := not connected;
  Button2.Enabled := connected;
  Button3.Enabled := connected;
  lvPhotos.Enabled := connected;
  lvAlbums.Enabled := connected;
  lbComments.Enabled := connected;

end;

end.
