unit UYoutubeDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.TMSCloudBase, FMX.TMSCloudBaseFMX, FMX.TMSCloudCustomGoogle,
  FMX.TMSCloudGoogleFMX, FMX.TMSCloudCustomYouTube, FMX.TMSCloudYouTube,
  FMX.ListBox, FMX.Layouts, FMX.Grid, FMX.TMSCloudListView, FMX.Edit,
  FMX.Objects, FMX.TMSCloudImage;

type
  TForm1171 = class(TForm)
    TMSFMXCloudYouTube1: TTMSFMXCloudYouTube;
    Panel1: TPanel;
    btConnect: TButton;
    btRemove: TButton;
    btGetVideos: TButton;
    btNext: TButton;
    lvVideos: TTMSFMXCloudListView;
    cbPageSize: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    lbLink: TLabel;
    Label4: TLabel;
    btSetRating: TButton;
    btDelete: TButton;
    cbRating: TComboBox;
    edTitle: TEdit;
    edDescription: TEdit;
    OpenDialog1: TOpenDialog;
    Panel2: TPanel;
    btUpload: TButton;
    btUpdate: TButton;
    TMSFMXCloudImage1: TTMSFMXCloudImage;
    procedure btConnectClick(Sender: TObject);
    procedure TMSFMXCloudYouTube1AfterAddVideo(Sender: TObject;
      AFileName: string; AVideo: TYouTubeVideo);
    procedure TMSFMXCloudYouTube1ReceivedAccessToken(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btGetVideosClick(Sender: TObject);
    procedure btNextClick(Sender: TObject);
    procedure lvVideosChange(Sender: TObject);
    procedure btDeleteClick(Sender: TObject);
    procedure btSetRatingClick(Sender: TObject);
    procedure btUpdateClick(Sender: TObject);
    procedure btUploadClick(Sender: TObject);
    procedure cbPageSizeChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Connected: boolean;
    InitLV: boolean;
    procedure LoadVideos(First: boolean = true);
    procedure FillVideos;
    procedure ToggleControls;
  end;

var
  Form1171: TForm1171;

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

procedure TForm1171.btUpdateClick(Sender: TObject);
var
  v: TYouTubeVideo;
begin
  if lvVideos.ItemIndex >= 0 then
  begin
    v := TMSFMXCloudYouTube1.Videos[lvVideos.ItemIndex];
    v.Title := edTitle.Text;
    v.Description := edDescription.Text;
    TMSFMXCloudYouTube1.UpdateVideo(v);
  end;
end;

procedure TForm1171.btUploadClick(Sender: TObject);
begin
  if edTitle.Text <> '' then
  begin
    if OpenDialog1.Execute then
    begin
       TMSFMXCloudYouTube1.UploadVideo(OpenDialog1.FileName, edTitle.Text, edDescription.Text);
    end;
  end
  else
    ShowMessage('Please enter a title for the video.');
end;

procedure TForm1171.cbPageSizeChange(Sender: TObject);
begin
  LoadVideos(true);
end;

procedure TForm1171.btConnectClick(Sender: TObject);
var
  acc: boolean;
begin
  TMSFMXCloudYouTube1.App.Key := GAppKey;
  TMSFMXCloudYouTube1.App.Secret := GAppSecret;

  if TMSFMXCloudYouTube1.App.Key <> '' then
  begin
    TMSFMXCloudYouTube1.PersistTokens.Location := plIniFile;
    TMSFMXCloudYouTube1.PersistTokens.Key := '.\youtube.ini';
    TMSFMXCloudYouTube1.PersistTokens.Section := 'tokens';
    TMSFMXCloudYouTube1.LoadTokens;

    acc := TMSFMXCloudYouTube1.TestTokens;

    if not acc then
    begin
      TMSFMXCloudYouTube1.RefreshAccess;
      acc := TMSFMXCloudYouTube1.TestTokens;
    end;

    if not acc then
    begin
        TMSFMXCloudYouTube1.DoAuth;
    end
    else
    begin
      Connected := true;
      ToggleControls;
      LoadVideos;
    end;
  end
  else
    ShowMessage('Please provide a valid application ID for the client component');
end;

procedure TForm1171.btGetVideosClick(Sender: TObject);
begin
  LoadVideos( (Sender as TButton).Tag = 0);
end;

procedure TForm1171.btNextClick(Sender: TObject);
begin
  LoadVideos(false);
end;

procedure TForm1171.btSetRatingClick(Sender: TObject);
var
  Rating: TYouTubeRating;
begin
  Rating := yrUnspecified;

  if lvVideos.ItemIndex >= 0 then
  begin
    if cbRating.ItemIndex = 0 then
      Rating := yrDislike
    else if cbRating.ItemIndex = 1 then
      Rating := yrLike
    else if cbRating.ItemIndex = 2 then
      Rating := yrNone
    else if cbRating.ItemIndex = 3 then
      Rating := yrUnspecified;

    TMSFMXCloudYouTube1.SetRating(TMSFMXCloudYouTube1.Videos[lvVideos.ItemIndex], Rating);
  end;
end;

procedure TForm1171.btDeleteClick(Sender: TObject);
begin
  if lvVideos.ItemIndex >= 0 then
  begin
    if MessageDlg('Are you sure you want to delete the selected video?', TMsgDlgType.mtConfirmation, mbOKCancel, 0) = mrOk then
    begin
      TMSFMXCloudYouTube1.DeleteVideo(TMSFMXCloudYouTube1.Videos[lvVideos.ItemIndex]);
      FillVideos;
    end;
  end
  else
  begin
    ShowMessage('Please select a video first.');
  end;
end;

procedure TForm1171.FillVideos;
var
  I: integer;
  li: TListItem;
begin
  lbLink.Text := '';
  TMSFMXCloudImage1.URL := '';
  cbRating.ItemIndex := 3;
  InitLV := true;
  lvVideos.Items.Clear;
  for I := 0 to TMSFMXCloudYouTube1.Videos.Count - 1 do
  begin
    li := lvVideos.Items.Add;
    li.Text := IntToStr(i + 1);

    li.SubItems.Add(TMSFMXCloudYouTube1.Videos[I].Title);
    li.SubItems.Add(TMSFMXCloudYouTube1.Videos[I].Description);
    li.SubItems.Add(FormatDateTime('dd/mm/yyyy hh:nn',TMSFMXCloudYouTube1.Videos[I].PublishDate));
    li.Data := TMSFMXCloudYouTube1.Videos[I];
  end;
  InitLV := false;
end;

procedure TForm1171.FormCreate(Sender: TObject);
begin
  ToggleControls;
  lvVideos.ColumnByIndex(2).Width := 450;
end;

procedure TForm1171.LoadVideos(First: boolean);
begin
  if first then
    TMSFMXCloudYouTube1.GetFirstVideos(StrToInt(cbPageSize.Items[cbPageSize.ItemIndex]))
  else
    TMSFMXCloudYouTube1.GetNextVideos(StrToInt(cbPageSize.Items[cbPageSize.ItemIndex]));
  FillVideos;
end;

procedure TForm1171.lvVideosChange(Sender: TObject);
var
  v: TYouTubeVideo;
begin
  if lvVideos.ItemIndex >= 0 then
  begin
    if InitLV then
      Exit;

    v := TMSFMXCloudYouTube1.Videos[lvVideos.ItemIndex];
    lbLink.Text := v.Link;
    edTitle.Text := v.Title;
    edDescription.Text := v.Description;
    TMSFMXCloudImage1.URL := v.ImageURL;

    TMSFMXCloudYouTube1.GetRating(v);

    if v.Rating = yrDislike then
      cbRating.ItemIndex := 0
    else if v.Rating = yrLike then
      cbRating.ItemIndex := 1
    else if v.Rating = yrNone then
      cbRating.ItemIndex := 2
    else if v.Rating = yrUnspecified then
      cbRating.ItemIndex := 3;
  end;
end;

procedure TForm1171.TMSFMXCloudYouTube1AfterAddVideo(Sender: TObject;
  AFileName: string; AVideo: TYouTubeVideo);
begin
  if Assigned(AVideo) then
    ShowMessage('The video has been uploaded.')
  else
    ShowMessage('Upload failed, please try again.')
end;

procedure TForm1171.TMSFMXCloudYouTube1ReceivedAccessToken(Sender: TObject);
begin
  TMSFMXCloudYouTube1.SaveTokens;
  Connected := true;
  ToggleControls;
  LoadVideos;
end;

procedure TForm1171.ToggleControls;
begin
  btConnect.Enabled := not Connected;
  btRemove.Enabled := Connected;
  btGetVideos.Enabled := Connected;
  lvVideos.Enabled := Connected;
  btDelete.Enabled := Connected;
  lbLink.Enabled := Connected;
  edTitle.Enabled := Connected;
  edDescription.Enabled := Connected;
  btUpload.Enabled := Connected;
  btNext.Enabled := Connected;
  btSetRating.Enabled := Connected;
  cbRating.Enabled := Connected;
  cbPageSize.Enabled := Connected;
  btUpdate.Enabled := Connected;
end;

end.
