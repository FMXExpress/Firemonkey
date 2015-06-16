unit UCloudFlickrDemo;

interface

uses

  FMX.Forms, SysUtils, FMX.TMSCloudBase, FMX.TMSCloudFlickr, FMX.TMSCloudGoogleLookup,
  FMX.Dialogs, FMX.Objects, FMX.TMSCloudImage, FMX.Memo, FMX.ListBox, FMX.Grid,
  FMX.Layouts, FMX.TMSCloudListView, FMX.Edit, FMX.StdCtrls, FMX.Controls,
  System.Classes, FMX.Types, UITypes, Math, FMX.TMSCloudBaseFMX,
  FMX.TMSCloudCustomFlickr;

type
  TForm816 = class(TForm)
    Button1: TButton;
    Panel2: TPanel;
    Button3: TButton;
    CloudListView1: TTMSFMXCloudListView;
    OpenDialog1: TOpenDialog;
    CloudListView2: TTMSFMXCloudListView;
    SaveDialog1: TSaveDialog;
    Button6: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Edit2: TEdit;
    Label2: TLabel;
    Button8: TButton;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    ComboBox1: TComboBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Edit3: TEdit;
    Edit4: TEdit;
    Label6: TLabel;
    SpeedButton5: TSpeedButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Image1: TImage;
    TMSFMXCloudCloudImage1: TTMSFMXCloudImage;
    ListBox1: TListBox;
    Label7: TLabel;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    ProgressBar1: TProgressBar;
    SpeedButton8: TSpeedButton;
    Memo1: TMemo;
    TMSFMXCloudFlickr1: TTMSFMXCloudFlickr;
    Edit5: TEdit;
    TMSFMXCloudGoogleLocationLookupProvider1: TTMSFMXCloudGoogleLocationLookupProvider;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure CloudListView1Click(Sender: TObject);
    procedure CloudListView2Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
    procedure TMSFMXCloudFlickr1DownloadProgress(Sender: TObject; FileName: string;
      Position, Total: Int64);
    procedure TMSFMXCloudFlickr1ReceivedAccessToken(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ToggleControls;
    procedure AddSetToList(ASet: TFlickrSet);
    procedure AddPhotoToList(APhoto: TFlickrPhoto);
    procedure AddPhotoStreamToList(APhotoStream: TFlickrPhoto);
    procedure AutoLoad;
  end;

var
  Form816: TForm816;
  TMSFMXCloudFlickr1: TTMSFMXCloudFlickr;
  connected: Boolean;

implementation

{$R *.FMX}

// PLEASE USE A VALID INCLUDE FILE THAT CONTAINS THE APPLICATION KEY & SECRET
// FOR THE CLOUD STORAGE SERVICES YOU WANT TO USE
// STRUCTURE OF THIS .INC FILE SHOULD BE
//
// const
//  FlickrAppkey = 'xxxxxxxxx';
//  FlickrAppSecret = 'yyyyyyyy';
//  GoogleLookupAppKey = 'xxxxxxxxx';

{$I APPIDS.INC}

procedure TForm816.SpeedButton6Click(Sender: TObject);
var
  str: string;
begin
  str := InputBox('Add a new tag ?', '', '');
  if str <> '' then
    ListBox1.Items.Add(str);
end;

procedure TForm816.SpeedButton7Click(Sender: TObject);
begin
  if ListBox1.ItemIndex <> -1 then
  begin
    if MessageDlg('Delete selected tag ?', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes then
      ListBox1.Items.Delete(ListBox1.ItemIndex);
  end;
end;

procedure TForm816.AddSetToList(ASet: TFlickrSet);
var
  li: TListItem;
begin
  li := CloudListView1.Items.Add;
  li.Data := ASet;
  li.Text  := ASet.Title;
  li.SubItems.Add(ASet.Description);
end;

procedure TForm816.TMSFMXCloudFlickr1DownloadProgress(Sender: TObject; FileName: string;
  Position, Total: Int64);
begin
  ProgressBar1.Max := Total;
  ProgressBar1.Value := Position;
  if ProgressBar1.Value = ProgressBar1.Max then
    ProgressBar1.Value := 0;

  ProgressBar1.Visible := ProgressBar1.Value > 0;
end;

procedure TForm816.TMSFMXCloudFlickr1ReceivedAccessToken(Sender: TObject);
begin
  TMSFMXCloudFlickr1.SaveTokens;
  TMSFMXCloudFlickr1.GetAccountInfo;
  Connected := true;
  ToggleControls;
  AutoLoad;
end;

procedure TForm816.AutoLoad;
begin
  Button2Click(Self);
  Button4Click(Self);
end;

procedure TForm816.AddPhotoStreamToList(APhotoStream: TFlickrPhoto);
begin
  ComboBox1.Items.AddObject(APhotoStream.Title, APhotoStream);
end;

procedure TForm816.AddPhotoToList(APhoto: TFlickrPhoto);
var
  li: TListItem;
  I: Integer;
  str: String;
begin
  li := CloudListView2.Items.Add;
  li.Data := APhoto;
  li.Text  := APhoto.Title;
  li.SubItems.Add(APhoto.Description);
  li.SubItems.Add(floattostr(APhoto.Latitude) + ' ' + floattostr(APhoto.Longitude));
  str := '';
  for I := 0 to APhoto.Tags.Count - 1 do
  begin
    str := str + APhoto.Tags[I].Value;
    if I < APhoto.Tags.Count - 1 then
      str := str + ';';
  end;
  li.SubItems.Add(str);
end;

procedure TForm816.Button10Click(Sender: TObject);
begin
  if ComboBox1.ItemIndex <> -1 then
  begin
    if MessageDlg('Delete selected photo from photostream ?', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes then
    begin
      if TMSFMXCloudFlickr1.RemovePhoto(TFlickrPhoto(ComboBox1.Items.Objects[ComboBox1.ItemIndex]).ID) then
      begin
        ComboBox1.Items.Delete(ComboBox1.ItemIndex);
        ComboBox1.Repaint;
      end;

      button6.Enabled := False;
      Button2Click(Self);
    end;
  end;
end;

procedure TForm816.Button11Click(Sender: TObject);
var
  ph: TFlickrPhoto;
begin
  if (OpenDialog1.Execute) then
  begin
    ph := TMSFMXCloudFlickr1.UploadPhotoToStream(OpenDialog1.FileName);
    if Assigned(ph) then
    begin
      AddPhotoStreamToList(ph);
      ComboBox1.ItemIndex := ComboBox1.Items.Count - 1;
      ComboBox1Change(Self);
    end;
  end;
end;

procedure TForm816.Button12Click(Sender: TObject);
begin
  if Assigned(CloudListView1.Selected) then
  begin
    if MessageDlg('Delete selected set ?', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes then
    begin
      if TMSFMXCloudFlickr1.RemoveSet(TFlickrSet(CloudListView1.Selected.Data).ID) then
        CloudListView1.Items.Delete(CloudListView1.Selected.Index);

      CloudListView2.Items.Clear;
      Button8.Text  := 'Upload Photo to';
      Button8.Enabled := False;
    end;
  end;
end;

procedure TForm816.Button13Click(Sender: TObject);
var
  ph: TFlickrPhoto;
begin
  if ComboBox1.ItemIndex <> -1 then
  begin
    if Assigned(CloudListView1.Selected) then
    begin
      ph := TFlickrPhoto(ComboBox1.Items.Objects[ComboBox1.ItemIndex]);
      TFlickrSet(CloudListView1.Selected.Data).AddPhoto(ph);
      AddPhotoToList(ph);
    end;
  end;
end;

procedure TForm816.Button14Click(Sender: TObject);
var
  ph: TFlickrPhoto;
begin
  if Assigned(CloudListView2.Selected) then
  begin
    ph := CloudListView2.Selected.Data;
    ph.Latitude := RandomRange(-90, 90);
    ph.Longitude := RandomRange(-180, 180);
    if ph.SetGeoLocation then
      CloudListView2.Selected.SubItems[1] := floattostr(ph.Latitude) + ' ' + floattostr(ph.Longitude);
  end;
end;

procedure TForm816.Button1Click(Sender: TObject);
var
  acc: boolean;
begin
  TMSFMXCloudFlickr1.App.Key := FlickrAppkey;
  TMSFMXCloudFlickr1.App.Secret := FlickrAppSecret;

  if TMSFMXCloudFlickr1.App.Key <> '' then
  begin
    TMSFMXCloudFlickr1.PersistTokens.Key := ExtractFilePath(ParamStr(0)) + 'flickr.ini';
    TMSFMXCloudFlickr1.PersistTokens.Section := 'tokens';
    TMSFMXCloudFlickr1.LoadTokens;

    acc := TMSFMXCloudFlickr1.TestTokens;

    if not acc then
    begin
      TMSFMXCloudFlickr1.RefreshAccess;
      TMSFMXCloudFlickr1.DoAuth;
    end
    else
    begin
      connected := true;
      ToggleControls;
      AutoLoad;
    end;
  end
  else
    ShowMessage('Please provide a valid application ID for the client component');
end;

procedure TForm816.Button2Click(Sender: TObject);
var
  I: Integer;
  fg: TFlickrSet;
begin
  CloudListView1.Items.Clear;
  if TMSFMXCloudFlickr1.GetSets then
  begin
    for I := 0 to TMSFMXCloudFlickr1.Sets.Count - 1 do
    begin
      fg := TMSFMXCloudFlickr1.Sets[I];
      AddSetToList(fg);
    end;
  end;
end;

procedure TForm816.Button3Click(Sender: TObject);
begin
  TMSFMXCloudCloudImage1.URL := '';
  CloudListView1.Items.Clear;
  CloudListView2.Items.Clear;
  ComboBox1.Items.Clear;
  TMSFMXCloudFlickr1.ClearTokens;
  connected := False;
  ToggleControls;
end;

procedure TForm816.Button4Click(Sender: TObject);
var
  I: Integer;
  fg: TFlickrPhoto;
begin
  ComboBox1.Items.Clear;
  if TMSFMXCloudFlickr1.GetPhotoStream then
  begin
    for I := 0 to TMSFMXCloudFlickr1.PhotoStream.Count - 1 do
    begin
      fg := TMSFMXCloudFlickr1.PhotoStream[I];
      AddPhotoStreamToList(fg);
    end;

    ComboBox1.ItemIndex := 0;
    if ComboBox1.ItemIndex <> -1 then
    begin
      Button6.Enabled := True;
      SpeedButton4.Enabled := True;
    end;
  end;
end;

procedure TForm816.Button5Click(Sender: TObject);
var
  ph: TFlickrPhoto;
begin
  if (SaveDialog1.Execute) and Assigned(CloudListView2.Selected) then
  begin
    ph := TFlickrPhoto(CloudListView2.Selected.Data);
    ph.DownloadLargest(SaveDialog1.FileName);
  end;
end;

procedure TForm816.Button6Click(Sender: TObject);
var
  fg: TFlickrSet;
begin
  if ComboBox1.ItemIndex <> -1 then
  begin
    fg := TMSFMXCloudFlickr1.CreateSet(Edit1.Text, Edit2.Text, TFlickrPhoto(ComboBox1.Items.Objects[ComboBox1.ItemIndex]).ID);
    if Assigned(fg) then
      AddSetToList(fg);
  end
  else
    ShowMessage('Please select / upload a photo from / to the photostream');
end;

procedure TForm816.Button7Click(Sender: TObject);
var
  ph: TFlickrPhoto;
  str: String;
begin
  if Assigned(CloudListView2.Selected) then
  begin
    if MessageDlg('Delete selected photo from set ?', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes then
    begin
      ph := TFlickrPhoto(CloudListView2.Selected.Data);
      str := ph.Title;
      if TMSFMXCloudFlickr1.RemovePhoto(ph.ID) then
      begin
        CloudListView2.Items.Delete(CloudListView2.Selected.Index);
        Button4Click(Self);
        if CloudListView2.Items.Count = 0 then
        begin
          //remove set (set does not exist when photo's are empty
          if Assigned(CloudListView1.Selected) then
          begin
            TMSFMXCloudFlickr1.Sets.Delete(TFlickrSet(CloudListView1.Selected.Data).Index);
            CloudListView1.Items.Delete(CloudListView1.Selected.Index);
          end;
        end;
      end;
    end;
  end;
end;

procedure TForm816.Button8Click(Sender: TObject);
var
  fg: TFlickrSet;
  ph: TFlickrPhoto;
  geo: TGeoLocation;
  I: Integer;
begin
  if (OpenDialog1.Execute) and Assigned(CloudListView1.Selected) then
  begin
    fg := TFlickrSet(CloudListView1.Selected.Data);
    ph := fg.AddAndUploadPhoto(OpenDialog1.FileName, Edit3.Text, Edit4.Text);
    if Assigned(ph) then
    begin
      ph.Title := Edit3.Text;
      ph.Description := Edit4.Text;
      //set title & description
      ph.SetMeta;
      ph.Tags.Clear;
      for I := 0 to ListBox1.Items.Count - 1 do
        ph.Tags.Add.Value := ListBox1.Items[I];

      //set tags
      ph.SetTags;

      if Edit5.Text <> '' then
      begin
        geo := TMSFMXCloudGoogleLocationLookupProvider1.GetGeoLocation(Edit5.Text);
        ph.Latitude := geo.Latitude;
        ph.Longitude := geo.Longitude;
        //set geo
        ph.SetGeoLocation;
      end;
      AddPhotoToList(ph);
      AddPhotoStreamToList(ph);
    end;
  end;
end;

procedure TForm816.ComboBox1Change(Sender: TObject);
begin
  if ComboBox1.ItemIndex <> -1 then
  begin
    SpeedButton4.Enabled := True;
    Button6.Enabled := True;
  end;
end;

procedure TForm816.FormCreate(Sender: TObject);
begin
  TMSFMXCloudGoogleLocationLookupProvider1.App.Key := GoogleLookupAppKey;
  connected := False;
  ToggleControls;
  CloudListView1.ColumnByIndex(1).Width := 280;
end;

procedure TForm816.CloudListView1Click(Sender: TObject);
var
  fg: TFlickrSet;
  I: Integer;
  ph: TFlickrPhoto;
begin
  SpeedButton1.Enabled := False;
  CloudListView2.Items.Clear;
  if Assigned(CloudListView1.Selected) then
  begin
    fg := TFlickrSet(CloudListView1.Selected.Data);
    Button8.Enabled := True;
    Edit3.Enabled := True;
    Edit4.Enabled := True;
    Edit5.Enabled := True;
    Button8.Text  := 'Upload Photo to ' + fg.Title;
    SpeedButton5.Enabled := True;
    if Assigned(fg) then
    begin
      if fg.GetPhotos then
      begin
        for I := 0 to fg.Photos.Count - 1 do
        begin
          ph := fg.Photos[I];
          ph.GetGeoLocation;
          ph.GetInfo;
          ph.GetTags;
          AddPhotoToList(ph);
        end;
      end;
    end;
  end;
end;

procedure TForm816.CloudListView2Click(Sender: TObject);
var
  ph: TFlickrPhoto;
  i: integer;
begin
  Memo1.Lines.Clear;
  TMSFMXCloudCloudImage1.URL := '';
  if Assigned(CloudListView2.Selected) then
  begin
    SpeedButton2.Enabled := True;
    SpeedButton1.Enabled := True;
    SpeedButton8.Enabled := True;

    ph := TFlickrPhoto(CloudListView2.Selected.Data);
    if Assigned(ph) then
    begin
      if ph.GetSizes then
        TMSFMXCloudCloudImage1.URL := ph.Sizes[ph.Sizes.Count - 1].DownloadURL;

      if ph.GetComments then
      begin
        for i := 0 to ph.Comments.Count - 1 do
          Memo1.Lines.Add(ph.Comments[I].Author + ' : ' + ph.Comments[I].Value);
      end;
    end;
  end;
end;

procedure TForm816.ToggleControls;
begin
  Button1.Enabled := not connected;
  Button3.Enabled := connected;
  CloudListView1.Enabled := connected;
  CloudListView2.Enabled := connected;
  SpeedButton3.Enabled := connected;
  Edit1.Enabled := connected;
  Edit2.Enabled := connected;
  ComboBox1.Enabled := connected;
end;

end.
