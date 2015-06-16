unit UFlickrDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TMSCloudBase, FMX.TMSCloudFlickr, FMX.TMSCloudImage,
  FMX.ListBox, FMX.Layouts, FMX.Objects, FMX.Edit, FMX.ListView.Types, FMX.Memo,
  FMX.ListView, FMX.TMSCloudGoogleLookup, FMX.TMSCloudBaseFMX,
  FMX.TMSCloudCustomFlickr, iOUtils;

type
  TForm82 = class(TForm)
    ComboBox1: TComboBox;
    SpeedButton4: TSpeedButton;
    Button6: TButton;
    ToolBar1: TToolBar;
    Button1: TButton;
    Button2: TButton;
    TMSFMXCloudFlickr1: TTMSFMXCloudFlickr;
    Panel1: TPanel;
    Panel2: TPanel;
    Line1: TLine;
    ListView1: TListBox;
    ListBox1: TListBox;
    ListView2: TListBox;
    SpeedButton5: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton8: TSpeedButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Button8: Tbutton;
    Edit3: TEdit;
    Edit4: TEdit;
    Memo1: TMemo;
    Edit5: TEdit;
    SpeedButton3: TSpeedbutton;
    TMSFMXCloudGoogleLocationLookupProvider1: TTMSFMXCloudGoogleLocationLookupProvider;
    TMSFMXCloudImage1: TTMSFMXCloudImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    Image7: TImage;
    Image8: TImage;
    Edit6: TEdit;
    procedure TMSFMXCloudFlickr1ReceivedAccessToken(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
    procedure SpeedButton8Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure ListView1Change(Sender: TObject);
    procedure ListView2Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Connected: Boolean;
    procedure ToggleControls;
    procedure AddSetToList(ASet: TFlickrSet);
    procedure AddPhotoToList(APhoto: TFlickrPhoto);
    procedure AddPhotoStreamToList(APhotoStream: TFlickrPhoto);
    procedure AutoLoad;
    procedure LoadSets;
    procedure LoadPhotoStream;
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
//  FlickrAppKey = 'xxxxxxxxx';
//  FlickrAppSecret = 'yyyyyyyy';

{$I APPIDS.INC}

procedure TForm82.AddPhotoStreamToList(APhotoStream: TFlickrPhoto);
begin
  ComboBox1.Items.AddObject(APhotoStream.Title, APhotoStream);
end;

procedure TForm82.AddPhotoToList(APhoto: TFlickrPhoto);
begin
  ListView2.Items.AddObject(APhoto.Title, APhoto);
end;

procedure TForm82.AddSetToList(ASet: TFlickrSet);
begin
  ListView1.Items.AddObject(ASet.Title, ASet);
end;

procedure TForm82.AutoLoad;
begin
  LoadSets;
  LoadPhotoStream;
end;

procedure TForm82.ListView1Change(Sender: TObject);
var
  fg: TFlickrSet;
  I: Integer;
  ph: TFlickrPhoto;
begin
  ListView2.Items.Clear;
  if Assigned(ListView1.Selected) then
  begin
    fg := TFlickrSet(ListView1.Selected.Data);
    Button8.Enabled := True;
    Edit3.Enabled := True;
    Edit4.Enabled := True;
    Edit5.Enabled := True;
    Button8.Text := 'Upload Photo to ' + fg.Title;
    SpeedButton5.Enabled := True;
    ListView2.BeginUpdate;
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
    ListView2.EndUpdate;
  end;
end;

procedure TForm82.ListView2Change(Sender: TObject);
var
  ph: TFlickrPhoto;
  i: integer;
begin
  Memo1.Lines.Clear;
  TMSFMXCloudImage1.URL := '';
  if Assigned(ListView2.Selected) then
  begin
    SpeedButton2.Enabled := True;
//    SpeedButton1.Enabled := True;
    SpeedButton8.Enabled := True;

    ph := TFlickrPhoto(ListView2.Selected.Data);
    if Assigned(ph) then
    begin
      if ph.GetSizes then
        TMSFMXCloudImage1.URL := ph.Sizes[ph.Sizes.Count - 1].DownloadURL;

      if ph.GetComments then
      begin
        for i := 0 to ph.Comments.Count - 1 do
          Memo1.Lines.Add(ph.Comments[I].Author + ' : ' + ph.Comments[I].Value);
      end;
    end;
  end;

end;

procedure TForm82.LoadPhotoStream;
var
  I: Integer;
  fg: TFlickrPhoto;
begin
  ComboBox1.Items.Clear;
  if TMSFMXCloudFlickr1.GetPhotoStream then
  begin
    ComboBox1.BeginUpdate;
    for I := 0 to TMSFMXCloudFlickr1.PhotoStream.Count - 1 do
    begin
      fg := TMSFMXCloudFlickr1.PhotoStream[I];
      AddPhotoStreamToList(fg);
    end;
    ComboBox1.EndUpdate;

    ComboBox1.ItemIndex := 0;
    if ComboBox1.ItemIndex <> -1 then
    begin
      Button6.Enabled := True;
      SpeedButton4.Enabled := True;
    end;
  end;
end;

procedure TForm82.Button1Click(Sender: TObject);
var
  acc: boolean;
begin

  TMSFMXCloudFlickr1.App.Key := FlickrAppkey;
  TMSFMXCloudFlickr1.App.Secret := FlickrAppSecret;

  if TMSFMXCloudFlickr1.App.Key <> '' then
  begin
//    TMSFMXCloudFlickr1.PersistTokens.Location := plIniFile;
    TMSFMXCloudFlickr1.PersistTokens.Key := TPath.GetDocumentsPath + '/flickr.ini';
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

procedure TForm82.Button2Click(Sender: TObject);
begin
  TMSFMXCloudImage1.URL := '';
  ListView1.Items.Clear;
  ListView2.Items.Clear;
  ComboBox1.Items.Clear;
  TMSFMXCloudFlickr1.ClearTokens;
  connected := False;
  ToggleControls;
end;

procedure TForm82.Button6Click(Sender: TObject);
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

procedure TForm82.Button8Click(Sender: TObject);
var
  fg: TFlickrSet;
  ph: TFlickrPhoto;
  geo: TGeoLocation;
  I: Integer;
begin
  if Assigned(ListView1.Selected) then
  begin
    fg := TFlickrSet(ListView1.Selected.Data);
    ph := fg.AddAndUploadPhoto(TPath.GetDocumentsPath + '/sample.jpg', Edit3.Text, Edit4.Text);
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

procedure TForm82.ComboBox1Change(Sender: TObject);
begin
  if ComboBox1.ItemIndex <> -1 then
  begin
    SpeedButton4.Enabled := True;
    Button6.Enabled := True;
  end;
end;

procedure TForm82.LoadSets;
var
  I: Integer;
  fg: TFlickrSet;
begin
  ListView1.Items.Clear;
  if TMSFMXCloudFlickr1.GetSets then
  begin
    for I := 0 to TMSFMXCloudFlickr1.Sets.Count - 1 do
    begin
      fg := TMSFMXCloudFlickr1.Sets[I];
      AddSetToList(fg);
    end;
  end;
end;

procedure TForm82.SpeedButton1Click(Sender: TObject);
var
  ph: TFlickrPhoto;
begin
  if Assigned(ListView2.Selected) then
  begin
    ph := TFlickrPhoto(ListView2.Selected.Data);
    ph.DownloadLargest(TPath.GetDocumentsPath + '/' + ph.ID + '.jpg');
  end;
end;

procedure TForm82.SpeedButton2Click(Sender: TObject);
var
  ph: TFlickrPhoto;
  str: String;
begin
  if Assigned(ListView2.Selected) then
  begin
    if MessageDlg('Delete selected photo from set ?', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes then
    begin
      ph := TFlickrPhoto(ListView2.Selected.Data);
      str := ph.Title;
      if TMSFMXCloudFlickr1.RemovePhoto(ph.ID) then
      begin
        ListView2.Items.Delete(ListView2.Selected.Index);
        LoadPhotoStream;
        if ListView2.Items.Count = 0 then
        begin
          //remove set (set does not exist when photo's are empty
          if Assigned(ListView1.Selected) then
          begin
            TMSFMXCloudFlickr1.Sets.Delete(TFlickrSet(ListView1.Selected.Data).Index);
            ListView1.RemoveObject(ListView1.Selected);
          end;
        end;
      end;
    end;
  end;

end;

procedure TForm82.SpeedButton3Click(Sender: TObject);
var
  ph: TFlickrPhoto;
begin
  if MessageDlg('Do you want to upload a sample file?', TMsgDlgType.mtConfirmation,[TMsgDlgBtn.mbYes,TMsgDlgBtn.mbNo],0) = mrYes then
  begin
    ph := TMSFMXCloudFlickr1.UploadPhotoToStream(TPath.GetDocumentsPath + '/sample.jpg');
    if Assigned(ph) then
    begin
      AddPhotoStreamToList(ph);
      ComboBox1.ItemIndex := ComboBox1.Items.Count - 1;
      ComboBox1Change(Self);
    end;
  end;
end;

procedure TForm82.SpeedButton4Click(Sender: TObject);
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

      Button6.Enabled := False;
      LoadPhotoStream;
    end;
  end;
end;

procedure TForm82.SpeedButton5Click(Sender: TObject);
begin
  if Assigned(ListView1.Selected) then
  begin
    if MessageDlg('Delete selected set ?', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes then
    begin
      if TMSFMXCloudFlickr1.RemoveSet(TFlickrSet(ListView1.Selected.Data).ID) then
        ListView1.Items.Delete(ListView1.Selected.Index);

      ListView2.Items.Clear;
      Button8.Text := 'Upload Photo to';
      Button8.Enabled := False;
    end;
  end;
end;

procedure TForm82.SpeedButton6Click(Sender: TObject);
var
  str: string;
begin
  str := Edit6.Text;
  if str <> '' then
    ListBox1.Items.Add(str);
end;

procedure TForm82.SpeedButton7Click(Sender: TObject);
begin
  if ListBox1.ItemIndex <> -1 then
  begin
    if MessageDlg('Delete selected tag ?', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes then
      ListBox1.Items.Delete(ListBox1.ItemIndex);
  end;
end;

procedure TForm82.SpeedButton8Click(Sender: TObject);
var
  ph: TFlickrPhoto;
begin
  if Assigned(ListView2.Selected) then
  begin
    ph := TFlickrPhoto(ListView2.Selected.Data);
    if MessageDlg('Do you want to download the file?', TMsgDlgType.mtConfirmation,[TMsgDlgBtn.mbYes,TMsgDlgBtn.mbNo],0) = mrYes then
      ph.DownloadLargest(TPath.GetDocumentsPath + '/' + ph.ID + '.jpg');
  end;
end;

procedure TForm82.FormCreate(Sender: TObject);
begin
  {$IFDEF IOS}
  TFile.Copy(ExtractFilePath(Paramstr(0)) + 'sample.jpg', TPath.GetDocumentsPath + '/sample.jpg', True);
  {$ENDIF}
  TMSFMXCloudImage1.Margins.Left := 10;
  TMSFMXCloudImage1.Margins.Top := 10;
  TMSFMXCloudImage1.Margins.Bottom := 10;
  TMSFMXCloudImage1.Margins.Right := 10;
end;

procedure TForm82.TMSFMXCloudFlickr1ReceivedAccessToken(Sender: TObject);
begin
  TMSFMXCloudFlickr1.SaveTokens;
  TMSFMXCloudFlickr1.GetAccountInfo;
  Connected := true;
  ToggleControls;
  AutoLoad;
end;

procedure TForm82.ToggleControls;
begin
  Button1.Enabled := not connected;
  Button2.Enabled := connected;
  ListView1.Enabled := connected;
  ListView2.Enabled := connected;
  SpeedButton3.Enabled := connected;
  Edit1.Enabled := connected;
  Edit2.Enabled := connected;
  ComboBox1.Enabled := connected;
end;

end.
