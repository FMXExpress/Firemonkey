unit UPryvDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.TMSCloudBase, FMX.TMSCloudBaseFMX, FMX.TMSCloudCustomPryv,
  FMX.TMSCloudPryv, FMX.Objects, FMX.Layouts, FMX.TreeView, FMX.Edit,
  FMX.ListBox, FMX.Grid, FMX.TMSCloudListView, FMX.TMSCloudImage, FMX.Memo,
  FMX.ExtCtrls, FMX.DateTimeCtrls, IOUtils;

type
  TForm44 = class(TForm)
    Panel1: TPanel;
    btConnect: TButton;
    Image1: TImage;
    TMSFMXCloudPryv1: TTMSFMXCloudPryv;
    btRemove: TButton;
    Panel2: TPanel;
    Label1: TLabel;
    btStreams: TButton;
    TreeView1: TTreeView;
    Label13: TLabel;
    edStreamName: TEdit;
    cbSubStream: TCheckBox;
    btAddStream: TButton;
    btDeleteStream: TButton;
    btUpdateStream: TButton;
    Panel3: TPanel;
    Label2: TLabel;
    btEvents: TButton;
    Label3: TLabel;
    cbEventCount: TComboBox;
    cbStream: TCheckBox;
    TMSFMXCloudListView1: TTMSFMXCloudListView;
    Label4: TLabel;
    cbEventType: TComboBox;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    btAddEvent: TButton;
    btDeleteEvent: TButton;
    btUpdateEvent: TButton;
    edTags: TEdit;
    cbUnit: TComboEdit;
    edLongitude: TEdit;
    edLatitude: TEdit;
    Label10: TLabel;
    meValue: TMemo;
    meDescription: TMemo;
    Label11: TLabel;
    Label12: TLabel;
    TMSFMXCloudImage1: TTMSFMXCloudImage;
    dpDate: TCalendarEdit;
    Line1: TLine;
    procedure btConnectClick(Sender: TObject);
    procedure TMSFMXCloudPryv1ReceivedAccessToken(Sender: TObject);
    procedure btRemoveClick(Sender: TObject);
    procedure btStreamsClick(Sender: TObject);
    procedure TreeView1Change(Sender: TObject);
    procedure btAddStreamClick(Sender: TObject);
    procedure btDeleteStreamClick(Sender: TObject);
    procedure btUpdateStreamClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btEventsClick(Sender: TObject);
    procedure cbEventTypeChange(Sender: TObject);
    procedure btAddEventClick(Sender: TObject);
    procedure btDeleteEventClick(Sender: TObject);
    procedure btUpdateEventClick(Sender: TObject);
    procedure TMSFMXCloudListView1Change(Sender: TObject);
    procedure btDownloadClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Connected: boolean;
    InitLV: boolean;
    IsUploading: boolean;
    IsDownloading: boolean;
    procedure ToggleControls;
    procedure ToggleControlsText;
    procedure ToggleControlsPicture;
    procedure ToggleControlsPosition;
    procedure ToggleControlsValue;
    procedure FillStreams;
    procedure FillEvents;
  end;

var
  Form44: TForm44;

implementation

{$R *.fmx}

// PLEASE USE A VALID INCLUDE FILE THAT CONTAINS THE APPLICATION KEY & SECRET
// FOR THE CLOUD STORAGE SERVICES YOU WANT TO USE
// STRUCTURE OF THIS .INC FILE SHOULD BE
//
// const
//  PryvAppkey = 'xxxxxxxxx';

{$I APPIDS.INC}

procedure TForm44.btAddStreamClick(Sender: TObject);
var
  it: TPryvStreamItem;
  si: TPryvStreamItem;
begin
  it := TMSFMXCloudPryv1.Streams.Add;

  if cbSubStream.IsChecked then
  begin
    if not Assigned(TreeView1.Selected) then
    begin
      ShowMessage('Please select a Stream first.');
      exit;
    end;
    si := TPryvStreamItem(TTMSFMXCloudTreeViewItem(TreeView1.Selected).DataObject);
    it.ParentID := si.ID;
  end;

  it.Summary := edStreamName.Text;
  TMSFMXCloudPryv1.AddStream(it);
  FillStreams;
end;

procedure TForm44.btAddEventClick(Sender: TObject);
var
  text: TPryvText;
  pic: TPryvPicture;
  pos: TPryvPosition;
  val: TPryvValue;
  si: TPryvStreamItem;
begin
  if not Assigned(TreeView1.Selected) then
  begin
    ShowMessage('Please select a Stream first.');
    exit;
  end;

  si := TPryvStreamItem(TTMSFMXCloudTreeViewItem(TreeView1.Selected).DataObject);

  if cbEventType.ItemIndex = 0 then
  begin
    text := TPryvText.Create;
    TMSFMXCloudPryv1.Events.Add(text);
    text.Content := meValue.Text;
    text.StreamID := si.ID;
    text.DateTime := dpDate.Date;
    text.Tags.CommaText := edTags.Text;
    text.Description := meDescription.Text;
    TMSFMXCloudPryv1.AddEvent(text);
  end
  else if cbEventType.ItemIndex = 1 then
  begin
    IsUploading := true;

    pic := TPryvPicture.Create;
    TMSFMXCloudPryv1.Events.Add(pic);
    pic.StreamID := si.ID;
    pic.DateTime := dpDate.Date;
    pic.Tags.CommaText := edTags.Text;
    pic.Description := meDescription.Text;
    TMSFMXCloudPryv1.AddEvent(pic, ExtractFilePath(ParamStr(0)) + 'sample.jpg');

    IsUploading := false;
  end
  else if cbEventType.ItemIndex = 2 then
  begin
    pos := TPryvPosition.Create;
    TMSFMXCloudPryv1.Events.Add(pos);
    pos.Latitude := StrToFloat(edLatitude.Text);
    pos.Longitude := StrToFloat(edLongitude.Text);
    pos.StreamID := si.ID;
    pos.DateTime := dpDate.Date;
    pos.Tags.CommaText := edTags.Text;
    pos.Description := meDescription.Text;
    TMSFMXCloudPryv1.AddEvent(pos);
  end
  else if cbEventType.ItemIndex = 3 then
  begin
    val := TPryvValue.Create;
    TMSFMXCloudPryv1.Events.Add(val);
    val.Content := meValue.Text;
    val.StreamID := si.ID;
    val.UnitValue := cbUnit.Text;
    val.DateTime := dpDate.Date;
    val.Tags.CommaText := edTags.Text;
    val.Description := meDescription.Text;
    TMSFMXCloudPryv1.AddEvent(val);
  end;

  FillEvents;
end;

procedure TForm44.btConnectClick(Sender: TObject);
var
  acc: boolean;
begin
  TMSFMXCloudPryv1.App.Key := PryvAppKey;

  if TMSFMXCloudPryv1.App.Key <> '' then
  begin
    TMSFMXCloudPryv1.PersistTokens.Location := plIniFile;
    TMSFMXCloudPryv1.PersistTokens.Key := TPath.GetDocumentsPath + '/pryv.ini';
    TMSFMXCloudPryv1.PersistTokens.Section := 'tokens';
    TMSFMXCloudPryv1.LoadTokens;

    acc := TMSFMXCloudPryv1.TestTokens;

    if not acc then
      TMSFMXCloudPryv1.DoAuth
    else
    begin
      Connected := true;
      ToggleControls;
    end;
  end
  else
    ShowMessage('Please provide a valid application ID for the client component');
end;

procedure TForm44.btDeleteStreamClick(Sender: TObject);
var
  si: TPryvStreamItem;
begin
  if not Assigned(TreeView1.Selected) then
  begin
    ShowMessage('Please select a Stream first.');
    exit;
  end;

  si := TPryvStreamItem(TTMSFMXCloudTreeViewItem(TreeView1.Selected).DataObject);
  TreeView1.RemoveObject(TreeView1.Selected);
  TMSFMXCloudPryv1.DeleteStream(si);
  FillStreams;
end;

procedure TForm44.btDownloadClick(Sender: TObject);
var
  o: TPryvObject;
  sv: TSaveDialog;
begin
  if TMSFMXCloudListView1.ItemIndex < 0 then
    Exit;

  o := TMSFMXCloudPryv1.Events[TMSFMXCloudListView1.ItemIndex];

  if o is TPryvPicture then
  begin
    sv := TSaveDialog.Create(Self);
    sv.FileName := (o as TPryvPicture).FileName;
    if sv.Execute then
    begin
      IsDownloading := true;
      TMSFMXCloudPryv1.Download(o as TPryvPicture,sv.FileName);
      IsDownloading := false;
      ShowMessage('File ' +  (o as TPryvPicture).FileName + ' downloaded');
    end;
    sv.Free;
  end;
end;

procedure TForm44.btDeleteEventClick(Sender: TObject);
begin
  TMSFMXCloudPryv1.DeleteEvent(TMSFMXCloudPryv1.Events[TMSFMXCloudListView1.ItemIndex]);
  FillEvents;
end;

procedure TForm44.btEventsClick(Sender: TObject);
begin
  FillEvents;
end;

procedure TForm44.btRemoveClick(Sender: TObject);
begin
  TMSFMXCloudPryv1.ClearTokens;
  Connected := false;
  ToggleControls;
end;

procedure TForm44.btUpdateStreamClick(Sender: TObject);
var
  si: TPryvStreamItem;
begin
  if not Assigned(TreeView1.Selected) then
  begin
    ShowMessage('Please select a Stream first.');
    exit;
  end;
  si := TPryvStreamItem(TTMSFMXCloudTreeViewItem(TreeView1.Selected).DataObject);
  si.Summary := edStreamName.Text;
  TMSFMXCloudPryv1.UpdateStream(si);
  FillStreams;
end;

procedure TForm44.btUpdateEventClick(Sender: TObject);
var
  text: TPryvText;
  pic: TPryvPicture;
  pos: TPryvPosition;
  val: TPryvValue;
begin
  if cbEventType.ItemIndex = 0 then
  begin
    text := TMSFMXCloudPryv1.Events[TMSFMXCloudListView1.ItemIndex] as TPryvText;
    text.Content := meValue.Text;
    text.DateTime := dpDate.Date;
    text.Tags.CommaText := edTags.Text;
    text.Description := meDescription.Text;
    TMSFMXCloudPryv1.UpdateEvent(text);
  end
  else if cbEventType.ItemIndex = 1 then
  begin
    pic := TMSFMXCloudPryv1.Events[TMSFMXCloudListView1.ItemIndex] as TPryvPicture;
    pic.DateTime := dpDate.Date;
    pic.Tags.CommaText := edTags.Text;
    pic.Description := meDescription.Text;
    TMSFMXCloudPryv1.UpdateEvent(pic);
  end
  else if cbEventType.ItemIndex = 2 then
  begin
    pos := TMSFMXCloudPryv1.Events[TMSFMXCloudListView1.ItemIndex] as TPryvPosition;
    pos.Latitude := StrToFloat(edLatitude.Text);
    pos.Longitude := StrToFloat(edLongitude.Text);
    pos.DateTime := dpDate.Date;
    pos.Tags.CommaText := edTags.Text;
    pos.Description := meDescription.Text;
    TMSFMXCloudPryv1.UpdateEvent(pos);
  end
  else if cbEventType.ItemIndex = 3 then
  begin
    val := TMSFMXCloudPryv1.Events[TMSFMXCloudListView1.ItemIndex] as TPryvValue;
    val.Content := meValue.Text;
    val.DateTime := dpDate.Date;
    val.UnitValue := cbUnit.Text;
    val.Tags.CommaText := edTags.Text;
    val.Description := meDescription.Text;
    TMSFMXCloudPryv1.UpdateEvent(val);
  end;

  FillEvents;
end;

procedure TForm44.cbEventTypeChange(Sender: TObject);
begin
  if cbEventType.ItemIndex = 0 then
    ToggleControlsText
   else if cbEventType.ItemIndex = 1 then
    ToggleControlsPicture
   else if cbEventType.ItemIndex = 2 then
    ToggleControlsPosition
   else if cbEventType.ItemIndex = 3 then
    ToggleControlsValue;

  btUpdateEvent.Enabled := false;
end;

procedure TForm44.btStreamsClick(Sender: TObject);
begin
  FillStreams;
end;

procedure TForm44.FillEvents;
var
 I: integer;
 o: TPryvObject;
 li: TListItem;
 si: TPryvStreamItem;
 sa: TStringArray;
begin
  InitLV := true;
  meValue.Text := '';
  edTags.Text := '';
  edLatitude.Text := '';
  edLongitude.Text := '';
  cbUnit.Text := '';
  meDescription.Text := '';
  TMSFMXCloudImage1.URL := '';
  btUpdateEvent.Enabled := true;

  TMSFMXCloudListView1.Items.Clear;

  if cbStream.isChecked then
  begin
    if Assigned(TreeView1.Selected) then
    begin
      si := TPryvStreamItem(TTMSFMXCloudTreeViewItem(TreeView1.Selected).DataObject);
      SetLength(sa, 1);
      sa[0] := si.ID;
      TMSFMXCloudPryv1.GetEvents(sa,nil,StrToInt(cbEventCount.Selected.Text));
    end;
  end
  else
    TMSFMXCloudPryv1.GetEvents(StrToInt(cbEventCount.Selected.Text));

  for I := 0 to TMSFMXCloudPryv1.Events.Count - 1 do
  begin
    o := TMSFMXCloudPryv1.Events[I];

    li := TMSFMXCloudListView1.Items.Add;

    if o is TPryvValue then
    begin
      li.Text := (o as TPryvValue).Content;
      li.SubItems.Add((o as TPryvValue).UnitValue);
    end
    else
    begin
      if o is TPryvText then
      begin
        li.Text := (o as TPryvText).Content;
        li.SubItems.Add('Text')
      end
      else if o is TPryvPicture then
      begin
        li.Text := '';
        li.SubItems.Add('Picture')
      end
      else if o is TPryvPosition then
      begin
        li.Text := '';
        li.SubItems.Add('Position');
      end;
    end;

    li.SubItems.Add(o.Description);
    li.SubItems.Add(DateTimeToStr(o.DateTime));

    si := TMSFMXCloudPryv1.Streams.FindStreamById(o.StreamID);
    if Assigned(si) then
      li.SubItems.Add(si.Summary);

  end;
  InitLV := false;
end;

procedure TForm44.FillStreams;
begin
  TMSFMXCloudPryv1.GetStreams;
  TMSFMXCloudPryv1.FillTreeView(TreeView1);
end;

procedure TForm44.FormCreate(Sender: TObject);
begin
  InitLV := false;
  IsUploading := false;
  Connected := false;
  ToggleControls;
  dpDate.Date := Now;
end;

procedure TForm44.TMSFMXCloudListView1Change(Sender: TObject);
var
  o: TPryvObject;
begin
  if (InitLV) or (TMSFMXCloudListView1.ItemIndex < 0 )then
    Exit;

  btUpdateEvent.Enabled := true;

  meValue.Text := '';
  edTags.Text := '';
  edLatitude.Text := '';
  edLongitude.Text := '';
  cbUnit.Text := '';
  meDescription.Text := '';
  TMSFMXCloudImage1.URL := '';

  o := TMSFMXCloudPryv1.Events[TMSFMXCloudListView1.ItemIndex];
  meDescription.Text := o.Description;
  edTags.Text := o.Tags.CommaText;
  dpDate.Date := o.DateTime;

  if (o is TPryvValue) then
  begin
    cbEventType.ItemIndex := 3;
    ToggleControlsValue;
    cbUnit.Text := (o as TPryvValue).UnitValue;
  end
  else if (o is TPryvText) then
  begin
    cbEventType.ItemIndex := 0;
    ToggleControlsText;
    meValue.Text := (o as TPryvText).Content;
  end
  else if (o is TPryvPicture) then
  begin
    cbEventType.ItemIndex := 1;
    ToggleControlsPicture;
    TMSFMXCloudImage1.URL := (o as TPryvPicture).ImageURL;
  end
  else if (o is TPryvPosition) then
  begin
    cbEventType.ItemIndex := 2;
    ToggleControlsPosition;
    edLatitude.Text := FloatToStr((o as TPryvPosition).Latitude);
    edLongitude.Text := FloatToStr((o as TPryvPosition).Longitude);
  end;
end;

procedure TForm44.TMSFMXCloudPryv1ReceivedAccessToken(Sender: TObject);
begin
  TMSFMXCloudPryv1.SaveTokens;
  Connected := true;
  ToggleControls;
end;

procedure TForm44.ToggleControls;
begin
  btConnect.Enabled := not Connected;
  btRemove.Enabled := Connected;
  btStreams.Enabled := Connected;
  btEvents.Enabled := Connected;
  btAddEvent.Enabled := Connected;
  btAddStream.Enabled := Connected;
  btDeleteEvent.Enabled := Connected;
  btDeleteStream.Enabled := Connected;
  btUpdateEvent.Enabled := Connected;
  btUpdateStream.Enabled := Connected;
  TMSFMXCloudListView1.Enabled := Connected;
  TreeView1.Enabled := Connected;
  edStreamName.Enabled := Connected;
  edTags.Enabled := Connected;
  edLatitude.Enabled := Connected;
  edLongitude.Enabled := Connected;
  cbUnit.Enabled := Connected;
  meValue.Enabled := Connected;
  meDescription.Enabled := Connected;
  cbEventCount.Enabled := Connected;
  cbEventType.Enabled := Connected;
  cbStream.Enabled := Connected;
  cbSubStream.Enabled := Connected;
  dpDate.Enabled := Connected;
end;

procedure TForm44.ToggleControlsPicture;
begin
  edLatitude.Enabled := false;
  edLongitude.Enabled := false;
  cbUnit.Enabled := false;
  meValue.Enabled := false;
end;

procedure TForm44.ToggleControlsPosition;
begin
  edLatitude.Enabled := true;
  edLongitude.Enabled := true;
  cbUnit.Enabled := false;
  meValue.Enabled := false;
end;

procedure TForm44.ToggleControlsText;
begin
  edLatitude.Enabled := false;
  edLongitude.Enabled := false;
  cbUnit.Enabled := false;
  meValue.Enabled := true;
end;

procedure TForm44.ToggleControlsValue;
begin
  edLatitude.Enabled := false;
  edLongitude.Enabled := false;
  cbUnit.Enabled := true;
  meValue.Enabled := true;
end;

procedure TForm44.TreeView1Change(Sender: TObject);
var
  si: TPryvStreamItem;
begin
  if Assigned(TreeView1.Selected) then
  begin
    si := TPryvStreamItem(TTMSFMXCloudTreeViewItem(TreeView1.Selected).DataObject);
    edStreamName.Text := si.Summary;
  end;
end;

end.

