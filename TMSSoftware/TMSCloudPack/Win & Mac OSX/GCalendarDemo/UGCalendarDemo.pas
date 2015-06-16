unit UGCalendarDemo;

interface

uses
  FMX.Forms, FMX.TMSCloudBase, FMX.TMSCloudGCalendar, FMX.Controls,
  FMX.Memo, FMX.StdCtrls, FMX.ListBox, FMX.Edit, FMX.Grid, FMX.Layouts, FMX.Dialogs, DateUtils,
  FMX.TMSCloudListView, FMX.ExtCtrls, FMX.Objects, System.Classes, FMX.Types, UITypes, SysUtils,
  FMX.TMSCloudBaseFMX, FMX.TMSCloudCustomGoogle, FMX.TMSCloudGoogleFMX,
  FMX.TMSCloudCustomGCalendar;

type

  TForm1 = class(TForm)
StyleBook1: TStyleBook;

    TMSFMXCloudGCalendar1: TTMSFMXCloudGCalendar;
    Panel1: TPanel;
    Button1: TButton;
    GroupBox1: TGroupBox;
    ComboBox1: TComboBox;
    dpCalStartDate: TCalendarEdit;
    dpCalEndDate: TCalendarEdit;
    btUpdate: TButton;
    Label1: TLabel;
    Label13: TLabel;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label11: TLabel;
    Label10: TLabel;
    Edit3: TEdit;
    Edit5: TEdit;
    StartDate: TCalendarEdit;
    EndDate: TCalendarEdit;
    StartTime: TCalendarEdit;
    EndTime: TCalendarEdit;
    cbVisibility: TComboBox;
    cbAllday: TCheckBox;
    CloudListView1: TTMSFMXCloudListView;
    Panel2: TPanel;
    Button6: TButton;
    Button7: TButton;
    Button5: TButton;
    GroupBox4: TGroupBox;
    lvAtt: TTMSFMXCloudListView;
    Label3: TLabel;
    Label12: TLabel;
    EditAttEmail: TEdit;
    EditAttName: TEdit;
    btInvite: TButton;
    Image1: TImage;
    btRemove: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure FillCalendars();
    procedure FillCalendarItems();
    procedure FillCalendarItemDetails();
    procedure ToggleControls();
    procedure ClearControls();
    procedure Init();
    procedure SetCalendarItem(Item: TGCalendarItem);
    procedure ComboBox1Change(Sender: TObject);
    procedure TMSFMXCloudGCalendar1ReceivedAccessToken(Sender: TObject);
    procedure btInviteClick(Sender: TObject);
    procedure ListAttendees(Item: TGCalendarItem);
    procedure FormCreate(Sender: TObject);
    procedure btUpdateClick(Sender: TObject);
    procedure cbAlldayClick(Sender: TObject);
    procedure ComboBox1Click(Sender: TObject);
    procedure btRemoveClick(Sender: TObject);
    procedure CloudListView1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Connected: boolean;
    Inserting: boolean;
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
//  GAppkey = 'xxxxxxxxx';
//  GAppSecret = 'yyyyyyyy';

{$I APPIDS.INC}

procedure TForm1.TMSFMXCloudGCalendar1ReceivedAccessToken(Sender: TObject);
begin
  TMSFMXCloudGCalendar1.SaveTokens;
  Init;
end;

procedure TForm1.btInviteClick(Sender: TObject);
var
  li: TGCalendarItem;
  att: TGAttendee;
begin
  if (CloudListView1.ItemIndex >= 0) then
  begin
    li := CloudListView1.Items[CloudListView1.ItemIndex].Data;
    att := li.Attendees.Add;
    att.Summary := EditAttName.Text;
    att.Email := EditAttEmail.Text;
    ListAttendees(li);
    EditAttEmail.Text := '';
    EditAttName.Text := '';
  end;
end;

procedure TForm1.btRemoveClick(Sender: TObject);
begin
  TMSFMXCloudGCalendar1.ClearTokens;
  Connected := false;
  ToggleControls;
end;

procedure TForm1.btUpdateClick(Sender: TObject);
begin
  FillCalendarItems;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  TMSFMXCloudGCalendar1.Logging := true;
  TMSFMXCloudGCalendar1.App.Key := GAppkey;
  TMSFMXCloudGCalendar1.App.Secret := GAppSecret;
  TMSFMXCloudGCalendar1.Logging := true;

  if not TMSFMXCloudGCalendar1.TestTokens then
    TMSFMXCloudGCalendar1.RefreshAccess;

  if not TMSFMXCloudGCalendar1.TestTokens then
    TMSFMXCloudGCalendar1.DoAuth
  else
    Init;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  li: TGCalendarItem;
begin
  if not (Inserting) then
  begin
    ClearControls;
    Edit3.SetFocus;
    CloudListView1.ItemIndex := -1;
    Button5.Text  := 'Insert';
    Inserting := true;
  end
  else
  begin
    Button5.Text  := 'New';
    li := TMSFMXCloudGCalendar1.Items.Add;
    SetCalendarItem(li);
    li.CalendarID := (ComboBox1.Items.Objects[ComboBox1.ItemIndex] as TGCalendar).ID;
    TMSFMXCloudGCalendar1.Add(li);

    FillCalendarItems;
    Inserting := false;
  end;
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  buttonSelected: integer;
begin
  if CloudListView1.ItemIndex >= 0 then
  begin
    buttonSelected := MessageDlg('Are you sure you want to delete the selected Event?', TMsgDlgType.mtConfirmation, mbOKCancel, 0);

    if buttonSelected = mrOk then
    begin
      TMSFMXCloudGCalendar1.Delete(CloudListView1.Items[CloudListView1.ItemIndex].Data);
      FillCalendarItems;
      ClearControls;
    end;
  end
  else
  begin
    ShowMessage('Please select an Event first.');
  end;
end;

procedure TForm1.Button7Click(Sender: TObject);
var
  li: TGCalendarItem;
  SelectedID: string;
begin
  if CloudListView1.ItemIndex >= 0 then
  begin
    li := CloudListView1.Items[CloudListView1.ItemIndex].Data;

    SetCalendarItem(li);
    TMSFMXCloudGCalendar1.Update(li);
    SelectedID := li.ID;

    FillCalendarItems;

    CloudListView1.ItemIndex := TMSFMXCloudGCalendar1.Items.Find(SelectedID).Index;
  end
  else
  begin
    ShowMessage('Please select an Event first.');
  end;
end;

procedure TForm1.cbAlldayClick(Sender: TObject);
begin
  StartTime.Enabled := not cbAllday.IsChecked;
  EndTime.Enabled := not cbAllday.IsChecked;
end;

procedure TForm1.ClearControls;
begin
  Edit3.Text := '';
  Edit5.Text := '';
  Memo1.Lines.Text := '';
  cbVisibility.ItemIndex := 0;
  cbAllday.IsChecked  := false;
  StartDate.Date := Now;
  EndDate.Date := Now;
  StartTime.Date := StrToDateTime(IntToStr(HourOf(IncHour(Time, 1))) + ':00');
  EndTime.Date := IncHour(StartTime.Date, 2);
  lvAtt.Items.Clear;
  EditAttName.Text := '';
  EditAttEmail.Text := '';
end;

procedure TForm1.CloudListView1Change(Sender: TObject);
begin
  FillCalendarItemDetails;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  FillCalendarItems;
end;

procedure TForm1.ComboBox1Click(Sender: TObject);
begin
  FillCalendarItems;
end;

procedure TForm1.FillCalendarItems;
var
  I: Integer;
  gcal: TGCalendar;
  li: TListItem;
begin
  if ComboBox1.ItemIndex >= 0 then
  begin
    gcal := (ComboBox1.Items.Objects[ComboBox1.ItemIndex] as TGCalendar);
    TMSFMXCloudGCalendar1.GetCalendar(gcal.ID, dpCalStartDate.Date, dpCalEndDate.Date);

    CloudListView1.Items.Clear;
    for I := 0 to TMSFMXCloudGCalendar1.Items.Count - 1 do
    begin
      li := CloudListView1.Items.Add;
      li.Text  := FormatDateTime('dd/mm/yyyy hh:nn', TMSFMXCloudGCalendar1.Items[I].StartTime);
      li.SubItems.Add(FormatDateTime('dd/mm/yyyy hh:nn', TMSFMXCloudGCalendar1.Items[I].EndTime));
      li.SubItems.Add(TMSFMXCloudGCalendar1.Items[I].Summary);
      li.SubItems.Add(TMSFMXCloudGCalendar1.Items[I].Description);
      li.Data := TMSFMXCloudGCalendar1.Items[I];
    end;
  end;
end;

procedure TForm1.FillCalendars;
var
  I: Integer;
begin
  TMSFMXCloudGCalendar1.GetCalendars();

  ComboBox1.Items.Clear;

  for I := 0 to TMSFMXCloudGCalendar1.Calendars.Count - 1 do
  begin
    ComboBox1.items.AddObject(TMSFMXCloudGCalendar1.Calendars[I].Summary, TMSFMXCloudGCalendar1.Calendars[I]);
  end;
  ComboBox1.ItemIndex := 0;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  TMSFMXCloudGCalendar1.PersistTokens.Key := ExtractFilePath(ParamStr(0)) + 'google.ini';
  TMSFMXCloudGCalendar1.PersistTokens.Section := 'tokens';
  TMSFMXCloudGCalendar1.LoadTokens;

  dpCalStartDate.Date := IncMonth(Now, -1);
  dpCalEndDate.Date := IncMonth(Now, 2);
  ClearControls;

  Inserting := false;
  Connected := false;
  ToggleControls;
  CloudListView1.ColumnByIndex(3).Width := 380;
end;


procedure TForm1.Init;
begin
  Connected := true;
  FillCalendars;
  FillCalendarItems;
  ToggleControls;
end;

procedure TForm1.FillCalendarItemDetails();
var
  li: TGCalendarItem;
begin
  if CloudListView1.ItemIndex >= 0 then
  begin
    li := CloudListView1.Items[CloudListView1.ItemIndex].Data;

    Edit3.Text := li.Summary;
    Memo1.Lines.Text := li.Description;
    Edit5.Text := li.Location;
    StartDate.Date := li.StartTime;
    EndDate.Date := li.EndTime;
    StartTime.Date := li.StartTime;
    EndTime.Date := li.EndTime;
    EditAttEmail.Text := '';
    EditAttName.Text := '';

    cbVisibility.ItemIndex := Ord(li.Visibility);
    cbAllday.IsChecked  := li.IsAllDay;

    ListAttendees(li);
  end;
end;

procedure TForm1.ListAttendees(Item: TGCalendarItem);
var
  att: string;
  I: Integer;
  li: TListItem;
begin
  att := '';
  lvAtt.Items.Clear;
  for I := 0 to Item.Attendees.Count - 1 do
  begin
    case Item.Attendees[I].Status of
      rsNeedsAction: att := 'needs action';
      rsDeclined: att := 'declined';
      rsTentative: att := 'tentative';
      rsAccepted: att := 'accpeted';
    end;

    li := lvAtt.Items.Add;
    li.Text  := att;
    li.SubItems.Add(Item.Attendees[I].Summary);
    li.SubItems.Add(Item.Attendees[I].Email);
  end;
end;

procedure TForm1.SetCalendarItem(Item: TGCalendarItem);
begin
    Item.Summary := Edit3.Text;
    Item.Description := Memo1.Lines.Text;
    Item.Location := Edit5.Text;

    if cbAllday.IsChecked  then
    begin
      Item.StartTime := EncodeDateTime(YearOf(StartDate.Date), MonthOf(StartDate.Date), DayOf(StartDate.Date), 0, 0, 0, 0);
      Item.EndTime := EncodeDateTime(YearOf(EndDate.Date), MonthOf(EndDate.Date), DayOf(EndDate.Date), 0, 0, 0, 0);
      Item.IsAllDay := true;
    end
    else
    begin
      Item.StartTime := EncodeDateTime(YearOf(StartDate.Date), MonthOf(StartDate.Date), DayOf(StartDate.Date), HourOf(StartTime.Date), MinuteOf(StartTime.Date), SecondOf(StartTime.Date),0);
      Item.EndTime := EncodeDateTime(YearOf(EndDate.Date), MonthOf(EndDate.Date), DayOf(EndDate.Date), HourOf(EndTime.Date), MinuteOf(EndTime.Date), SecondOf(EndTime.Date),0);
      Item.IsAllDay := false;
    end;

    Item.Visibility := TVisibility(cbVisibility.ItemIndex);
end;

procedure TForm1.ToggleControls;
begin
  GroupBox1.Enabled := Connected;
  GroupBox2.Enabled := Connected;
  GroupBox3.Enabled := Connected;
  GroupBox4.Enabled := Connected;
  Panel2.Enabled := Connected;
  ComboBox1.Enabled := Connected;
  dpCalStartDate.Enabled := Connected;
  dpCalEndDate.Enabled := Connected;
  btUpdate.Enabled := Connected;
  CloudListView1.Enabled := Connected;
  Edit3.Enabled := Connected;
  Edit5.Enabled := Connected;
  Memo1.Enabled := Connected;
  cbVisibility.Enabled := Connected;
  cbAllday.Enabled := Connected;
  StartDate.Enabled := Connected;
  EndDate.Enabled := Connected;
  StartTime.Enabled := Connected;
  EndTime.Enabled := Connected;
  lvAtt.Enabled := Connected;
  EditAttName.Enabled := Connected;
  EditAttEmail.Enabled := Connected;
  btInvite.Enabled := Connected;
  Button5.Enabled := Connected;
  Button6.Enabled := Connected;
  Button7.Enabled := Connected;
  btRemove.Enabled := Connected;
  Button1.Enabled := not Connected;
end;

end.
