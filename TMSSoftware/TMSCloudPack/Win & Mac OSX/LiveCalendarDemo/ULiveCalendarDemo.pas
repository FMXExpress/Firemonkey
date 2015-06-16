unit ULiveCalendarDemo;

interface

uses
  FMX.TMSCloudBase, FMX.TMSCloudLiveCalendar, FMX.Controls, SysUtils, DateUtils,
  FMX.StdCtrls, FMX.ListBox, FMX.Edit, FMX.Grid, FMX.Layouts, FMX.Dialogs, UITypes,
  FMX.TMSCloudListView, FMX.ExtCtrls, FMX.Objects, System.Classes, FMX.Types, FMX.Forms,
  FMX.TMSCloudBaseFMX, FMX.TMSCloudCustomLive, FMX.TMSCloudLiveFMX,
  FMX.TMSCloudCustomLiveCalendar;

type

  TForm1 = class(TForm)
StyleBook1: TStyleBook;

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
    Edit4: TEdit;
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
    TMSFMXCloudLiveCalendar1: TTMSFMXCloudLiveCalendar;
    Image1: TImage;
    btRemove: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure FillCalendars();
    procedure FillCalendarItems();
    procedure FillCalendarItemDetails();
    procedure ToggleControls();
    procedure ToggleEditControls(Enabled: boolean);
    procedure ClearControls();
    procedure Init();
    function ValidateForm(): boolean;
    procedure SetCalendarItem(Item: TLiveCalendarItem);
    procedure ComboBox1Change(Sender: TObject);
    procedure TMSFMXCloudLiveCalendar1ReceivedAccessToken(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btUpdateClick(Sender: TObject);
    procedure cbAlldayClick(Sender: TObject);
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
//  LiveAppkey = 'xxxxxxxxx';
//  LiveAppSecret = 'yyyyyyyy';

{$I APPIDS.INC}

procedure TForm1.TMSFMXCloudLiveCalendar1ReceivedAccessToken(Sender: TObject);
begin
  TMSFMXCloudLiveCalendar1.SaveTokens;
  Init;
end;

procedure TForm1.btRemoveClick(Sender: TObject);
begin
  TMSFMXCloudLiveCalendar1.ClearTokens;
  Connected := false;
  ToggleControls;
end;

procedure TForm1.btUpdateClick(Sender: TObject);
begin
  FillCalendarItems;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  TMSFMXCloudLiveCalendar1.App.Key := LiveAppKey;
  TMSFMXCloudLiveCalendar1.App.Secret := LiveAppSecret;
  TMSFMXCloudLiveCalendar1.Logging := true;

  if not TMSFMXCloudLiveCalendar1.TestTokens then
    TMSFMXCloudLiveCalendar1.RefreshAccess;

  if not TMSFMXCloudLiveCalendar1.TestTokens then
    TMSFMXCloudLiveCalendar1.DoAuth
  else
    Init;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  li: TLiveCalendarItem;
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
    if not (ValidateForm) then
      exit;

    Button5.Text  := 'New';
    li := TMSFMXCloudLiveCalendar1.Items.Add;
    SetCalendarItem(li);
    li.CalendarID := (ComboBox1.Items.Objects[ComboBox1.ItemIndex] as TLiveCalendar).ID;
    TMSFMXCloudLiveCalendar1.Add(li);

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
      TMSFMXCloudLiveCalendar1.Delete(CloudListView1.Items[CloudListView1.ItemIndex].Data);
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
  li: TLiveCalendarItem;
  SelectedID: string;
begin
  if CloudListView1.ItemIndex >= 0 then
  begin
    if not (ValidateForm) then
      exit;

    li := CloudListView1.Items[CloudListView1.ItemIndex].Data;

    SetCalendarItem(li);
    TMSFMXCloudLiveCalendar1.Update(li);
    SelectedID := li.ID;

    FillCalendarItems;

    CloudListView1.ItemIndex := TMSFMXCloudLiveCalendar1.Items.Find(SelectedID).Index;
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
  Edit4.Text := '';
  Edit5.Text := '';
  cbVisibility.ItemIndex := 0;
  cbAllday.IsChecked  := false;
  StartDate.Date := Now;
  EndDate.Date := Now;
  StartTime.Date := StrToDateTime(IntToStr(HourOf(IncHour(Time, 1))) + ':00');
  EndTime.Date := IncHour(StartTime.Date, 2);
end;

procedure TForm1.CloudListView1Change(Sender: TObject);
begin
  FillCalendarItemDetails;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  ClearControls;
  FillCalendarItems;
end;

procedure TForm1.FillCalendarItems;
var
  I: Integer;
  LiveCalendar: TLiveCalendar;
  li: TListItem;
begin
  if ComboBox1.ItemIndex >= 0 then
  begin
    LiveCalendar := (ComboBox1.Items.Objects[ComboBox1.ItemIndex] as TLiveCalendar);

    ToggleEditControls(not LiveCalendar.ReadOnly);

    TMSFMXCloudLiveCalendar1.GetCalendar(LiveCalendar.ID, dpCalStartDate.Date, dpCalEndDate.Date);

    CloudListView1.Items.Clear;
    for I := 0 to TMSFMXCloudLiveCalendar1.Items.Count - 1 do
    begin
      li := CloudListView1.Items.Add;
      li.Text  := FormatDateTime('dd/mm/yyyy hh:nn', TMSFMXCloudLiveCalendar1.Items[I].StartTime);
      li.SubItems.Add(FormatDateTime('dd/mm/yyyy hh:nn', TMSFMXCloudLiveCalendar1.Items[I].EndTime));
      li.SubItems.Add(TMSFMXCloudLiveCalendar1.Items[I].Summary);
      li.SubItems.Add(TMSFMXCloudLiveCalendar1.Items[I].Description);
      li.Data := TMSFMXCloudLiveCalendar1.Items[I];
    end;
  end;
end;

procedure TForm1.FillCalendars;
var
  I: Integer;
begin
  TMSFMXCloudLiveCalendar1.GetCalendars();

  ComboBox1.Items.Clear;

  for I := 0 to TMSFMXCloudLiveCalendar1.Calendars.Count - 1 do
    ComboBox1.Items.AddObject(TMSFMXCloudLiveCalendar1.Calendars[I].Summary, TMSFMXCloudLiveCalendar1.Calendars[I]);
  ComboBox1.ItemIndex := 0;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  TMSFMXCloudLiveCalendar1.PersistTokens.Key := ExtractFilePath(ParamStr(0)) + 'live.ini';
  TMSFMXCloudLiveCalendar1.PersistTokens.Section := 'tokens';
  TMSFMXCloudLiveCalendar1.LoadTokens;

  dpCalStartDate.Date := IncMonth(Now, -1);
  dpCalEndDate.Date := IncMonth(Now, 2);
  ClearControls;

  Inserting := false;
  Connected := false;
  ToggleControls;
  CloudListView1.ColumnByIndex(3).width := 370;
end;



procedure TForm1.Init;
begin
  Connected := true;
  ToggleControls;
  FillCalendars;
  FillCalendarItems;
end;

procedure TForm1.FillCalendarItemDetails();
var
  li: TLiveCalendarItem;
begin
  if CloudListView1.ItemIndex >= 0 then
  begin
    li := CloudListView1.Items[CloudListView1.ItemIndex].Data;

    Edit3.Text := li.Summary;
    Edit4.Text := li.Description;
    Edit5.Text := li.Location;
    StartDate.Date := li.StartTime;
    EndDate.Date := li.EndTime;
    StartTime.Date := li.StartTime;
    EndTime.Date := li.EndTime;

    cbVisibility.ItemIndex := Ord(li.Visibility);
    cbAllday.IsChecked  := li.IsAllDay;
  end;
end;

procedure TForm1.SetCalendarItem(Item: TLiveCalendarItem);
begin
    Item.Summary := Edit3.Text;
    Item.Description := Edit4.Text;
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
  Panel2.Enabled := Connected;
  ComboBox1.Enabled := Connected;
  dpCalStartDate.Enabled := Connected;
  dpCalEndDate.Enabled := Connected;
  btUpdate.Enabled := Connected;
  CloudListView1.Enabled := Connected;
  Edit3.Enabled := Connected;
  Edit4.Enabled := Connected;
  Edit5.Enabled := Connected;
  cbVisibility.Enabled := Connected;
  cbAllday.Enabled := Connected;
  StartDate.Enabled := Connected;
  EndDate.Enabled := Connected;
  StartTime.Enabled := Connected;
  EndTime.Enabled := Connected;
  Button5.Enabled := Connected;
  Button6.Enabled := Connected;
  Button7.Enabled := Connected;
  btRemove.Enabled := Connected;
  Button1.Enabled := not Connected;
end;

procedure TForm1.ToggleEditControls(Enabled: boolean);
begin
  Edit3.Enabled := Enabled;
  Edit4.Enabled := Enabled;
  Edit5.Enabled := Enabled;
  cbVisibility.Enabled := Enabled;
  cbAllday.Enabled := Enabled;
  StartDate.Enabled := Enabled;
  EndDate.Enabled := Enabled;
  StartTime.Enabled := Enabled;
  EndTime.Enabled := Enabled;
  Button5.Enabled := Enabled;
  Button6.Enabled := Enabled;
  Button7.Enabled := Enabled;
end;

function TForm1.ValidateForm: boolean;
begin
  Result := true;
  if Edit3.Text = '' then
  begin
    ShowMessage('Name field cannot be empty');
    Edit3.SetFocus;
    Result := false;
  end
  else if Edit4.Text = '' then
  begin
    ShowMessage('Description field cannot be empty');
    Edit4.SetFocus;
    Result := false;
  end;
end;

end.
