unit ULiveCalendarDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.ListBox,
  FMX.TMSCloudBase, FMX.TMSCloudLiveCalendar, FMX.Layouts,
  FMX.Edit, FMX.ExtCtrls, DateUtils, FMX.Objects, IOUtils, FMX.TMSCloudBaseFMX,
  FMX.TMSCloudCustomLive, FMX.TMSCloudLiveFMX,
  FMX.TMSCloudCustomLiveCalendar, FMX.DateTimeCtrls;

type
  TForm82 = class(TForm)
    ToolBar1: TToolBar;
    Button1: TButton;
    Button2: TButton;
    ComboBox1: TComboBox;
    ListBox1: TListBox;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    CheckBox1: TCheckBox;
    Label4: TLabel;
    CalendarEdit1: TCalendarEdit;
    CalendarEdit2: TCalendarEdit;
    Label5: TLabel;
    Edit2: TEdit;
    SpeedButton6: TSpeedButton;
    Image6: TImage;
    SpeedButton7: TSpeedButton;
    Image7: TImage;
    SpeedButton1: TSpeedButton;
    Image1: TImage;
    TMSFMXCloudLiveCalendar1: TTMSFMXCloudLiveCalendar;
    procedure Button1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TMSFMXCloudLiveCalendar1ReceivedAccessToken(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
    Connected: Boolean;
  public
    { Public declarations }
    procedure FillCalendars;
    procedure FillCalendarItems;
    procedure Init;
    procedure ClearControls;
    procedure ToggleControls;
    procedure SetCalendarItem(Item: TLiveCalendarItem);
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
//  LiveAppkey = 'xxxxxxxxx';
//  LiveAppSecret = 'yyyyyyyy';


{$I APPIDS.INC}

procedure TForm82.Button1Click(Sender: TObject);
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

procedure TForm82.Button4Click(Sender: TObject);
begin
  ClearControls;
end;

procedure TForm82.SetCalendarItem(Item: TLiveCalendarItem);
begin
  Item.Summary := Edit1.Text;
  Item.Description := Edit2.Text;
  Item.Location := '';

  Item.StartTime := CalendarEdit1.Date;
  Item.EndTime := CalendarEdit2.Date;
  Item.IsAllDay := CheckBox1.IsChecked;
end;

procedure TForm82.SpeedButton1Click(Sender: TObject);
var
  li: TLiveCalendarItem;
begin
  if Assigned(ListBox1.Selected) then
  begin
    li := (ListBox1.Selected.Data as TLiveCalendarItem);
    SetCalendarItem(li);
    TMSFMXCloudLiveCalendar1.Update(li);
    FillCalendarItems;
  end;
end;

procedure TForm82.Button5Click(Sender: TObject);
var
  li: TLiveCalendarItem;
begin
  li := TMSFMXCloudLiveCalendar1.Items.Add;
  SetCalendarItem(li);
  li.CalendarID := (ComboBox1.Items.Objects[ComboBox1.ItemIndex] as TLiveCalendar).ID;
  TMSFMXCloudLiveCalendar1.Add(li);
  FillCalendarItems;
end;

procedure TForm82.ClearControls;
begin
  Edit1.Text := '';
  Edit2.Text := '';
  CheckBox1.IsChecked := False;
  CalendarEdit1.Date := Now;
  CalendarEdit2.Date := Now;
end;

procedure TForm82.ComboBox1Change(Sender: TObject);
begin
  ClearControls;
  FillCalendarItems;
end;

procedure TForm82.FillCalendarItems;
var
  I: Integer;
  LiveCalendar: TLiveCalendar;
begin
  if ComboBox1.ItemIndex >= 0 then
  begin
    LiveCalendar := (ComboBox1.Items.Objects[ComboBox1.ItemIndex] as TLiveCalendar);

    TMSFMXCloudLiveCalendar1.GetCalendar(LiveCalendar.ID);

    ListBox1.Items.Clear;
    for I := 0 to TMSFMXCloudLiveCalendar1.Items.Count - 1 do
      ListBox1.Items.AddObject(TMSFMXCloudLiveCalendar1.Items[I].Summary, TMSFMXCloudLiveCalendar1.Items[I]);
  end;
end;

procedure TForm82.FillCalendars;
var
  I: Integer;
begin
  TMSFMXCloudLiveCalendar1.GetCalendars;

  ComboBox1.Items.Clear;

  for I := 0 to TMSFMXCloudLiveCalendar1.Calendars.Count - 1 do
  begin
    ComboBox1.Items.AddObject(TMSFMXCloudLiveCalendar1.Calendars[I].Summary, TMSFMXCloudLiveCalendar1.Calendars[I]);
  end;
  ComboBox1.ItemIndex := 0;
end;

procedure TForm82.FormCreate(Sender: TObject);
begin
  CalendarEdit1.Date := Now;
  CalendarEdit2.Date := Now;
  TMSFMXCloudLiveCalendar1.PersistTokens.Key := TPath.GetDocumentsPath + '/live.ini';
  TMSFMXCloudLiveCalendar1.PersistTokens.Section := 'tokens';
  TMSFMXCloudLiveCalendar1.LoadTokens;

  ClearControls;

  Connected := false;
  ToggleControls;
end;

procedure TForm82.Init;
begin
  Connected := true;
  ToggleControls;
  FillCalendars;
  FillCalendarItems;
end;

procedure TForm82.TMSFMXCloudLiveCalendar1ReceivedAccessToken(Sender: TObject);
begin
  TMSFMXCloudLiveCalendar1.SaveTokens;
  Init;
end;

procedure TForm82.ToggleControls;
begin
  SpeedButton6.Enabled := Connected;
  SpeedButton7.Enabled := Connected;
  SpeedButton1.Enabled := Connected;
  ComboBox1.Enabled := Connected;
  Edit1.Enabled := Connected;
  ListBox1.Enabled := Connected;
  CheckBox1.Enabled := Connected;
  CalendarEdit1.Enabled := Connected;
  CalendarEdit2.Enabled := Connected;
  Edit2.Enabled := Connected;
  Button1.Enabled := not Connected;
end;

end.
