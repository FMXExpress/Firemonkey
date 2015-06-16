unit UGCalendarDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.ExtCtrls,
  FMX.TMSCloudBase, FMX.TMSCloudGCalendar, FMX.ListBox,
  FMX.Layouts, DateUtils, FMX.Edit, FMX.Objects, IOUtils, FMX.TMSCloudBaseFMX,
  FMX.TMSCloudCustomGoogle, FMX.TMSCloudGoogleFMX, FMX.TMSCloudCustomGCalendar,
  FMX.DateTimeCtrls;

type
  TForm82 = class(TForm)
    ToolBar1: TToolBar;
    Button1: TButton;
    Button2: TButton;
    TMSFMXCloudGCalendar1: TTMSFMXCloudGCalendar;
    ComboBox1: TComboBox;
    ListBox1: TListBox;
    Button3: TButton;
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
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure TMSFMXCloudGCalendar1ReceivedAccessToken(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ListBox1Change(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Connected: boolean;
    Inserting: boolean;
    procedure FillCalendars;
    procedure FillCalendarItems;
    procedure FillCalendarItemDetails;
    procedure ToggleControls;
    procedure ClearControls;
    procedure Init;
    procedure SetCalendarItem(Item: TGCalendarItem);
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
begin
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

procedure TForm82.Button2Click(Sender: TObject);
begin
  TMSFMXCloudGCalendar1.ClearTokens;
  Connected := false;
  ToggleControls;
end;

procedure TForm82.SetCalendarItem(Item: TGCalendarItem);
begin
  Item.Summary := Edit1.Text;
  Item.Description := Edit2.Text;
  Item.StartTime := CalendarEdit1.Date;
  Item.EndTime := CalendarEdit2.Date;
  Item.IsAllDay := CheckBox1.IsChecked;
end;

procedure TForm82.Button3Click(Sender: TObject);
var
  li: TGCalendarItem;
begin
  li := TMSFMXCloudGCalendar1.Items.Add;
  SetCalendarItem(li);
  li.CalendarID := (ComboBox1.Items.Objects[ComboBox1.ItemIndex] as TGCalendar).ID;
  TMSFMXCloudGCalendar1.Add(li);

  FillCalendarItems;
end;

procedure TForm82.Button4Click(Sender: TObject);
begin
  ClearControls;
end;

procedure TForm82.Button5Click(Sender: TObject);
var
  li: TGCalendarItem;
begin
  if Assigned(ListBox1.Selected) then
  begin
    li := (ListBox1.Selected.Data as TGCalendarItem);
    SetCalendarItem(li);
    TMSFMXCloudGCalendar1.Update(li);
    FillCalendarItems;
  end;
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
  FillCalendarItems;
end;

procedure TForm82.FillCalendarItemDetails;
var
  li: TGCalendarItem;
begin
  if Assigned(ListBox1.Selected) then
  begin
    li := TGCalendarItem(ListBox1.Selected.Data);

    Edit2.Text := li.Description;
    Edit1.Text := li.Summary;
    CalendarEdit1.Date := li.StartTime;
    CalendarEdit2.Date := li.EndTime;
    CheckBox1.IsChecked := li.IsAllDay;
  end;
end;

procedure TForm82.FillCalendarItems;
var
  I: Integer;
  gcal: TGCalendar;
begin
  if ComboBox1.ItemIndex >= 0 then
  begin
    gcal := (ComboBox1.Items.Objects[ComboBox1.ItemIndex] as TGCalendar);
    TMSFMXCloudGCalendar1.GetCalendar(gcal.ID);

    ListBox1.BeginUpdate;
    ListBox1.Items.Clear;
    for I := 0 to TMSFMXCloudGCalendar1.Items.Count - 1 do
      ListBox1.Items.AddObject(TMSFMXCloudGCalendar1.Items[I].Summary, TMSFMXCloudGCalendar1.Items[I]);
    ListBox1.EndUpdate;
  end;
end;

procedure TForm82.FillCalendars;
var
  I: Integer;
begin
  TMSFMXCloudGCalendar1.GetCalendars();

  ComboBox1.BeginUpdate;
  ComboBox1.Items.Clear;
  for I := 0 to TMSFMXCloudGCalendar1.Calendars.Count - 1 do
    ComboBox1.Items.AddObject(TMSFMXCloudGCalendar1.Calendars[I].Summary, TMSFMXCloudGCalendar1.Calendars[I]);
  ComboBox1.EndUpdate;
  ComboBox1.ItemIndex := 0;

end;

procedure TForm82.FormCreate(Sender: TObject);
begin
  CalendarEdit1.Date := Now;
  CalendarEdit2.Date := Now;
  TMSFMXCloudGCalendar1.PersistTokens.Key := TPath.GetDocumentsPath +  '/google.ini';
  TMSFMXCloudGCalendar1.PersistTokens.Section := 'tokens';
  TMSFMXCloudGCalendar1.LoadTokens;
  ClearControls;
  Inserting := false;
  Connected := false;
  ToggleControls;
end;

procedure TForm82.Init;
begin
  Connected := true;
  FillCalendars;
  FillCalendarItems;
  ToggleControls;
end;

procedure TForm82.ListBox1Change(Sender: TObject);
begin
  FillCalendarItemDetails;
end;

procedure TForm82.TMSFMXCloudGCalendar1ReceivedAccessToken(Sender: TObject);
begin
  TMSFMXCloudGCalendar1.SaveTokens;
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
  Button1.Enabled := not Connected;
  Edit2.Enabled := Connected;
end;

end.
