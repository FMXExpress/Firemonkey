unit UCloudAdapterDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TMSBaseControl, FMX.TMSPlannerBase, FMX.TMSPlannerData, FMX.TMSPlanner,
  FMX.TMSCloudCustomLive, FMX.TMSCloudLiveFMX, FMX.TMSCloudCustomLiveCalendar,
  FMX.TMSCloudLiveCalendar, FMX.TMSCloudBase, FMX.TMSCloudBaseFMX,
  FMX.TMSCloudCustomGoogle, FMX.TMSCloudGoogleFMX, FMX.TMSCloudCustomGCalendar,
  FMX.TMSCloudGCalendar, FMX.TMSBaseGroup, FMX.TMSRadioGroup,
  FMX.TMSPlannerLiveAdapter, FMX.TMSPlannerGoogleAdapter, FMX.ListBox;

type
  TForm1 = class(TForm)
    TMSFMXPlanner1: TTMSFMXPlanner;
    TMSFMXPlannerGoogleAdapter1: TTMSFMXPlannerGoogleAdapter;
    TMSFMXPlannerLiveAdapter1: TTMSFMXPlannerLiveAdapter;
    TMSFMXRadioGroup1: TTMSFMXRadioGroup;
    btnConnect: TButton;
    TMSFMXCloudGCalendar1: TTMSFMXCloudGCalendar;
    TMSFMXCloudLiveCalendar1: TTMSFMXCloudLiveCalendar;
    comboCal: TComboBox;
    btnGetCal: TButton;
    Label1: TLabel;
    procedure TMSFMXRadioGroup1RadioButtonChange(Sender: TObject;
      Index: Integer);
    procedure btnConnectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TMSFMXCloudGCalendar1ReceivedAccessToken(Sender: TObject);
    procedure TMSFMXCloudLiveCalendar1ReceivedAccessToken(Sender: TObject);
    procedure btnGetCalClick(Sender: TObject);
    procedure TMSFMXPlanner1AfterNavigateToDateTime(Sender: TObject;
      ADirection: TTMSFMXPlannerNavigationDirection; ACurrentDateTime,
      ANewDateTime: TDateTime);
    procedure TMSFMXPlanner1BeforeNavigateToDateTime(Sender: TObject;
      ADirection: TTMSFMXPlannerNavigationDirection;
      ACurrentDateTime: TDateTime; var ANewDateTime: TDateTime;
      var AAllow: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure InitGCalendars;
    procedure InitLiveCalendars;
    procedure Connect;
    procedure Disconnect;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

{$I APPIDS.INC}

procedure TForm1.btnConnectClick(Sender: TObject);
begin
  if btnConnect.Tag = 1 then
    Disconnect
  else
    Connect;
end;

procedure TForm1.btnGetCalClick(Sender: TObject);
var
  idx: integer;

begin
  idx := comboCal.ItemIndex;
  if idx < 0 then
    Exit;

  TMSFMXPlanner1.Enabled := true;

  case TMSFMXRadioGroup1.ItemIndex of
  0:
    begin
      TMSFMXPlanner1.Adapter := TMSFMXPlannerGoogleAdapter1;
      TMSFMXPlannerGoogleAdapter1.CalendarID := TMSFMXCloudGCalendar1.Calendars[idx].ID;
      TMSFMXPlannerGoogleAdapter1.Active := true;
    end;
  1:
    begin
      TMSFMXPlanner1.Adapter := TMSFMXPlannerLiveAdapter1;
      TMSFMXPlannerLiveAdapter1.CalendarID := TMSFMXCloudLiveCalendar1.Calendars[idx].ID;
      TMSFMXPlannerLiveAdapter1.Active := true;
    end;
  end;

end;

procedure TForm1.Connect;
var
  res: boolean;
begin
  case TMSFMXRadioGroup1.ItemIndex of
  0:
    begin
      res := TMSFMXCloudGCalendar1.TestTokens;

      if not res then
      begin
        TMSFMXCloudGCalendar1.RefreshAccess;
        res := TMSFMXCloudGCalendar1.TestTokens;
      end;

      if res then
      begin
        InitGCalendars
      end
      else
        TMSFMXCloudGCalendar1.DoAuth;
    end;
  1:
    begin
      res := TMSFMXCloudLiveCalendar1.TestTokens;

      if not res then
      begin
        TMSFMXCloudLiveCalendar1.RefreshAccess;
        res := TMSFMXCloudLiveCalendar1.TestTokens;
      end;

      if res then
        InitLiveCalendars
      else
        TMSFMXCloudLiveCalendar1.DoAuth;
    end;
  end;

end;

procedure TForm1.Disconnect;
begin
  btnConnect.Text := 'Connect';
  btnConnect.Tag := 0;
  btnGetCal.Enabled := false;
  comboCal.ItemIndex := -1;
  comboCal.Items.Clear;
  TMSFMXPlannerGoogleAdapter1.Active := false;
  TMSFMXPlannerLiveAdapter1.Active := false;
  TMSFMXPlanner1.Enabled := false;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // initialize cloud calendar access components
  TMSFMXCloudGCalendar1.App.Key := GAppkey;
  TMSFMXCloudGCalendar1.App.Secret := GAppSecret;

  TMSFMXCloudGCalendar1.PersistTokens.Location := plIniFile;
  TMSFMXCloudGCalendar1.PersistTokens.Key := '.\TOKENS.INI';
  TMSFMXCloudGCalendar1.PersistTokens.Section := 'GOOGLE';

  TMSFMXCloudLiveCalendar1.App.Key := LiveAppkey;
  TMSFMXCloudLiveCalendar1.App.Secret := LiveAppSecret;

  TMSFMXCloudLiveCalendar1.PersistTokens.Location := plIniFile;
  TMSFMXCloudLiveCalendar1.PersistTokens.Key := '.\TOKENS.INI';
  TMSFMXCloudLiveCalendar1.PersistTokens.Section := 'MSLIVE';

  TMSFMXCloudGCalendar1.LoadTokens;
  TMSFMXCloudLiveCalendar1.LoadTokens;

  TMSFMXPlanner1.ModeSettings.StartTime := Int(Now);
end;

procedure TForm1.InitGCalendars;
var
  i: integer;
begin
  // get Google calendars
  TMSFMXCloudGCalendar1.GetCalendars;

  comboCal.Items.Clear;

  for i := 0 to TMSFMXCloudGCalendar1.Calendars.Count - 1 do
    comboCal.Items.Add(TMSFMXCloudGCalendar1.Calendars[i].Summary);

  btnGetCal.Enabled := comboCal.Items.Count > 0;

  btnConnect.Text := 'Disconnect';
  btnConnect.Tag := 1;

  if comboCal.Items.Count > 0 then
  begin
    comboCal.ItemIndex := 0;
  end;
end;

procedure TForm1.InitLiveCalendars;
var
  i: integer;
begin
  // get Live calendars
  TMSFMXCloudLiveCalendar1.GetCalendars;

  comboCal.Items.Clear;

  for i := 0 to TMSFMXCloudLiveCalendar1.Calendars.Count - 1 do
    comboCal.Items.Add(TMSFMXCloudLiveCalendar1.Calendars[i].Summary);

  btnGetCal.Enabled := comboCal.Items.Count > 0;

  btnConnect.Text := 'Disconnect';
  btnConnect.Tag := 1;

  if comboCal.Items.Count > 0 then
    comboCal.ItemIndex := 0;
end;

procedure TForm1.TMSFMXCloudGCalendar1ReceivedAccessToken(Sender: TObject);
begin
  TMSFMXCloudGCalendar1.SaveTokens;
  InitGCalendars;
end;

procedure TForm1.TMSFMXCloudLiveCalendar1ReceivedAccessToken(Sender: TObject);
begin
  TMSFMXCloudLiveCalendar1.SaveTokens;
  InitLiveCalendars;
end;

procedure TForm1.TMSFMXPlanner1AfterNavigateToDateTime(Sender: TObject;
  ADirection: TTMSFMXPlannerNavigationDirection; ACurrentDateTime,
  ANewDateTime: TDateTime);
begin
  case TMSFMXRadioGroup1.ItemIndex of
  0: TMSFMXPlannerGoogleAdapter1.Update;
  1: TMSFMXPlannerLiveAdapter1.Update;
  end;
end;

procedure TForm1.TMSFMXPlanner1BeforeNavigateToDateTime(Sender: TObject;
  ADirection: TTMSFMXPlannerNavigationDirection; ACurrentDateTime: TDateTime;
  var ANewDateTime: TDateTime; var AAllow: Boolean);
begin
  case ADirection of
    pndPrevious: ANewDateTime := ACurrentDateTime - 7;
    pndNext: ANewDateTime := ACurrentDateTime + 7;
  end;
end;

procedure TForm1.TMSFMXRadioGroup1RadioButtonChange(Sender: TObject;
  Index: Integer);
begin
  case Index of
  0: TMSFMXPlanner1.Adapter := TMSFMXPlannerGoogleAdapter1;
  1: TMSFMXPlanner1.Adapter := TMSFMXPlannerLiveAdapter1;
  end;
end;

end.
