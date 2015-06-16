unit ULinkedInDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TMSCloudBase, FMX.TMSCloudLinkedIn, FMX.TabControl,
  FMX.Layouts, FMX.ListBox, FMX.Objects, FMX.TMSCloudImage, FMX.Edit, FMX.Memo, IOUtils,
  FMX.TMSCloudBaseFMX, FMX.TMSCloudCustomLinkedIn;

type
  TForm82 = class(TForm)
    ToolBar1: TToolBar;
    Button1: TButton;
    Button2: TButton;
    TMSFMXCloudLinkedIn1: TTMSFMXCloudLinkedIn;
    TMSFMXCloudImage1: TTMSFMXCloudImage;
    lbConnections: TListBox;
    Button3: TButton;
    Title: TEdit;
    Image: TEdit;
    Link: TEdit;
    Descr: TEdit;
    Button5: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Button4: TButton;
    Label6: TLabel;
    Edit1: TEdit;
    Button6: TButton;
    Label5: TLabel;
    Memo1: TMemo;
    Label7: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure TMSFMXCloudLinkedIn1ReceivedAccessToken(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure lbConnectionsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Connected: boolean;
    resultpage: integer;
    resultcount: integer;
    pagesize: integer;
    procedure ToggleControls;
    procedure DisplayProfile(Profile: TLinkedInProfile);
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
//  LinkedInAppkey = 'xxxxxxxxx';
//  LinkedInAppSecret = 'yyyyyyyy';

{$I APPIDS.INC}

procedure TForm82.Button1Click(Sender: TObject);
var
  acc: boolean;
begin
  TMSFMXCloudLinkedIn1.App.Key := LinkedInAppKey;
  TMSFMXCloudLinkedIn1.App.Secret := LinkedInAppSecret;

  if TMSFMXCloudLinkedIn1.App.Key <> '' then
  begin
    TMSFMXCloudLinkedIn1.PersistTokens.Key := TPath.GetDocumentsPath + '/linkedin.ini';
    TMSFMXCloudLinkedIn1.PersistTokens.Section := 'tokens';
    TMSFMXCloudLinkedIn1.LoadTokens;

    acc := TMSFMXCloudLinkedIn1.TestTokens;

    if not acc then
    begin
      TMSFMXCloudLinkedIn1.RefreshAccess;
      TMSFMXCloudLinkedIn1.DoAuth;
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
  TMSFMXCloudLinkedIn1.ClearTokens;
  Connected := false;
  ToggleControls;
end;

procedure TForm82.Button3Click(Sender: TObject);
var
  i: integer;
begin
  TMSFMXCloudLinkedIn1.GetConnections;

  lbConnections.Items.Clear;

  for i := 0 to TMSFMXCloudLinkedIn1.Connections.Count - 1 do
    lbConnections.Items.AddObject(TMSFMXCloudLinkedIn1.Connections.Items[i].Profile.FirstName + ' ' + TMSFMXCloudLinkedIn1.Connections.Items[i].Profile.LastName, TMSFMXCloudLinkedIn1.Connections.Items[i]);
end;

procedure TForm82.Button4Click(Sender: TObject);
begin
  TMSFMXCloudLinkedIn1.Activity(Edit1.Text)
end;

procedure TForm82.Button5Click(Sender: TObject);
begin
  TMSFMXCloudLinkedIn1.Share(Title.Text, Descr.Text, Link.Text, Image.Text);
end;

procedure TForm82.Button6Click(Sender: TObject);
var
  lp: TLinkedInProfile;
begin
  TMSFMXCloudLinkedIn1.GetDefaultProfile;
  lp := TMSFMXCloudLinkedIn1.DefaultProfile;
  DisplayProfile(lp);
  TMSFMXCloudImage1.URL := lp.PictureURL;
end;

procedure TForm82.DisplayProfile(Profile: TLinkedInProfile);
begin
  memo1.Lines.Text :=
    'FormattedName: ' + Profile.FormattedName + #13 +
    'Headline: ' + Profile.Headline + #13 +
    'Summary: ' + Profile.Summary + #13 +
    'Email: ' + Profile.EmailAddress + #13 +
    'PublicProfileURL: ' + Profile.PublicProfileURL + #13 +
    'Location: ' + Profile.Location + #13 +
    'CountryCode: ' + Profile.CountryCode;
end;

procedure TForm82.FormCreate(Sender: TObject);
begin
  Connected := False;
  ToggleControls;
end;

procedure TForm82.lbConnectionsChange(Sender: TObject);
var
  lp: TLinkedInProfile;
  cn: TLinkedInConnection;
begin
  if Assigned(lbConnections.Selected) then
  begin
    cn := (lbConnections.Selected.Data as TLinkedInConnection);
    lp := TMSFMXCloudLinkedIn1.GetProfile(cn.Profile.ID);
    TMSFMXCloudImage1.URL := lp.PictureURL;
    DisplayProfile(lp);
  end;
end;

procedure TForm82.TMSFMXCloudLinkedIn1ReceivedAccessToken(Sender: TObject);
begin
  TMSFMXCloudLinkedIn1.SaveTokens;
  Connected := true;
  ToggleControls;
end;

procedure TForm82.ToggleControls;
begin
  Button1.Enabled := not Connected;
  Button2.Enabled := Connected;
  Button3.Enabled := Connected;
  Button4.Enabled := Connected;
  Button5.Enabled := Connected;
  Button6.Enabled := Connected;
  Edit1.Enabled := Connected;
  Title.Enabled := Connected;
  Link.Enabled := Connected;
  Memo1.Enabled := Connected;
  Image.Enabled := Connected;
  Descr.Enabled := Connected;
  lbConnections.Enabled := Connected;
end;

end.
