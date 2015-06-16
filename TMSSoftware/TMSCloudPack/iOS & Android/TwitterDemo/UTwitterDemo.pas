unit UTwitterDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TMSCloudBase, FMX.TMSCloudTwitter, FMX.Layouts,
  FMX.ListBox, FMX.Edit, IOUtils, FMX.TMSCloudBaseFMX, FMX.TMSCloudCustomTwitter;

type
  TForm82 = class(TForm)
    ToolBar1: TToolBar;
    Button1: TButton;
    Button2: TButton;
    TMSFMXCloudTwitter1: TTMSFMXCloudTwitter;
    ListBox1: TListBox;
    ListBox2: TListBox;
    ListBox3: TListBox;
    Button6: TButton;
    Button7: TButton;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    SpeedButton3: TSpeedButton;
    Edit1: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure TMSFMXCloudTwitter1ReceivedAccessToken(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button7Click(Sender: TObject);
  private
    { Private declarations }
    Connected: Boolean;
  public
    { Public declarations }
    procedure FillFollowers;
    procedure FillFriends;
    procedure LoadTweets;
    procedure ToggleControls;
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
//  TwitterAppKey = 'xxxxxxxxx';
//  TwitterAppSecret = 'yyyyyyyyy';


{$I APPIDS.INC}

procedure TForm82.Button1Click(Sender: TObject);
var
  acc: boolean;
begin
  TMSFMXCloudTwitter1.App.Key := TwitterAppkey;
  TMSFMXCloudTwitter1.App.Secret := TwitterAppSecret;

  if TMSFMXCloudTwitter1.App.Key <> '' then
  begin
    TMSFMXCloudTwitter1.PersistTokens.Key := TPath.GetDocumentsPath + '/twitter.ini';
    TMSFMXCloudTwitter1.PersistTokens.Section := 'tokens';
    TMSFMXCloudTwitter1.LoadTokens;

    acc := TMSFMXCloudTwitter1.TestTokens;

    if not acc then
    begin
      TMSFMXCloudTwitter1.RefreshAccess;
      TMSFMXCloudTwitter1.DoAuth;
      TMSFMXCloudTwitter1.GetAccountInfo;
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
  TMSFMXCloudTwitter1.ClearTokens;
  Connected := false;
  ToggleControls;
end;

procedure TForm82.Button3Click(Sender: TObject);
begin
  FillFollowers;
end;

procedure TForm82.Button4Click(Sender: TObject);
begin
  FillFriends;
end;

procedure TForm82.Button5Click(Sender: TObject);
begin
  LoadTweets;
end;

procedure TForm82.Button6Click(Sender: TObject);
begin
  TMSFMXCloudTwitter1.Tweet(Edit1.Text);
  ListBox3.Items.Clear;
  TMSFMXCloudTwitter1.Statuses.Clear;
  LoadTweets;
end;

procedure TForm82.Button7Click(Sender: TObject);
begin
  TMSFMXCloudTwitter1.TweetWithMedia(Edit1.Text, TPath.GetDocumentsPath + '/sample.jpg');
  ListBox3.Items.Clear;
  TMSFMXCloudTwitter1.Statuses.Clear;
  LoadTweets;
end;

procedure TForm82.FillFollowers;
var
  i: integer;
begin
  TMSFMXCloudTwitter1.GetFollowers;
  TMSFMXCloudTwitter1.GetProfileListInfo(TMSFMXCloudTwitter1.Followers);

  ListBox1.Items.Clear;
  for i := 0 to TMSFMXCloudTwitter1.Followers.Count - 1 do
    listbox1.Items.AddObject(TMSFMXCloudTwitter1.Followers.Items[i].Name, TMSFMXCloudTwitter1.Followers.Items[i]);
end;

procedure TForm82.FillFriends;
var
  i: integer;
begin
  TMSFMXCloudTwitter1.GetFriends;
  TMSFMXCloudTwitter1.GetProfileListInfo(TMSFMXCloudTwitter1.Friends);

  ListBox2.Items.Clear;
  for i := 0 to TMSFMXCloudTwitter1.Friends.Count - 1 do
    listbox2.Items.AddObject(TMSFMXCloudTwitter1.Friends.Items[i].Name, TMSFMXCloudTwitter1.Friends.Items[i]);
end;

procedure TForm82.FormCreate(Sender: TObject);
begin
  Connected := False;
  ToggleControls;
  {$IFDEF IOS}
  TFile.Copy(ExtractFilePath(Paramstr(0)) + 'sample.jpg', TPath.GetDocumentsPath + '/sample.jpg', True);
  {$ENDIF}
end;

procedure TForm82.LoadTweets;
var
  i: integer;
  TwitterStatus: TTWitterStatus;
begin
  if ListBox3.Items.Count = 0 then
    TMSFMXCloudTwitter1.GetStatuses(10)
  else
  begin
    TWitterStatus := TTwitterStatus(ListBox3.Items.Objects[ListBox3.Items.Count - 1]);
    TMSFMXCloudTwitter1.GetStatuses(10, -1, TwitterStatus.ID);
  end;

  for i := ListBox3.Items.Count to TMSFMXCloudTwitter1.Statuses.Count - 1 do
    ListBox3.Items.AddObject(TMSFMXCloudTwitter1.Statuses.Items[i].Text, TMSFMXCloudTwitter1.Statuses.Items[i]);
end;

procedure TForm82.TMSFMXCloudTwitter1ReceivedAccessToken(Sender: TObject);
begin
  TMSFMXCloudTwitter1.SaveTokens;
  TMSFMXCloudTwitter1.GetAccountInfo;
  Connected := true;
  ToggleControls;
end;

procedure TForm82.ToggleControls;
begin
  Edit1.Enabled := Connected;
  Button1.Enabled := not Connected;
  Button2.Enabled := Connected;
  SpeedButton3.Enabled := Connected;
  SpeedButton1.Enabled := Connected;
  SpeedButton2.Enabled := Connected;
  Button6.Enabled := Connected;
  ListBox1.Enabled := Connected;
  ListBox2.Enabled := Connected;
  ListBox3.Enabled := Connected;
end;

end.
