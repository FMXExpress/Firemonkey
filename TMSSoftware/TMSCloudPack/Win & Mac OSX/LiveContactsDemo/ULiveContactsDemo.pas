unit ULiveContactsDemo;

interface

uses
  FMX.TMSCloudBase, FMX.TMSCloudLiveContacts, FMX.Controls,
  FMX.Edit, FMX.ExtCtrls, FMX.ListBox, FMX.StdCtrls, FMX.Grid, FMX.Layouts, SysUtils,
  FMX.TMSCloudListView, FMX.Objects, System.Classes, FMX.Types, FMX.Forms,
  FMX.TMSCloudBaseFMX, FMX.TMSCloudCustomLive, FMX.TMSCloudLiveFMX,
  FMX.TMSCloudCustomLiveContacts;

type

  TForm1 = class(TForm)
StyleBook1: TStyleBook;

    TMSFMXCloudLiveContacts1: TTMSFMXCloudLiveContacts;
    Panel1: TPanel;
    Button1: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label6: TLabel;
    edFirstName: TEdit;
    btAdd: TButton;
    edLastName: TEdit;
    cbGender: TComboBox;
    CloudListView1: TTMSFMXCloudListView;
    dpBirthDay: TCalendarEdit;
    Label4: TLabel;
    Image1: TImage;
    btRemove: TButton;
    procedure Button1Click(Sender: TObject);
    procedure TMSFMXCloudLiveContacts1ReceivedAccessToken(Sender: TObject);
    procedure btAddClick(Sender: TObject);
    procedure GetContacts();
    procedure ClearControls;
    procedure ToggleControls;
    procedure Init;
    procedure FormCreate(Sender: TObject);
    procedure btRemoveClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Connected: boolean;

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

procedure TForm1.TMSFMXCloudLiveContacts1ReceivedAccessToken(Sender: TObject);
begin
  TMSFMXCloudLiveContacts1.SaveTokens;
  Init;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  TMSFMXCloudLiveContacts1.App.Key := LiveAppkey;
  TMSFMXCloudLiveContacts1.App.Secret := LiveAppSecret;
  TMSFMXCloudLiveContacts1.Logging := true;

  if not TMSFMXCloudLiveContacts1.TestTokens then
    TMSFMXCloudLiveContacts1.RefreshAccess;

  if not TMSFMXCloudLiveContacts1.TestTokens then
    TMSFMXCloudLiveContacts1.DoAuth
  else
    Init;
end;

procedure TForm1.btAddClick(Sender: TObject);
var
  li: TLiveContactItem;
begin
  li := TMSFMXCloudLiveContacts1.Items.Add;

  li.FirstName := edFirstName.Text;
  li.LastName := edLastName.Text;

  if cbGender.ItemIndex = 0 then
    li.Gender := geNotSpecified
  else if cbGender.ItemIndex = 1 then
    li.Gender := geFemale
  else if cbGender.ItemIndex = 2 then
    li.Gender := geMale;

  li.BirthDay := StrToInt(FormatDateTime('dd', dpBirthDay.Date));
  li.BirthMonth := StrToInt(FormatDateTime('MM', dpBirthDay.Date));

  TMSFMXCloudLiveContacts1.Add(li);

  GetContacts;
  ClearControls;
end;

procedure TForm1.btRemoveClick(Sender: TObject);
begin
  TMSFMXCloudLiveContacts1.ClearTokens;
  Connected := false;
  ToggleControls;
end;

procedure TForm1.ClearControls;
begin
  edFirstName.Text := '';
  edLastName.Text := '';
  cbGender.ItemIndex := 0;
  dpBirthDay.Date := Now;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  TMSFMXCloudLiveContacts1.PersistTokens.Key := ExtractFilePath(ParamStr(0)) + 'live.ini';
  TMSFMXCloudLiveContacts1.PersistTokens.Section := 'tokens';
  TMSFMXCloudLiveContacts1.LoadTokens;

  Connected := False;
  ToggleControls;
  ClearControls;
end;

procedure TForm1.GetContacts;
var
  I: integer;
  li: TListItem;
begin
 TMSFMXCloudLiveContacts1.GetContacts;

 CloudListView1.Items.Clear;
 for I := 0 to TMSFMXCloudLiveContacts1.Items.Count - 1 do
 begin
    li := CloudListView1.Items.Add;
    li.Text  := TMSFMXCloudLiveContacts1.Items[I].FirstName + ' ' + TMSFMXCloudLiveContacts1.Items[I].LastName;
    li.SubItems.Add(IntToStr(TMSFMXCloudLiveContacts1.Items[I].BirthDay) + '/' + IntToStr(TMSFMXCloudLiveContacts1.Items[I].BirthMonth));

    if TMSFMXCloudLiveContacts1.Items[I].IsFavorite then
      li.SubItems.Add('Yes')
    else
      li.SubItems.Add('No');

    if TMSFMXCloudLiveContacts1.Items[I].Gender = geFemale then
      li.SubItems.Add('Female')
    else if TMSFMXCloudLiveContacts1.Items[I].Gender = geMale then
      li.SubItems.Add('Male')
    else
      li.SubItems.Add('');

    li.Data := TMSFMXCloudLiveContacts1.Items[I];
 end;
end;

procedure TForm1.Init;
begin
  Connected := True;
  ToggleControls;
  ClearControls;
  GetContacts;
end;

procedure TForm1.ToggleControls;
begin
  edFirstName.Enabled := Connected;
  edLastName.Enabled := Connected;
  cbGender.Enabled := Connected;
  dpBirthDay.Enabled := Connected;
  btAdd.Enabled := Connected;
  btRemove.Enabled := Connected;
  Button1.Enabled := not Connected;
end;

end.
