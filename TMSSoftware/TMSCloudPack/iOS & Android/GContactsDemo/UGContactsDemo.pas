unit UGContactsDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TMSCloudBase, FMX.TMSCloudGContacts,
  FMX.Layouts, FMX.ListBox, FMX.Edit, FMX.Objects, FMX.TMSCloudImage, FMX.Memo, IOUtils,
  FMX.TMSCloudBaseFMX, FMX.TMSCloudCustomGoogle, FMX.TMSCloudGoogleFMX,
  FMX.TMSCloudCustomGContacts;

type
  TForm82 = class(TForm)
    ToolBar1: TToolBar;
    Button1: TButton;
    Button2: TButton;
    TMSFMXCloudGContacts1: TTMSFMXCloudGContacts;
    Button3: TButton;
    lbGroups: TListBox;
    Line1: TLine;
    Panel1: TPanel;
    Panel2: TPanel;
    ListBox1: TListBox;
    Button4: TButton;
    edGroupName: TEdit;
    TMSFMXCloudImage1: TTMSFMXCloudImage;
    Label11: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    lbName: TLabel;
    meNotes: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure TMSFMXCloudGContacts1ReceivedAccessToken(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure lbGroupsChange(Sender: TObject);
    procedure btGroupAddClick(Sender: TObject);
    procedure btGroupUpdateClick(Sender: TObject);
    procedure btGroupDeleteClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ListBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Connected: boolean;
    Inserting: boolean;
    procedure FillContacts;
    procedure FillContactDetails;
    procedure FillGroups(Update: Boolean);
    procedure ToggleControls;
    procedure ClearControls;
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

procedure TForm82.btGroupAddClick(Sender: TObject);
var
  gr: TGGroup;
begin
  gr := TMSFMXCloudGContacts1.Groups.Add;
  gr.Summary := edGroupName.Text;
  TMSFMXCloudGContacts1.AddGroup(gr);
  FillGroups(False);
end;

procedure TForm82.btGroupDeleteClick(Sender: TObject);
var
  gg: TGGroup;
  buttonSelected: integer;
begin
  if lbGroups.ItemIndex >= 0 then
  begin
    buttonSelected := MessageDlg('Are you sure you want to delete the selected Group?', TMsgDlgType.mtConfirmation, mbOKCancel, 0);

    if buttonSelected = mrOk then
    begin
      gg := lbGroups.Items.Objects[lbGroups.ItemIndex] as TGGroup;
      TMSFMXCloudGContacts1.DeleteGroup(gg);
      FillGroups(False);
    end;
  end
  else
    ShowMessage('Please select a Group first');
end;

procedure TForm82.btGroupUpdateClick(Sender: TObject);
var
  gg: TGGroup;
begin
  if lbGroups.ItemIndex >= 0 then
  begin
    gg := lbGroups.Items.Objects[lbGroups.ItemIndex] as TGGroup;
    gg.Summary := edGroupName.Text;
    TMSFMXCloudGContacts1.UpdateGroup(gg);
    FillGroups(False);
  end
  else
    ShowMessage('Please select a Group first');
end;

procedure TForm82.Button1Click(Sender: TObject);
var
  acc: boolean;
begin
  TMSFMXCloudGContacts1.App.Key := GAppkey;
  TMSFMXCloudGContacts1.App.Secret := GAppSecret;
  TMSFMXCloudGContacts1.Logging := true;

  if TMSFMXCloudGContacts1.App.Key <> '' then
  begin
    TMSFMXCloudGContacts1.PersistTokens.Key := TPath.GetDocumentsPath + '/google.ini';
    TMSFMXCloudGContacts1.PersistTokens.Section := 'tokens';
    TMSFMXCloudGContacts1.LoadTokens;

    acc := TMSFMXCloudGContacts1.TestTokens;

    if not acc then
    begin
      TMSFMXCloudGContacts1.RefreshAccess;
      TMSFMXCloudGContacts1.DoAuth;
    end
    else
    begin
      Connected := true;
      ToggleControls;
    end;
  end
  else
    ShowMessage('Please provide a valid application ID for the client component');
end;

procedure TForm82.Button2Click(Sender: TObject);
begin
  TMSFMXCloudGContacts1.ClearTokens;
  Connected := false;
  ClearControls;
  lbGroups.Items.Clear;
  ListBox1.Clear;
  ToggleControls;
end;

procedure TForm82.Button3Click(Sender: TObject);
begin
  FillGroups(True);
end;

procedure TForm82.Button4Click(Sender: TObject);
begin
  FillContacts;
end;

procedure TForm82.ClearControls;
begin

end;

procedure TForm82.FillContactDetails;
var
  gc: TGContact;
begin
  if Assigned(ListBox1.Selected) then
  begin
    gc := ListBox1.Selected.Data as TGContact;
    lbName.Text := gc.FirstName + ' ' +gc.LastName;
    meNotes.Lines.Text := gc.Notes;
    Label6.Text := gc.Company + ' ' +gc.JobTitle;
    Label5.Text := DateToStr(gc.BirthDay);
    TMSFMXCloudImage1.URL := gc.ImageURL;
  end;
end;

procedure TForm82.FillContacts;
var
  i: integer;
  str: string;
begin
  ListBox1.Items.Clear;
  TMSFMXCloudgcontacts1.GetContacts;
  ListBox1.BeginUpdate;
  for i := 0 to TMSFMXCloudgcontacts1.Contacts.Count - 1 do
  begin
    str := Trim(TMSFMXCloudgcontacts1.Contacts[i].Title);
    if str <> '' then
      listbox1.Items.AddObject(str, TMSFMXCloudGContacts1.Contacts[i]);
  end;
  ListBox1.EndUpdate;
end;

procedure TForm82.FillGroups(Update: Boolean);
var
  i: integer;
begin
  if Update then
    TMSFMXCloudgcontacts1.GetContactGroups;
  lbGroups.Items.Clear;
  for i := 0 to TMSFMXCloudgcontacts1.Groups.Count - 1 do
    lbGroups.Items.AddObject(TMSFMXCloudgcontacts1.Groups[i].Summary, TMSFMXCloudGContacts1.Groups[i]);
end;

procedure TForm82.FormCreate(Sender: TObject);
begin
  Connected := False;
  ToggleControls;
end;

procedure TForm82.lbGroupsChange(Sender: TObject);
var
  gg: TGGroup;
begin
  if lbGroups.ItemIndex >= 0 then
  begin
    gg := lbGroups.Items.Objects[lbGroups.ItemIndex] as TGGroup;
    edGroupName.Text := gg.Summary;
  end;
end;

procedure TForm82.ListBox1Change(Sender: TObject);
begin
  FillContactDetails;
end;

procedure TForm82.TMSFMXCloudGContacts1ReceivedAccessToken(Sender: TObject);
begin
  TMSFMXCloudGContacts1.SaveTokens;
  Connected := true;
  ToggleControls;
end;

procedure TForm82.ToggleControls;
begin
  Panel1.Enabled := Connected;
  Panel2.Enabled := Connected;
  Button1.Enabled := not Connected;
  Button2.Enabled := Connected;
end;

end.
