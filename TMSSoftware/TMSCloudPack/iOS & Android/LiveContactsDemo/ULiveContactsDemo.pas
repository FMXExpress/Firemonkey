unit ULiveContactsDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TMSCloudBase, FMX.TMSCloudLiveContacts,
  FMX.Layouts, FMX.ListBox, FMX.Edit, FMX.Objects, FMX.TMSCloudImage, FMX.Memo, IOUtils,
  FMX.TMSCloudBaseFMX, FMX.TMSCloudCustomLive, FMX.TMSCloudLiveFMX,
  FMX.TMSCloudCustomLiveContacts;

type
  TForm82 = class(TForm)
    ToolBar1: TToolBar;
    Button1: TButton;
    Button2: TButton;
    Panel2: TPanel;
    ListBox1: TListBox;
    Button4: TButton;
    TMSFMXCloudLiveContacts1: TTMSFMXCloudLiveContacts;
    procedure Button1Click(Sender: TObject);
    procedure TMSFMXCloudLiveContacts1ReceivedAccessToken(Sender: TObject);
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
//  LiveAppkey = 'xxxxxxxxx';
//  LiveAppSecret = 'yyyyyyyy';

{$I APPIDS.INC}

procedure TForm82.Button1Click(Sender: TObject);
var
  acc: boolean;
begin
  TMSFMXCloudLiveContacts1.App.Key := LiveAppkey;
  TMSFMXCloudLiveContacts1.App.Secret := LiveAppSecret;
  TMSFMXCloudLiveContacts1.Logging := true;

  if TMSFMXCloudLiveContacts1.App.Key <> '' then
  begin
    TMSFMXCloudLiveContacts1.PersistTokens.Key := TPath.GetDocumentsPath + '/google.ini';
    TMSFMXCloudLiveContacts1.PersistTokens.Section := 'tokens';
    TMSFMXCloudLiveContacts1.LoadTokens;

    acc := TMSFMXCloudLiveContacts1.TestTokens;

    if not acc then
    begin
      TMSFMXCloudLiveContacts1.RefreshAccess;
      TMSFMXCloudLiveContacts1.DoAuth;
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
  TMSFMXCloudLiveContacts1.ClearTokens;
  Connected := false;
  ClearControls;
  ListBox1.Clear;
  ToggleControls;
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
  gc: TLiveContactItem;
begin
  if Assigned(ListBox1.Selected) then
  begin
    gc := ListBox1.Selected.Data as TLiveContactItem;
    ShowMessage(gc.FirstName + ' ' + gc.LastName);
  end;
end;

procedure TForm82.FillContacts;
var
  i: integer;
begin
  ListBox1.Items.Clear;
  TMSFMXCloudLiveContacts1.GetContacts;
  for i := 0 to TMSFMXCloudLiveContacts1.Items.Count - 1 do
    listbox1.Items.AddObject(TMSFMXCloudLivecontacts1.Items[i].FullName, TMSFMXCloudLiveContacts1.Items[i]);
end;

procedure TForm82.FormCreate(Sender: TObject);
begin
  Connected := False;
  ToggleControls;
end;

procedure TForm82.ListBox1Change(Sender: TObject);
begin
  FillContactDetails;
end;

procedure TForm82.TMSFMXCloudLiveContacts1ReceivedAccessToken(Sender: TObject);
begin
  TMSFMXCloudLiveContacts1.SaveTokens;
  Connected := true;
  ToggleControls;
end;

procedure TForm82.ToggleControls;
begin
  Panel2.Enabled := Connected;
  Button1.Enabled := not Connected;
  Button2.Enabled := Connected;
end;

end.
