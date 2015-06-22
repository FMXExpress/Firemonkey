unit frmContacts;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, FMX.ListBox, FMX.StdCtrls, FMX.ListView,
  CommonHeader, strutils,
  FmxJabberTools, FMX.TabControl, FMX.Layouts, FMX.Memo, FMX.Objects,
  frmAddContact;

type
  TFormContacts = class(TForm)
    ListViewContacts: TListView;
    lbDisplayName: TLabel;
    cmbAvailability: TComboBox;
    btnAddcontact: TButton;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabControl2: TTabControl;
    lbContactsList: TLabel;
    ImgUserstatus: TImage;
    procedure FormCreate(Sender: TObject);
    procedure btnAddcontactClick(Sender: TObject);
    procedure cmbAvailabilityChange(Sender: TObject);
    procedure ListViewContactsClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ListViewContactsDeletingItem(Sender: TObject; AIndex: Integer;
      var ACanDelete: Boolean);
  private
    procedure OnNewContactProc(AJID : string);
    procedure OnMessageReceivedProc(AFrom, AMessage : string);
    procedure OnAskTosendMessageProc(ATo, AMessage : string);
    function GetTabIndex(ATagString : string ) : integer;
    procedure CreateConversation(AContact : PContact); overload;
    procedure OnPResenceCallBack(AJid : String);
    procedure UpdateContactDisplay(AIdx : integer);
    procedure ContactAskToAddProc(Jid : string; var AAccept : Boolean);
    procedure OnAddContactStatusProc(AJid : string; AAccepted : Boolean);
  public
    procedure ShowContacts;
  end;

var
  FormContacts: TFormContacts;

implementation

{$R *.fmx}

uses frmConversation, frmConnect;

procedure TFormContacts.btnAddcontactClick(Sender: TObject);
var
  wID : string;
begin
  if InputQuery('Add new contact', 'Set user ID',wID) then
    GJabberClient.AddContact(wID,wID,'');
end;


procedure TFormContacts.cmbAvailabilityChange(Sender: TObject);
begin
  case cmbAvailability.ItemIndex of
    0 : GJabberClient.SetPresence(usOnline, '');
    1 : GJabberClient.SetPresence(usAway, '');
    2 : GJabberClient.SetPresence(usInVisible, '');
  end;
end;


procedure TFormContacts.ContactAskToAddProc(Jid : string; var AAccept: Boolean);
var wContactName : String;
begin
  AAccept := False;
  wContactName := jid;
  if MessageDlg(wContactName + ' want to add you to it''s contacts list, ok ?',TMsgDlgType.mtConfirmation,mbYesNo,0) = mrYes then
    AAccept := True;
end;

procedure TFormContacts.CreateConversation(AContact : PContact);
var
  wNewTab : TTabItem;
  wFrame : TFrameConversation;
begin
  try
    wNewTab := TTabItem.Create(TabControl2);
    wNewTab.Text := AContact.Jid;
    wNewTab.TagString := AContact.Jid;
    wFrame := TFrameConversation.Create(Self);
    wFrame.Name := 'Frame_' + inttostr(TabControl2.TabCount);
    wFrame.Parent := wNewTab;
    wFrame.MessageTo :=AContact.Jid;
    wFrame.OnSendMessage := OnAskTosendMessageProc;
    wFrame.Initialize(AContact.Jid);
    TabControl2.AddObject(wNewTab);
    wFrame.Align := TAlignLayout.alClient;
  except
    On E:Exception do
      Raise Exception.create('[TFormContacts.CreateConversation] : '+E.message);
  end;
end;


procedure TFormContacts.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if GJabberClient.Connected then
    GJabberClient.Disconnect;
  FormConnect.Close;
end;

procedure TFormContacts.FormCreate(Sender: TObject);
begin
  Caption := GJabberClient.Login;
  GJabberClient.OnNewContact := OnNewContactProc;
  GJabberClient.OnMessageReceived := OnMessageReceivedProc;
  GJabberClient.OnUpdatePresence := OnPResenceCallBack;
  GJabberClient.OnContactAskToAdd := ContactAskToAddProc;
  GJabberClient.OnAddContactStatus := OnAddContactStatusProc;
end;


function TFormContacts.GetTabIndex(ATagString: string): integer;
var i : integer;
begin
  Result := -1;
  for i := 0 to TabControl2.TabCount -1 do
    if Pos(uppercase(TabControl2.Tabs[i].TagString) , UpperCase(ATagString)) > 0 then
    begin
      Result := i;
      Break;
    end;
end;


procedure TFormContacts.ListViewContactsClick(Sender: TObject);
var
  wTabID : integer;
  wContact : PContact;
begin
  if ListViewContacts.ItemIndex <> -1 then
  begin
    wContact := GJabberClient.Contacts[ListViewContacts.ItemIndex];
    wTabID := GetTabIndex( wContact.Jid );

    if wTabID = -1 then
    begin
      CreateConversation(wContact);
      wTabID := TabControl2.TabCount -1;
    end;

    TabControl1.ActiveTab := TabItem2;
    TabControl2.TabIndex := wTabID;
  end;
end;


procedure TFormContacts.ListViewContactsDeletingItem(Sender: TObject;
  AIndex: Integer; var ACanDelete: Boolean);
begin
  if MessageDlg('Are you sure you want to delete the user ' + ListViewContacts.Items[AIndex].Text + '?',TMsgDlgType.mtConfirmation,
     mbYesNo,0) = mrYes then
  begin
    ACanDelete := True;
    GJabberClient.RemoveContact(AIndex);
  end
  else ACanDelete := False;
end;

procedure TFormContacts.OnAddContactStatusProc(AJid: string;
  AAccepted: Boolean);
begin
//  if AAccepted then
//    showmessage(AJid + ' accepted your request.')
//  else
 //   showmessage(AJid + ' refused your request.');
end;

procedure TFormContacts.OnAskTosendMessageProc(ATo, AMessage: string);
begin
  GJabberClient.SendMessage(ATo, AMessage);
end;


procedure TFormContacts.OnMessageReceivedProc(AFrom, AMessage: string);
var i, wTabID : Integer;
    wFrame : TFrameConversation;
    wContact : PContact;
    wUnreadMsgColor : TAlphaColorRec;
begin
  ListViewContacts.BeginUpdate;
  try
    wTabID := GetTabIndex(AFrom);
    if wTabID <> -1 then
    begin
      wFrame := TFrameConversation(FindComponent('Frame_' + inttostr(wTabID)));
      if wFrame <> nil then
      begin
        wFrame.TalkComp.AddMessageFromPeople2( AMessage );
        wFrame.MemText2Send.SetFocus;
      end;
    end
    else begin
      for i := 0 to GJabberClient.ContactsCount -1 do
      begin
        wContact := GJabberClient.Contacts[i];
        if Pos(uppercase(wContact.Jid), uppercase(AFrom)) > 0 then
        begin
          CreateConversation(GJabberClient.Contacts[i]);
          wTabID := GetTabIndex(AFrom);
          if wTabID <> -1 then
          begin
            wFrame := TFrameConversation(FindComponent('Frame_' + inttostr(wTabID)));
            if wFrame <> nil then
            begin
              wFrame.TalkComp.AddMessageFromPeople2( AMessage );
              wFrame.MemText2Send.SetFocus;
            end;
          end;
          ListViewContacts.Items[i].Detail := '! New message !';
          ListViewContacts.Repaint;

          Break;
        end;
      end;
    end;
  finally
    ListViewContacts.EndUpdate;
  end;
end;


procedure TFormContacts.OnNewContactProc(AJID : string);
begin
  ShowContacts;
end;


procedure TFormContacts.OnPResenceCallBack(AJid: String);
var i : integer;
begin
  if AJid = '-2' then
  begin

    case GJabberClient.UserStatus of
      usOnline    : begin
        cmbAvailability.ItemIndex := 0;
        ImgUserstatus.Bitmap.Assign(FormConnect.OnlineImage.Bitmap); //LoadFromFile(GetImageFilename('online.png'));
      end;
      usAway    : begin
        cmbAvailability.ItemIndex := 1;
        ImgUserstatus.Bitmap.Assign(FormConnect.BusyImage.Bitmap); //LoadFromFile(GetImageFilename('busy.png'));
      end;
      usInVisible    : begin
        cmbAvailability.ItemIndex := 2;
        ImgUserstatus.Bitmap.Assign(FormConnect.OfflineImage.Bitmap); //LoadFromFile(GetImageFilename('offline.png'));
      end;
    end;
  end
  else begin
    for i := 0 to GJabberClient.ContactsCount -1 do
    begin
      if CompareText(AJid,GJabberClient.Contacts[i].Jid) = 0 then
        UpdateContactDisplay(i);
    end;
  end;
end;


procedure TFormContacts.ShowContacts;
var i : integer;
  wContact : PContact;
  wTListViewItem : TListViewItem;
begin
  ListViewContacts.ClearItems;
  FormContacts.ListViewContacts.BeginUpdate;
  try
    for i := 0 to GJabberClient.ContactsCount -1 do
    begin
      wTListViewItem := ListViewContacts.Items.Add;
      wContact := GJabberClient.Contacts[i];
      wTListViewItem.Text := IFTHEN(wContact.Name = '', wContact.Jid, wContact.Name);
      wTListViewItem.Detail := wContact.ADisplayMessage;
      wTListViewItem.Bitmap.Assign(FormConnect.OfflineImage.Bitmap); //LoadFromFile(GetImageFilename('offline.png'));
    end;
  finally
    FormContacts.ListViewContacts.EndUpdate;
  end;

end;


procedure TFormContacts.UpdateContactDisplay(AIdx: integer);
begin
  ListViewContacts.Items[Aidx].Detail := GJabberClient.Contacts[AIdx].ADisplayMessage;
  case GJabberClient.Contacts[AIdx].AStatus of
    usOnline    : ListViewContacts.Items[Aidx].Bitmap.Assign(FormConnect.OnlineImage.Bitmap); //LoadFromFile(GetImageFilename('online.png'));
    usAway      : ListViewContacts.Items[Aidx].Bitmap.Assign(FormConnect.BusyImage.Bitmap); //LoadFromFile(GetImageFilename('busy.png'));
    usInVisible : ListViewContacts.Items[Aidx].Bitmap.Assign(FormConnect.OfflineImage.Bitmap); //LoadFromFile(GetImageFilename('offline.png'));
  end;
end;

end.
