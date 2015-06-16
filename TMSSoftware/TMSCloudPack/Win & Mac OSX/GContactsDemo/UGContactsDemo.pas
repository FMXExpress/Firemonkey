unit UGContactsDemo;

interface

uses
  FMX.TMSCloudBase, FMX.TMSCloudGContacts, FMX.Dialogs,
  FMX.StdCtrls, FMX.ListBox, FMX.Grid, FMX.TMSCloudListView, FMX.TabControl,
  FMX.Memo, FMX.Edit, FMX.ExtCtrls, FMX.Objects, FMX.TMSCloudImage, FMX.Layouts,
  System.Classes, FMX.Types, FMX.Controls, FMX.Forms, SysUtils, UITypes,
  FMX.TMSCloudBaseFMX, FMX.TMSCloudCustomGoogle, FMX.TMSCloudGoogleFMX,
  FMX.TMSCloudCustomGContacts;


type
  TForm1 = class(TForm)
    GroupBox2: TGroupBox;
    Panel1: TPanel;
    Button1: TButton;
    TMSFMXCloudGContacts1: TTMSFMXCloudGContacts;
    Button6: TButton;
    lbGroups: TListBox;
    GroupBox3: TGroupBox;
    ListBox1: TListBox;
    Button2: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    TMSFMXCloudCloudImage1: TTMSFMXCloudImage;
    Label9: TLabel;
    Label10: TLabel;
    Label17: TLabel;
    edFirstName: TEdit;
    edCompany: TEdit;
    edJob: TEdit;
    dpBirthDay: TCalendarEdit;
    edNickName: TEdit;
    edLastName: TEdit;
    meNotes: TMemo;
    PageControl1: TTabControl;
    TabSheet1: TTabItem;
    TabSheet2: TTabItem;
    TabSheet3: TTabItem;
    TabSheet4: TTabItem;
    TabSheet5: TTabItem;
    TabSheet6: TTabItem;
    TabSheet7: TTabItem;
    TabSheet8: TTabItem;
    Panel2: TPanel;
    btAddContact: TButton;
    btUpdateContact: TButton;
    btDeleteContact: TButton;
    GroupBox4: TGroupBox;
    Label19: TLabel;
    edGroupName: TEdit;
    Panel3: TPanel;
    btGroupAdd: TButton;
    btGroupUpdate: TButton;
    btGroupDelete: TButton;
    btRemove: TButton;
    Label2: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    lvEmail: TTMSFMXCloudListView;
    cbEmailType: TComboBox;
    edEmail: TEdit;
    edEmailCustom: TEdit;
    cbEmailPrimary: TCheckBox;
    btAddEmail: TButton;
    btUpdateEmail: TButton;
    btDeleteEmail: TButton;
    lvPhone: TTMSFMXCloudListView;
    edPhone: TEdit;
    Label8: TLabel;
    Label11: TLabel;
    cbPhoneType: TComboBox;
    edPhoneCustom: TEdit;
    Label12: TLabel;
    btAddPhone: TButton;
    btUpdatePhone: TButton;
    btDeletePhone: TButton;
    cbPhonePrimary: TCheckBox;
    btAddAddress: TButton;
    btDeleteAddress: TButton;
    btUpdateAddress: TButton;
    cbAddressPrimary: TCheckBox;
    edAddressCustom: TEdit;
    Label13: TLabel;
    Label14: TLabel;
    cbAddressType: TComboBox;
    edStreet: TEdit;
    Label15: TLabel;
    lvAddress: TTMSFMXCloudListView;
    Label16: TLabel;
    edZip: TEdit;
    lbCity: TLabel;
    edCity: TEdit;
    lbCountry: TLabel;
    edCountry: TEdit;
    lvRelation: TTMSFMXCloudListView;
    edRelation: TEdit;
    cbRelation: TComboBox;
    edCustomRelation: TEdit;
    Label18: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    btAddRelation: TButton;
    btUpdateRelation: TButton;
    btDeleteRelation: TButton;
    lvIM: TTMSFMXCloudListView;
    edIM: TEdit;
    cbIMType: TComboBox;
    edIMCustom: TEdit;
    btAddIM: TButton;
    btUpdateIM: TButton;
    btDeleteIM: TButton;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    lvCustom: TTMSFMXCloudListView;
    edCustomKey: TEdit;
    btAddCustom: TButton;
    btUpdateCustom: TButton;
    btDeleteCustom: TButton;
    Label26: TLabel;
    Label28: TLabel;
    edCustomValue: TEdit;
    edWebsite: TEdit;
    btDeleteWebsite: TButton;
    btUpdateWebsite: TButton;
    btAddWebsite: TButton;
    edCustomWebsite: TEdit;
    cbCustomWebsite: TComboBox;
    Label25: TLabel;
    Label27: TLabel;
    Label29: TLabel;
    lvWebsite: TTMSFMXCloudListView;
    lvGroup: TTMSFMXCloudListView;
    Label30: TLabel;
    cbGroups: TComboBox;
    btAddGroup: TButton;
    btDeleteGroup: TButton;
    OpenDialog1: TOpenDialog;
    btUpdateImage: TButton;
    btDeleteImage: TButton;
    Label31: TLabel;
    Image1: TImage;
    procedure ToggleControls;
    procedure ClearControls;
    procedure Button1Click(Sender: TObject);
    procedure TMSFMXCloudGContacts1ReceivedAccessToken(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure btAddContactClick(Sender: TObject);
    procedure btDeleteContactClick(Sender: TObject);
    procedure btUpdateContactClick(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure websiteClick(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure btGroupAddClick(Sender: TObject);
    procedure lbGroupsClick(Sender: TObject);
    procedure btGroupUpdateClick(Sender: TObject);
    procedure btGroupDeleteClick(Sender: TObject);
    procedure btRemoveClick(Sender: TObject);
    procedure btAddEmailClick(Sender: TObject);
    procedure btUpdateEmailClick(Sender: TObject);
    procedure btDeleteEmailClick(Sender: TObject);
    procedure btAddPhoneClick(Sender: TObject);
    procedure btUpdatePhoneClick(Sender: TObject);
    procedure btDeletePhoneClick(Sender: TObject);
    procedure btAddAddressClick(Sender: TObject);
    procedure btUpdateAddressClick(Sender: TObject);
    procedure btDeleteAddressClick(Sender: TObject);
    procedure btAddRelationClick(Sender: TObject);
    procedure btUpdateRelationClick(Sender: TObject);
    procedure btDeleteRelationClick(Sender: TObject);
    procedure btAddIMClick(Sender: TObject);
    procedure btUpdateIMClick(Sender: TObject);
    procedure btAddCustomClick(Sender: TObject);
    procedure btUpdateCustomClick(Sender: TObject);
    procedure btAddWebsiteClick(Sender: TObject);
    procedure btUpdateWebsiteClick(Sender: TObject);
    procedure btDeleteWebsiteClick(Sender: TObject);
    procedure btAddGroupClick(Sender: TObject);
    procedure btDeleteGroupClick(Sender: TObject);
    procedure btDeleteImageClick(Sender: TObject);
    procedure btUpdateImageClick(Sender: TObject);
    procedure lvEmailChange(Sender: TObject);
    procedure lvIMChange(Sender: TObject);
    procedure lvAddressChange(Sender: TObject);
    procedure lvCustomChange(Sender: TObject);
    procedure lvPhoneChange(Sender: TObject);
    procedure lvRelationChange(Sender: TObject);
    procedure lvWebsiteChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Connected: boolean;
    Inserting: boolean;
    procedure FillContacts();
    procedure FillContactDetails();
    procedure FillGroups(Update: Boolean);
  end;

var
  Form1: TForm1;

implementation

{$R *.FMX}

{$I APPIDS.INC}

procedure TForm1.TMSFMXCloudGContacts1ReceivedAccessToken(Sender: TObject);
begin
  TMSFMXCloudGContacts1.SaveTokens;
  Connected := true;
  ToggleControls;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  acc: boolean;
begin
  TMSFMXCloudGContacts1.App.Key := GAppkey;
  TMSFMXCloudGContacts1.App.Secret := GAppSecret;
  TMSFMXCloudGContacts1.Logging := true;

  if TMSFMXCloudGContacts1.App.Key <> '' then
  begin
    TMSFMXCloudGContacts1.PersistTokens.Key := ExtractFilePath(ParamStr(0)) + 'google.ini';
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

procedure TForm1.Button2Click(Sender: TObject);
begin
  FillContacts;
end;

procedure TForm1.btAddContactClick(Sender: TObject);
var
  gc: TGContact;
begin
  if not (Inserting) then
  begin
    ClearControls;
    edFirstName.SetFocus;
    ListBox1.ItemIndex := -1;
    btAddContact.Text  := 'Insert';
    Inserting := true;
  end
  else
  begin
    btAddContact.Text  := 'New';

    gc := TMSFMXCloudGContacts1.Contacts.Add;
    gc.FirstName := edFirstName.Text;
    gc.LastName := edLastName.Text;
    gc.Nickname := edNickName.Text;
    gc.JobTitle := edJob.Text;
    gc.Company := edCompany.Text;
    gc.BirthDay := dpBirthDay.Date;
    gc.Notes := meNotes.Text;
    TMSFMXCloudGContacts1.Add(gc);

    FillContacts;
    Inserting := false;
  end;
end;

procedure TForm1.btAddCustomClick(Sender: TObject);
var
  gc: TGCustomData;
begin
  if (edCustomKey.Text <> '') and (edCustomValue.Text <> '') then
  begin
    gc := TMSFMXCloudGContacts1.Contacts[ListBox1.ItemIndex].CustomItems.Add;
    gc.Key := edCustomKey.Text;
    gc.Value := edCustomValue.Text;
  end;
  FillContactDetails;
end;

procedure TForm1.btDeleteContactClick(Sender: TObject);
var
  buttonSelected: integer;
begin
  if ListBox1.ItemIndex >= 0 then
  begin
    buttonSelected := MessageDlg('Are you sure you want to delete the selected Contact?', TMsgDlgType.mtConfirmation, mbOKCancel, 0);

    if buttonSelected = mrOk then
    begin
      TMSFMXCloudGContacts1.Delete(ListBox1.Items.Objects[ListBox1.ItemIndex] as TGContact);
      FillContacts;
      ClearControls;
    end;
  end
  else
  begin
    ShowMessage('Please select a Contact first.');
  end;
end;

procedure TForm1.btUpdateContactClick(Sender: TObject);
var
  buttonSelected: integer;
  gc: TGContact;
begin
  if ListBox1.ItemIndex >= 0 then
  begin
    buttonSelected := MessageDlg('Are you sure you want to update the selected Contact?', TMsgDlgType.mtConfirmation, mbOKCancel, 0);

    if buttonSelected = mrOk then
    begin
      gc := ListBox1.Items.Objects[ListBox1.ItemIndex] as TGContact;
      gc.FirstName := edFirstName.Text;
      gc.LastName := edLastName.Text;
      gc.Nickname := edNickName.Text;
      gc.JobTitle := edJob.Text;
      gc.Company := edCompany.Text;
      gc.BirthDay := dpBirthDay.Date;
      gc.Notes := meNotes.Text;

      TMSFMXCloudGContacts1.Update(gc);
    end;
  end
  else
  begin
    ShowMessage('Please select a Contact first.');
  end;
end;

procedure TForm1.btUpdateCustomClick(Sender: TObject);
var
  gc: TGCustomData;
begin
  if lvCustom.ItemIndex >= 0 then
  begin
    if edIM.Text <> '' then
    begin
      gc := TMSFMXCloudGContacts1.Contacts[ListBox1.ItemIndex].CustomItems[lvCustom.ItemIndex];
      gc.Key := edCustomKey.Text;
      gc.Value := edCustomValue.Text;
    end;
    FillContactDetails;
  end;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  FillGroups(True);
end;

procedure TForm1.ClearControls;
begin
  edFirstName.Text := '';
  edLastName.Text := '';
  edNickName.Text := '';
  dpBirthDay.Date := 0;
  edCompany.Text := '';
  edJob.Text := '';
  meNotes.Text := '';
  PageControl1.Visible := False;
end;

procedure TForm1.btAddAddressClick(Sender: TObject);
var
  ge: TGEmail;
  I: Integer;
begin
    if (edStreet.Text <> '') or (edZip.Text <> '') or (edCity.Text <> '') or (edCountry.text <> '') then
  begin
    ge := TMSFMXCloudGContacts1.Contacts[ListBox1.ItemIndex].Emails.Add;
    ge.Address := edEmail.Text;
    ge.EmailType := TGFieldType(cbEmailType.ItemIndex);
    ge.CustomType := edAddressCustom.Text;

    if cbEmailPrimary.IsChecked  then
    begin
      for I := 0 to TMSFMXCloudGContacts1.Contacts[ListBox1.ItemIndex].Emails.Count - 1 do
        TMSFMXCloudGContacts1.Contacts[ListBox1.ItemIndex].Emails[I].Primary := false;
    end;

    ge.Primary := cbEmailPrimary.IsChecked;
  end;
  FillContactDetails;
end;

procedure TForm1.btAddEmailClick(Sender: TObject);
var
  ge: TGEmail;
  I: Integer;
begin
  if edEmail.Text <> '' then
  begin
    ge := TMSFMXCloudGContacts1.Contacts[ListBox1.ItemIndex].Emails.Add;
    ge.Address := edEmail.Text;
    ge.EmailType := TGFieldType(cbEmailType.ItemIndex);
    ge.CustomType := edEmailCustom.Text;

    if cbEmailPrimary.IsChecked  then
    begin
      for I := 0 to TMSFMXCloudGContacts1.Contacts[ListBox1.ItemIndex].Emails.Count - 1 do
        TMSFMXCloudGContacts1.Contacts[ListBox1.ItemIndex].Emails[I].Primary := false;
    end;

    ge.Primary := cbEmailPrimary.IsChecked;
  end;
  FillContactDetails;
end;

procedure TForm1.btAddGroupClick(Sender: TObject);
var
  gg: TGGroup;
  gcg: TGContactGroup;
begin
  if cbGroups.ItemIndex >= 0 then
  begin
    gcg := TMSFMXCloudGContacts1.Contacts[ListBox1.ItemIndex].Groups.Add;
    gg := cbGroups.Items.Objects[cbGroups.ItemIndex] as TGGroup;
    gcg.ID := gg.ID;
    FillContactDetails;
  end;
end;

procedure TForm1.btAddIMClick(Sender: TObject);
var
  gi: TGInstantMessenger;
begin
  if edIM.Text <> '' then
  begin
    gi := TMSFMXCloudGContacts1.Contacts[ListBox1.ItemIndex].InstantMessengers.Add;
    gi.ID := edEmail.Text;
    gi.InstantMessengerType := TGIMType(cbIMType.ItemIndex);
    gi.CustomType := edIMCustom.Text;
  end;
  FillContactDetails;
end;

procedure TForm1.btAddPhoneClick(Sender: TObject);
var
  ge: TGPhoneNumber;
  I: Integer;
begin
  if edPhone.Text <> '' then
  begin
    ge := TMSFMXCloudGContacts1.Contacts[ListBox1.ItemIndex].PhoneNumbers.Add;
    ge.Number := edPhone.Text;
    ge.PhoneType := TGPhoneType(cbPhoneType.ItemIndex);
    ge.CustomType := edPhoneCustom.Text;

    if cbPhonePrimary.IsChecked  then
    begin
      for I := 0 to TMSFMXCloudGContacts1.Contacts[ListBox1.ItemIndex].PhoneNumbers.Count - 1 do
        TMSFMXCloudGContacts1.Contacts[ListBox1.ItemIndex].PhoneNumbers[I].Primary := false;
    end;

    ge.Primary := cbPhonePrimary.ISChecked;
  end;
  FillContactDetails;
end;

procedure TForm1.btAddRelationClick(Sender: TObject);
var
  gr: TGRelation;
begin
  if edRelation.Text <> '' then
  begin
    gr := TMSFMXCloudGContacts1.Contacts[ListBox1.ItemIndex].Relations.Add;
    gr.Value := edRelation.Text;
    gr.Relation := TGRelationType(cbRelation.ItemIndex);
    gr.CustomRelation := edCustomRelation.Text;
  end;
  FillContactDetails;
end;

procedure TForm1.btAddWebsiteClick(Sender: TObject);
var
  gw: TGWebsite;
begin
  if edWebsite.Text <> '' then
  begin
    gw := TMSFMXCloudGContacts1.Contacts[ListBox1.ItemIndex].Websites.Add;
    gw.URL := edWebsite.Text;
    gw.WebsiteType := TGWebsiteType(cbCustomWebsite.ItemIndex);
    gw.CustomType := edCustomWebsite.Text;
  end;
  FillContactDetails;
end;

procedure TForm1.btDeleteAddressClick(Sender: TObject);
begin
  if lvAddress.ItemIndex >= 0 then
  begin
    TMSFMXCloudGContacts1.Contacts[ListBox1.ItemIndex].PostalAddresses[lvEmail.ItemIndex].Free;
    FillContactDetails;
  end;
end;

procedure TForm1.btDeleteEmailClick(Sender: TObject);
begin
  if lvEmail.ItemIndex >= 0 then
  begin
    TMSFMXCloudGContacts1.Contacts[ListBox1.ItemIndex].Emails[lvEmail.ItemIndex].Free;
    FillContactDetails;
  end;
end;

procedure TForm1.btDeleteGroupClick(Sender: TObject);
begin
  if lvGroup.ItemIndex >= 0 then
  begin
    TMSFMXCloudGContacts1.Contacts[ListBox1.ItemIndex].Groups[lvGroup.ItemIndex].Free;
    FillContactDetails;
  end;
end;

procedure TForm1.btDeleteImageClick(Sender: TObject);
var
  buttonSelected: integer;
begin
  if ListBox1.ItemIndex >= 0 then
  begin
    buttonSelected := MessageDlg('Are you sure you want to delete the Image for the selected Contact?', TMsgDlgType.mtConfirmation, mbOKCancel, 0);

    if buttonSelected = mrOk then
    begin
      TMSFMXCloudGContacts1.DeleteImage(ListBox1.Items.Objects[ListBox1.ItemIndex] as TGContact);
      TMSFMXCloudCloudImage1.URL := '';
      FillContacts;
    end;
  end
  else
  begin
    ShowMessage('Please select a Contact first.');
  end;
end;

procedure TForm1.btDeletePhoneClick(Sender: TObject);
begin
  if lvPhone.ItemIndex >= 0 then
  begin
    TMSFMXCloudGContacts1.Contacts[ListBox1.ItemIndex].PhoneNumbers[lvPhone.ItemIndex].Free;
    FillContactDetails;
  end;
end;

procedure TForm1.btDeleteRelationClick(Sender: TObject);
begin
  if lvRelation.ItemIndex >= 0 then
  begin
    TMSFMXCloudGContacts1.Contacts[ListBox1.ItemIndex].Relations[lvRelation.ItemIndex].Free;
    FillContactDetails;
  end;
end;

procedure TForm1.btDeleteWebsiteClick(Sender: TObject);
begin
  if lvWebsite.ItemIndex >= 0 then
  begin
    TMSFMXCloudGContacts1.Contacts[ListBox1.ItemIndex].Websites[lvWebsite.ItemIndex].Free;
    FillContactDetails;
  end;
end;

procedure TForm1.btUpdateAddressClick(Sender: TObject);
var
  ga: TGPostalAddress;
  I: Integer;
begin
  if lvAddress.ItemIndex >= 0 then
  begin
    if (edStreet.Text <> '') or (edZip.Text <> '') or (edCity.Text <> '') or (edCountry.text <> '') then
    begin
      ga := TMSFMXCloudGContacts1.Contacts[ListBox1.ItemIndex].PostalAddresses[lvAddress.ItemIndex];
      ga.Street := edStreet.Text;
      ga.PostCode := edZip.Text;
      ga.City := edCity.Text;
      ga.Country := edCountry.text;
      ga.AddressType := TGFieldType(cbAddressType.ItemIndex);

      if cbAddressPrimary.IsChecked  then
      begin
        for I := 0 to TMSFMXCloudGContacts1.Contacts[ListBox1.ItemIndex].PostalAddresses.Count - 1 do
          TMSFMXCloudGContacts1.Contacts[ListBox1.ItemIndex].PostalAddresses[I].Primary := false;
      end;

      ga.Primary := cbAddressPrimary.IsChecked;
    end;
    FillContactDetails;
  end;
end;

procedure TForm1.btUpdateEmailClick(Sender: TObject);
var
  ge: TGEmail;
  I: Integer;
begin
  if lvEmail.ItemIndex >= 0 then
  begin
    if edEmail.Text <> '' then
    begin
      ge := TMSFMXCloudGContacts1.Contacts[ListBox1.ItemIndex].Emails[lvEmail.ItemIndex];
      ge.Address := edEmail.Text;
      ge.EmailType := TGFieldType(cbEmailType.ItemIndex);
      ge.CustomType := edEmailCustom.Text;

      if cbEmailPrimary.IsChecked  then
      begin
        for I := 0 to TMSFMXCloudGContacts1.Contacts[ListBox1.ItemIndex].Emails.Count - 1 do
          TMSFMXCloudGContacts1.Contacts[ListBox1.ItemIndex].Emails[I].Primary := false;
      end;

      ge.Primary := cbEmailPrimary.IsChecked;
    end;
    FillContactDetails;
  end;
end;

procedure TForm1.btUpdateImageClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
      TMSFMXCloudGContacts1.UpdateImage(ListBox1.Items.Objects[ListBox1.ItemIndex] as TGContact, opendialog1.FileName);
      FillContacts;
  end;
end;

procedure TForm1.btUpdateIMClick(Sender: TObject);
var
  gi: TGInstantMessenger;
begin
  if lvIM.ItemIndex >= 0 then
  begin
    if edIM.Text <> '' then
    begin
      gi := TMSFMXCloudGContacts1.Contacts[ListBox1.ItemIndex].InstantMessengers[lvIM.ItemIndex];
      gi.ID := edIM.Text;
      gi.InstantMessengerType := TGIMType(cbIMType.ItemIndex);
      gi.CustomType := edIMCustom.Text;
    end;
    FillContactDetails;
  end;
end;

procedure TForm1.btUpdatePhoneClick(Sender: TObject);
var
  ge: TGPhoneNumber;
  I: Integer;
begin
  if lvPhone.ItemIndex >= 0 then
  begin
    if edPhone.Text <> '' then
    begin
      ge := TMSFMXCloudGContacts1.Contacts[ListBox1.ItemIndex].PhoneNumbers[lvPhone.ItemIndex];
      ge.Number := edPhone.Text;
      ge.PhoneType := TGPhoneType(cbPhoneType.ItemIndex);
      ge.CustomType := edPhoneCustom.Text;

      if cbPhonePrimary.IsChecked  then
      begin
        for I := 0 to TMSFMXCloudGContacts1.Contacts[ListBox1.ItemIndex].PhoneNumbers.Count - 1 do
          TMSFMXCloudGContacts1.Contacts[ListBox1.ItemIndex].PhoneNumbers[I].Primary := false;
      end;

      ge.Primary := cbPhonePrimary.IsChecked;
    end;
    FillContactDetails;
  end;
end;

procedure TForm1.btUpdateRelationClick(Sender: TObject);
var
  gr: TGRelation;
begin
  if lvRelation.ItemIndex >= 0 then
  begin
    if edRelation.Text <> '' then
    begin
      gr := TMSFMXCloudGContacts1.Contacts[ListBox1.ItemIndex].Relations[lvRelation.ItemIndex];
      gr.Value := edRelation.Text;
      gr.Relation := TGRelationType(cbRelation.ItemIndex);
      gr.CustomRelation := edCustomRelation.Text;
    end;
    FillContactDetails;
  end;
end;

procedure TForm1.btUpdateWebsiteClick(Sender: TObject);
var
  gr: TGWebsite;
begin
  if lvWebsite.ItemIndex >= 0 then
  begin
    if edRelation.Text <> '' then
    begin
      gr := TMSFMXCloudGContacts1.Contacts[ListBox1.ItemIndex].Websites[lvWebsite.ItemIndex];
      gr.URL := edRelation.Text;
      gr.WebsiteType := TGWebsiteType(cbCustomWebsite.ItemIndex);
      gr.CustomType := edCustomWebsite.Text;
    end;
    FillContactDetails;
  end;
end;

procedure TForm1.btGroupAddClick(Sender: TObject);
var
  gr: TGGroup;
begin
  gr := TMSFMXCloudGContacts1.Groups.Add;
  gr.Summary := edGroupName.Text;
  TMSFMXCloudGContacts1.AddGroup(gr);
  FillGroups(False);
end;

procedure TForm1.btGroupDeleteClick(Sender: TObject);
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

procedure TForm1.btGroupUpdateClick(Sender: TObject);
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

procedure TForm1.btRemoveClick(Sender: TObject);
begin
  TMSFMXCloudGContacts1.ClearTokens;
  Connected := false;
  ClearControls;
  lbGroups.Items.Clear;
  ListBox1.Clear;
  ToggleControls;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Connected := false;
  ToggleControls;
  PageControl1.Visible := False;

  TMSFMXCloudGContacts1.Contacts.Clear;
  TMSFMXCloudGContacts1.Groups.Clear;

  TMSFMXCloudGContacts1.PersistTokens.Key := ExtractFilePath(ParamStr(0)) + 'google.ini';
  TMSFMXCloudGContacts1.PersistTokens.Section := 'tokens';
  TMSFMXCloudGContacts1.LoadTokens;
end;

procedure TForm1.lbGroupsClick(Sender: TObject);
var
  gg: TGGroup;
begin
  if lbGroups.ItemIndex >= 0 then
  begin
    gg := lbGroups.Items.Objects[lbGroups.ItemIndex] as TGGroup;
    edGroupName.Text := gg.Summary;
  end;
end;

procedure TForm1.FillContactDetails();
var
  gc: TGContact;
  I: Integer;
  fieldType, primary: string;
  J: Integer;
  li: TListItem;
begin
  if ListBox1.ItemIndex >= 0 then
  begin
    gc := ListBox1.Items.Objects[ListBox1.ItemIndex] as TGContact;
    edFirstName.Text := gc.FirstName;
    edLastName.Text := gc.LastName;
    meNotes.Lines.Text := gc.Notes;
    edCompany.Text := gc.Company;
    edJob.Text := gc.JobTitle;
    dpBirthDay.Date := gc.BirthDay;
    edNickName.Text := gc.Nickname;

    lvEmail.Items.Clear;
    for I := 0 to gc.Emails.Count - 1 do
    begin
      case gc.Emails[I].EmailType of
        ftHome: fieldType := 'Home';
        ftWork: fieldType := 'Work';
        ftCustom: fieldType := gc.Emails[I].CustomType;
      end;
      li := lvEmail.Items.Add;
      li.Text  := gc.Emails[I].Address;
      li.SubItems.Add(fieldType);
      if gc.Emails[I].Primary then
        primary := 'True'
      else
        primary := 'False';
      li.SubItems.Add(primary);
      li.Data := gc.Emails[I];
    end;

    lvPhone.Items.Clear;
    for I := 0 to gc.PhoneNumbers.Count - 1 do
    begin
      case gc.PhoneNumbers[I].PhoneType of
        ptMobile: fieldType := 'Mobile';
        ptHome: fieldType := 'Home';
        ptWork: fieldType := 'Work';
        ptMain: fieldType := 'Main';
        ptCustom: fieldType := gc.PhoneNumbers[I].CustomType;
      end;
      li := lvPhone.Items.Add;
      li.Text  := gc.PhoneNumbers[I].Number;
      li.SubItems.Add(fieldType);
      if gc.PhoneNumbers[I].Primary then
        primary := 'True'
      else
        primary := 'False';
      li.SubItems.Add(primary);
      li.Data := gc.PhoneNumbers[I];
    end;

    lvAddress.Items.Clear;
    for I := 0 to gc.PostalAddresses.Count - 1 do
    begin
      case gc.PostalAddresses[I].AddressType of
        ftHome: fieldType := 'Home';
        ftWork: fieldType := 'Work';
        ftCustom: fieldType := gc.PostalAddresses[I].CustomType;
      end;
      li := lvAddress.Items.Add;
      li.Text  := gc.PostalAddresses[I].Street + ', ' + gc.PostalAddresses[I].City;
      li.SubItems.Add(fieldType);
      if gc.PostalAddresses[I].Primary then
        primary := 'True'
      else
        primary := 'False';
      li.SubItems.Add(primary);
      li.Data := gc.PostalAddresses[I];
    end;

    lvWebsite.Items.Clear;
    for I := 0 to gc.Websites.Count - 1 do
    begin
      case gc.Websites[I].WebsiteType of
        wtProfile: fieldType := 'Profile';
        wtBlog: fieldType := 'Blog';
        wtHomePage: fieldType := 'HomePage';
        wtWork: fieldType := 'Work';
        wtCustom: fieldType := gc.Websites[I].CustomType;
      end;
      li := lvWebsite.Items.Add;
      li.Text  := gc.Websites[I].URL;
      li.SubItems.Add(fieldType);
      li.Data := gc.Websites[I];
    end;

    lvCustom.Items.Clear;
    for I := 0 to gc.CustomItems.Count - 1 do
    begin
      li := lvCustom.Items.Add;
      li.Text  := gc.CustomItems[I].Key;
      li.SubItems.Add(gc.CustomItems[I].Value);
      li.Data := gc.CustomItems[I];
    end;

    lvRelation.Items.Clear;
    for I := 0 to gc.Relations.Count - 1 do
    begin
      case gc.Relations[I].Relation of
        grSpouse: fieldType := 'Spouse';
        grChild: fieldType := 'Child';
        grMother: fieldType := 'Mother';
        grFather: fieldType := 'Father';
        grParent: fieldType := 'Parent';
        grBrother: fieldType := 'Brother';
        grSister: fieldType := 'Sister';
        grPartner: fieldType := 'Partner';
        grCustom: fieldType := gc.Relations[I].CustomRelation;
      end;
      li := lvRelation.Items.Add;
      li.Text  := gc.Relations[I].Value;
      li.SubItems.Add(fieldType);
      li.Data := gc.Relations[I];
    end;

    lvIM.Items.Clear;
    for I := 0 to gc.InstantMessengers.Count - 1 do
    begin
      case gc.InstantMessengers[I].InstantMessengerType of
        itGoogleTalk: fieldtype := 'Google Talk';
        itAIM: fieldtype := 'AIM';
        itYahoo: fieldtype := 'Yahoo';
        itSkype: fieldtype := 'Skype';
        itGQ: fieldtype := 'GQ';
        itMSN: fieldtype := 'MSN';
        itICQ: fieldtype := 'ICQ';
        itJabber: fieldtype := 'Jabber';
        itCustom: fieldtype := gc.InstantMessengers[I].CustomType;
      end;
      li := lvIM.Items.Add;
      li.Text  := gc.InstantMessengers[I].ID;
      li.SubItems.Add(fieldType);
      li.Data := gc.InstantMessengers[I];
    end;

    lvGroup.Items.Clear;
    for I := 0 to gc.Groups.Count - 1 do
    begin
      for J := 0 to TMSFMXCloudGContacts1.Groups.Count - 1 do
      begin
        if gc.Groups[I].ID = TMSFMXCloudGContacts1.Groups[J].ID then
        begin
          li := lvGroup.Items.Add;
          li.Text  := TMSFMXCloudGContacts1.Groups[J].Summary;
          li.Data := gc.Groups[I];
        end;
      end;
    end;

    TMSFMXCloudCloudImage1.URL := gc.ImageURL;
  end;
end;

procedure TForm1.FillContacts;
var
  i: integer;
begin
  ListBox1.Items.Clear;
  TMSFMXCloudgcontacts1.GetContacts;
  for i := 0 to TMSFMXCloudgcontacts1.Contacts.Count - 1 do
  begin
    listbox1.Items.AddObject(TMSFMXCloudgcontacts1.Contacts[i].Title, TMSFMXCloudGContacts1.Contacts[i]);
  end;
end;

procedure TForm1.FillGroups(Update: Boolean);
var
  i: integer;
begin
  if Update then
    TMSFMXCloudgcontacts1.GetContactGroups;
  lbGroups.Items.Clear;
  cbGroups.Items.Clear;
  for i := 0 to TMSFMXCloudgcontacts1.Groups.Count - 1 do
  begin
    lbGroups.Items.AddObject(TMSFMXCloudgcontacts1.Groups[i].Summary, TMSFMXCloudGContacts1.Groups[i]);

    cbGroups.Items.AddObject(TMSFMXCloudGContacts1.Groups[i].Summary, TMSFMXCloudGContacts1.Groups[i]);
    cbGroups.ItemIndex := 0;
  end;
end;

procedure TForm1.ListBox1Click(Sender: TObject);
begin
  FillContactDetails;
  PageControl1.Visible := true;
end;

procedure TForm1.lvAddressChange(Sender: TObject);
var
  ga: TGPostalAddress;
begin
  if lvAddress.ItemIndex >= 0 then
  begin
    ga := lvAddress.Items[lvAddress.ItemIndex].Data;
    edStreet.Text := ga.Street;
    edZip.Text := ga.PostCode;
    edCity.Text := ga.City;
    edCountry.Text := ga.Country;
    edAddressCustom.Text := ga.CustomType;
    cbAddressPrimary.IsChecked  := ga.Primary;
    cbAddressType.ItemIndex := Ord(ga.AddressType);
  end;
end;

procedure TForm1.lvCustomChange(Sender: TObject);
var
  gc: TGCustomData;
begin
  if lvCustom.ItemIndex >= 0 then
  begin
    gc := lvCustom.Items[lvCustom.ItemIndex].Data;
    edCustomValue.Text := gc.Value;
    edCustomKey.Text := gc.Key;
  end;
end;

procedure TForm1.lvEmailChange(Sender: TObject);
var
  ge: TGEmail;
begin
  if lvEmail.ItemIndex >= 0 then
  begin
    ge := lvEmail.Items[lvEmail.ItemIndex].Data;
    edEmail.Text := ge.Address;
    edEmailCustom.Text := ge.CustomType;
    cbEmailPrimary.IsChecked  := ge.Primary;
    cbEmailType.ItemIndex := Ord(ge.EmailType);
  end;
end;


procedure TForm1.lvIMChange(Sender: TObject);
var
  gi: TGInstantMessenger;
begin
  if lvIM.ItemIndex >= 0 then
  begin
    gi := lvIM.Items[lvIM.ItemIndex].Data;
    edIM.Text := gi.ID;
    edIMCustom.Text := gi.CustomType;
    cbIMType.ItemIndex := Ord(gi.InstantMessengerType);
  end;
end;

procedure TForm1.lvPhoneChange(Sender: TObject);
var
  gp: TGPhoneNumber;
begin
  if lvPhone.ItemIndex >= 0 then
  begin
    gp := lvPhone.Items[lvPhone.ItemIndex].Data;
    edPhone.Text := gp.Number;
    edPhoneCustom.Text := gp.CustomType;
    cbPhonePrimary.IsChecked  := gp.Primary;
    cbPhoneType.ItemIndex := Ord(gp.PhoneType);
  end;
end;

procedure TForm1.lvRelationChange(Sender: TObject);
var
  gr: TGRelation;
begin
  if lvRelation.ItemIndex >= 0 then
  begin
    gr := lvRelation.Items[lvRelation.ItemIndex].Data;
    edRelation.Text := gr.Value;
    edCustomRelation.Text := gr.CustomRelation;
    cbRelation.ItemIndex := Ord(gr.Relation);
  end;
end;

procedure TForm1.lvWebsiteChange(Sender: TObject);
var
  gw: TGWebsite;
begin
  if lvWebsite.ItemIndex >= 0 then
  begin
    gw := lvWebsite.Items[lvWebsite.ItemIndex].Data;
    edWebsite.Text := gw.URL;
    edCustomWebsite.Text := gw.CustomType;
    cbCustomWebsite.ItemIndex := Ord(gw.WebsiteType);
  end;
end;

procedure TForm1.ToggleControls;
begin
  Button1.Enabled := not Connected;
  btRemove.Enabled := Connected;
  Button6.Enabled := Connected;
  Button2.Enabled := Connected;
  lbGroups.Enabled := Connected;
  ListBox1.Enabled := Connected;
  edGroupName.Enabled := Connected;
  btGroupAdd.Enabled := Connected;
  btGroupUpdate.Enabled := Connected;
  btGroupDelete.Enabled := Connected;
  GroupBox1.Enabled := Connected;
  edFirstName.Enabled := Connected;
  edLastName.Enabled := Connected;
  edNickName.Enabled := Connected;
  dpBirthDay.Enabled := Connected;
  edCompany.Enabled := Connected;
  edJob.Enabled := Connected;
  meNotes.Enabled := Connected;
  btAddContact.Enabled := Connected;
  btUpdateContact.Enabled := Connected;
  btDeleteContact.Enabled := Connected;
  btUpdateImage.Enabled := Connected;
  btDeleteImage.Enabled := Connected;
end;

procedure TForm1.websiteClick(Sender: TObject);
begin
//  shellexecute(0,'open',pchar(website.Text),nil,nil,SW_NORMAL);
end;

end.
