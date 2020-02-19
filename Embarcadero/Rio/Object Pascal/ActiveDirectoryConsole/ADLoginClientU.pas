//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit ADLoginClientU;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, IPPeerClient,
  REST.Backend.ServiceTypes, REST.Backend.MetaTypes, System.JSON,
  REST.Backend.EMSServices, Data.Bind.Components, Data.Bind.ObjectScope,
  REST.Backend.BindSource, REST.Backend.ServiceComponents,
  REST.Backend.EMSProvider, System.Rtti, System.Bindings.Outputs,
  Fmx.Bind.Editors, Data.Bind.EngExt, Fmx.Bind.DBEngExt, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit, System.Actions, FMX.ActnList,
  FMX.TabControl,
  REST.Backend.EMSAPI, FMX.ScrollBox, FMX.Memo, FMX.Layouts, REST.Client,
  REST.Backend.EndPoint, FMX.ComboEdit;

type
  TEMSClientForm = class(TForm)
    EMSProvider1: TEMSProvider;
    BackendAuth1: TBackendAuth;
    EditLoginUserName: TEdit;
    LabelUserName: TLabel;
    LinkControlToFieldUserName: TLinkControlToField;
    BindingsList1: TBindingsList;
    EditLoginPassword: TEdit;
    LabelPassword: TLabel;
    LinkControlToFieldPassword: TLinkControlToField;
    EditLoginResource: TEdit;
    TabControl1: TTabControl;
    TabItemLogin: TTabItem;
    TabItemSignup: TTabItem;
    TabItemUser: TTabItem;
    ActionList1: TActionList;
    ActionLogin: TAction;
    ActionLogout: TAction;
    ActionSignup: TAction;
    ButtonLogin: TButton;
    ButtonSignup: TButton;
    ActionCurrentUserName: TAction;
    ButtonLogout: TButton;
    EditSignupUserName: TEdit;
    Label1: TLabel;
    EditSignupPassword: TEdit;
    Label2: TLabel;
    ActionDeleteUser: TAction;
    ButtonDeleteUser: TButton;
    LinkControlToField1: TLinkControlToField;
    Label3: TLabel;
    LinkControlToField2: TLinkControlToField;
    MemoJSONUser: TMemo;
    ButtonRetrieveFields: TButton;
    ButtonUpdateFields: TButton;
    ActionRetrieveUserFields: TAction;
    ActionUpdateUserFields: TAction;
    Label4: TLabel;
    Layout1: TLayout;
    Layout2: TLayout;
    CheckBoxLoginResource: TCheckBox;
    ActionUseCustomResource: TAction;
    TabItemImport: TTabItem;
    EditImportPassword: TEdit;
    Label5: TLabel;
    EditImportUserName: TEdit;
    Label6: TLabel;
    ButtonImport: TButton;
    ActionImport: TAction;
    BackendEndpointImport: TBackendEndpoint;
    Label7: TLabel;
    ComboEditGroup: TComboEdit;
    ButtonShowGroups: TButton;
    ActionShowGroups: TAction;
    BackendEndpointEnumGroups: TBackendEndpoint;
    ButtonEnumUsers: TButton;
    Label8: TLabel;
    ComboEditUsers: TComboEdit;
    ActionShowUsers: TAction;
    BackendEndpointEnumUsers: TBackendEndpoint;
    ButtonDeleteNonADUsers: TButton;
    ActionDeleteNonADUsers: TAction;
    BackendEndpointDeleteUsers: TBackendEndpoint;
    procedure ActionLoginExecute(Sender: TObject);
    procedure ActionLoginUpdate(Sender: TObject);
    procedure ActionLogoutExecute(Sender: TObject);
    procedure ActionLogoutUpdate(Sender: TObject);
    procedure ActionSignupExecute(Sender: TObject);
    procedure ActionCurrentUserNameExecute(Sender: TObject);
    procedure ActionCurrentUserNameUpdate(Sender: TObject);
    procedure ActionDeleteUserUpdate(Sender: TObject);
    procedure ActionDeleteUserExecute(Sender: TObject);
    procedure ActionRetrieveUserFieldsExecute(Sender: TObject);
    procedure ActionRetrieveUserFieldsUpdate(Sender: TObject);
    procedure ActionUpdateUserFieldsExecute(Sender: TObject);
    procedure ActionUpdateUserFieldsUpdate(Sender: TObject);
    procedure BackendAuth1LoggedOut(Sender: TObject);
    procedure ActionUseCustomResourceExecute(Sender: TObject);
    procedure ActionUseCustomResourceUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ActionImportExecute(Sender: TObject);
    procedure ActionShowGroupsExecute(Sender: TObject);
    procedure ActionShowUsersExecute(Sender: TObject);
    procedure ActionDeleteNonADUsersExecute(Sender: TObject);
  end;

var
  EMSClientForm: TEMSClientForm;

implementation

{$R *.fmx}

uses
  System.Generics.Collections, REST.Types, REST.JSON;

resourcestring
  SNewAddedUsers = 'The following users have been added from the Active Directory: ';
  SNoAddedUsers = 'There have been no new users from the Active Directory.';

  SDeletedUsers = 'The following users have been deleted: ';
  SNoDeletedUsers = 'There have been no users deleted.';

function Unquoted(const AValue: string): string;
begin
  if AValue.IsEmpty then
    Exit(string.Empty);

  Result := AValue;
  if Result.Chars[0] = '"' then
    Result := Result.Remove(0, 1);
  if Result.Chars[Result.Length - 1] = '"' then
    Result := Result.Remove(Result.Length - 1, 1);
end;

{ TEMSClientForm }

// Common

procedure TEMSClientForm.FormCreate(Sender: TObject);
begin
  EditLoginResource.Text := EMSProvider1.LoginResource;
end;

procedure TEMSClientForm.BackendAuth1LoggedOut(Sender: TObject);
begin
  MemoJSONUser.Lines.Clear;
end;

procedure TEMSClientForm.ActionUseCustomResourceExecute(Sender: TObject);
begin
  if EMSProvider1.LoginResource = '' then
    EMSProvider1.LoginResource := EditLoginResource.Text
  else
    EMSProvider1.LoginResource := '';
end;

procedure TEMSClientForm.ActionUseCustomResourceUpdate(Sender: TObject);
begin
  (Sender as TAction).Checked := EMSProvider1.LoginResource <> '';
  // Allow editing when not checked
  EditLoginResource.Enabled := not (Sender as TAction).Checked;
end;

procedure TEMSClientForm.ActionCurrentUserNameExecute(Sender: TObject);
begin
//
end;

procedure TEMSClientForm.ActionCurrentUserNameUpdate(Sender: TObject);
var
  LText: string;
begin
  if BackendAuth1.LoggedInUserName <> '' then
    LText := Format('UserName: %s', [BackendAuth1.LoggedInUserName])
  else
    LText := 'No current user';
  (Sender as TAction).Text := LText;
end;

// Login Page

procedure TEMSClientForm.ActionLoginExecute(Sender: TObject);
begin
  BackendAuth1.Login;
  // Clear fields after successful login
  BackendAuth1.ClearFieldValues;
end;

procedure TEMSClientForm.ActionLoginUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := not BackendAuth1.LoggedIn;
end;

procedure TEMSClientForm.ActionLogoutExecute(Sender: TObject);
begin
  BackendAuth1.Logout;
end;

procedure TEMSClientForm.ActionLogoutUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := BackendAuth1.LoggedIn;
end;

// Signup Page

procedure TEMSClientForm.ActionSignupExecute(Sender: TObject);
begin
  BackendAuth1.Signup;
  // Clear fields after successful login
  BackendAuth1.ClearFieldValues;
end;

// User Page

procedure TEMSClientForm.ActionRetrieveUserFieldsExecute(Sender: TObject);
var
  LEMSAPI: TEMSClientApi;
  LUser: TEMSClientAPI.TUser;
  LJSONArray: TJSONArray;
  LJSONObject: TJSONObject;
begin
  // Extract underlying EMSAPI from backend component.
  LEMSAPI := (BackendAuth1.ProviderService as IGetEMSApi).EMSAPI;
  LJSONArray := TJSONArray.Create;
  try
    if LEMSAPI.RetrieveCurrentUser(LUser, LJSONArray) then
    begin
      LJSONObject := LJSONArray.Items[0].GetValue<TJSONObject>('');
      // Remove meta data fields
      LJSONObject.RemovePair(TEMSClientAPI.TJSONNames.UserName);
      LJSONObject.RemovePair(TEMSClientAPI.TJSONNames.MetaData);
      LJSONObject.RemovePair(TEMSClientAPI.TJSONNames.UserID);
      MemoJSONUser.Text := REST.JSON.TJson.Format(LJSONObject);
    end;
  finally
    LJSONArray.Free;
  end;
end;

procedure TEMSClientForm.ActionRetrieveUserFieldsUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := BackendAuth1.LoggedIn;
end;

procedure TEMSClientForm.ActionUpdateUserFieldsExecute(Sender: TObject);
var
  LEMSAPI: TEMSClientApi;
  LJSONObject: TJSONObject;
  LUpdated: TEMSClientApi.TUpdatedAt;
begin
  // Extract underlying EMSAPI from backend component.
  LEMSAPI := (BackendAuth1.ProviderService as IGetEMSApi).EMSAPI;
  LJSONObject := TJSONObject.ParseJSONValue(MemoJSONUser.Text) as TJSONObject;
  if LJSONObject = nil then
    raise Exception.Create('Not valid JSON');
  try
    LEMSAPI.UpdateUser(BackendAuth1.LoggedInValue.ObjectID, LJSONObject, LUpdated);
  finally
    LJSONObject.Free;
  end;
end;

procedure TEMSClientForm.ActionUpdateUserFieldsUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := BackendAuth1.LoggedIn;
end;

procedure TEMSClientForm.ActionDeleteUserExecute(Sender: TObject);
var
  LEMSAPI: TEMSClientApi;
begin
  // Extract underlying EMSAPI from backend component.
  LEMSAPI := (BackendAuth1.ProviderService as IGetEMSApi).EMSAPI;
  LEMSAPI.DeleteUser(BackendAuth1.LoggedInValue.ObjectID);
  BackendAuth1.Logout;
end;

procedure TEMSClientForm.ActionDeleteUserUpdate(Sender: TObject);
begin
  with (Sender as TAction) do
  begin
    Enabled := BackendAuth1.LoggedIn;
    if BackendAuth1.LoggedIn then
      Text := Format('Delete %s', [BackendAuth1.LoggedInUserName])
    else
      Text := 'Delete User';
  end;
end;

// AD Page

procedure TEMSClientForm.ActionImportExecute(Sender: TObject);
var
  lJSON: TJSONObject;
  lNewUsers: TStrings;
  pPair: TJSONPair;
begin
  lNewUsers := nil;
  lJSON := TJSONObject.Create;
  try
    lJSON.AddPair(TEMSClientAPI.TJSONNames.UserName, EditImportUserName.Text);
    lJSON.AddPair(TEMSClientAPI.TJSONNames.Password, EditImportPassword.Text);
    if not ComboEditGroup.Text.IsEmpty then
      lJSON.AddPair(TEMSClientAPI.TJSONNames.GroupName, ComboEditGroup.Text);
    BackendEndpointImport.AddBody(lJSON);

    BackendEndpointImport.Execute;

    lNewUsers := TStringList.Create;
    for pPair in BackendEndpointImport.Response.JSONValue as TJSONObject do
      lNewUsers.Add(Unquoted(pPair.JsonString.ToString));
    if lNewUsers.Count > 0 then
      ShowMessage(SNewAddedUsers + lNewUsers.Text)
    else
      ShowMessage(SNoAddedUsers);

  finally
    lNewUsers.Free;
    lJSON.Free;
  end;
end;

procedure TEMSClientForm.ActionDeleteNonADUsersExecute(Sender: TObject);
var
  lJSON: TJSONObject;
  lDeletedUsers: TStrings;
  pPair: TJSONPair;
begin
  lDeletedUsers := nil;
  lJSON := TJSONObject.Create;
  try
    lJSON.AddPair(TEMSClientAPI.TJSONNames.UserName, EditImportUserName.Text);
    lJSON.AddPair(TEMSClientAPI.TJSONNames.Password, EditImportPassword.Text);
    if not ComboEditGroup.Text.IsEmpty then
      lJSON.AddPair(TEMSClientAPI.TJSONNames.GroupName, ComboEditGroup.Text);
    BackendEndpointDeleteUsers.AddBody(lJSON);

    BackendEndpointDeleteUsers.Execute;

    lDeletedUsers := TStringList.Create;
    for pPair in BackendEndpointDeleteUsers.Response.JSONValue as TJSONObject do
      lDeletedUsers.Add(Unquoted(pPair.JsonString.ToString));
    if lDeletedUsers.Count > 0 then
      ShowMessage(SDeletedUsers + lDeletedUsers.Text)
    else
      ShowMessage(SNoDeletedUsers);

  finally
    lDeletedUsers.Free;
    lJSON.Free;
  end;
end;

procedure TEMSClientForm.ActionShowGroupsExecute(Sender: TObject);
var
  lJSON: TJSONObject;
  pPair: TJSONPair;
begin
  lJSON := TJSONObject.Create;
  try
    lJSON.AddPair(TEMSClientAPI.TJSONNames.UserName, EditImportUserName.Text);
    lJSON.AddPair(TEMSClientAPI.TJSONNames.Password, EditImportPassword.Text);
    BackendEndpointEnumGroups.AddBody(lJSON);

    BackendEndpointEnumGroups.Execute;

    ComboEditGroup.Items.Clear;
    for pPair in BackendEndpointEnumGroups.Response.JSONValue as TJSONObject do
      ComboEditGroup.Items.Add(Unquoted(pPair.JsonString.ToString));
    if ComboEditGroup.Items.Count > 0 then
      ComboEditGroup.ItemIndex := 0;
  finally
    lJSON.Free;
  end;
end;

procedure TEMSClientForm.ActionShowUsersExecute(Sender: TObject);
var
  lJSON: TJSONObject;
  pPair: TJSONPair;
begin
  lJSON := TJSONObject.Create;
  try
    lJSON.AddPair(TEMSClientAPI.TJSONNames.UserName, EditImportUserName.Text);
    lJSON.AddPair(TEMSClientAPI.TJSONNames.Password, EditImportPassword.Text);
    BackendEndpointEnumUsers.AddBody(lJSON);

    BackendEndpointEnumUsers.Execute;

    ComboEditGroup.Items.Clear;
    for pPair in BackendEndpointEnumUsers.Response.JSONValue as TJSONObject do
      ComboEditUsers.Items.Add(Unquoted(pPair.JsonString.ToString));
    if ComboEditUsers.Items.Count > 0 then
      ComboEditUsers.ItemIndex := 0;
  finally
    lJSON.Free;
  end;
end;

end.
