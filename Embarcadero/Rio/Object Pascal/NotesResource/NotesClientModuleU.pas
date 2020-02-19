//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit NotesClientModuleU;

interface

uses
  System.SysUtils, System.Classes, IPPeerClient, REST.Backend.ServiceTypes,
  REST.Backend.MetaTypes, REST.Backend.EMSServices, System.JSON,
  REST.Backend.EMSProvider, REST.Client, REST.Backend.EndPoint,
  Data.Bind.Components, Data.Bind.ObjectScope, REST.Backend.BindSource,
  REST.Backend.ServiceComponents, NoteTypesU, System.Actions, FMX.ActnList;

type
  TNotesClientModule = class(TDataModule)
    BackendAuth1: TBackendAuth;
    BackendEndpointDeleteNote: TBackendEndpoint;
    BackendEndpointUpdateNote: TBackendEndpoint;
    BackendEndpointGetNotes: TBackendEndpoint;
    EMSProvider1: TEMSProvider;
    BackendEndpointGetNote: TBackendEndpoint;
    BackendEndpointAddNote: TBackendEndpoint;
  private
    function GetJSON<T>(const AResponse: TCustomRESTResponse): T;
    function GetJSONArray(const AResponse: TCustomRESTResponse): TJSONArray;
    function GetJSONObject(const AResponse: TCustomRESTResponse): TJSONObject;
    function GetLoggedIn: Boolean;
    function GetLoggedInUserName: string;
    { Private declarations }
  public
    { Public declarations }
    procedure Login(const AUserName, APassword: string);
    procedure Logout;
    procedure Signup(const AUserName, APassword: string);
    procedure AddNote(const ANote: TNote; out AID: string);
    function FindNote(const ATitle: string; out ANote: TNote): Boolean;
    function DeleteNote(const AID: string): Boolean;
    function GetNote(const AID: string; out ANote: TNote): Boolean;
    procedure UpdateNote(const ANote: TNote);
    function GetNotes: TArray<TNote>;
    property LoggedIn: Boolean read GetLoggedIn;
    property LoggedInUserName: string read GetLoggedInUserName;
  end;

var
  NotesClientModule: TNotesClientModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

uses FMX.Dialogs, REST.Types;

{$R *.dfm}

function TNotesClientModule.GetJSON<T>(const AResponse: TCustomRESTResponse): T;
begin
  if AResponse.JSONValue = nil then
    raise Exception.Create('JSON Expected');
  Result := AResponse.JSONValue.GetValue<T>('');
end;

function TNotesClientModule.GetJSONObject(const AResponse: TCustomRESTResponse): TJSONObject;
begin
  Result := GetJSON<TJSONObject>(AResponse);
end;

function TNotesClientModule.GetLoggedIn: Boolean;
begin
  Result := BackendAuth1.LoggedIn;
end;

function TNotesClientModule.GetLoggedInUserName: string;
begin
  Result := BackendAuth1.LoggedInUserName;
end;

function TNotesClientModule.GetJSONArray(const AResponse: TCustomRESTResponse): TJSONArray;
begin
  Result := GetJSON<TJSONArray>(AResponse);
end;

procedure TNotesClientModule.AddNote(const ANote: TNote; out AID: string);
var
  LJSONObject: TJSONObject;
  LEndpoint: TBackendEndpoint;
begin
  LEndpoint := BackendEndpointAddNote;
  LJSONObject := TNoteJSON.NoteToJSON(ANote);
  try
    LEndpoint.ClearBody;
    LEndpoint.AddBody(LJSONObject);
    LEndpoint.Execute;
    AID := GetJSONObject(LEndpoint.Response).GetValue<string>('id');
  finally
    LJSONObject.Free;
  end;
end;

function TNotesClientModule.DeleteNote(const AID: string): Boolean;
var
  LEndpoint: TBackendEndpoint;
begin
  LEndpoint := BackendEndpointDeleteNote;
  LEndpoint.Params.Items[0].Value := AID;
  // Don't raise exception on 404
  LEndpoint.AllowHTTPErrors := TBackendEndpoint.TAllowHTTPErrors.ClientErrorNotFound_404;
  LEndpoint.Execute;
  Result := LEndpoint.Response.Status.Success;
end;

function TNotesClientModule.GetNote(const AID: string; out ANote: TNote): Boolean;
var
  LEndpoint: TBackendEndpoint;
begin
  LEndpoint := BackendEndpointGetNote;
  LEndpoint.Params.Items[0].Value := AID;
  // Don't raise exception on 404
  LEndpoint.AllowHTTPErrors := TBackendEndpoint.TAllowHTTPErrors.ClientErrorNotFound_404;
  LEndpoint.Execute;
  Result := LEndpoint.Response.Status.Success;
  if Result then
  begin
    ANote := TNoteJSON.JSONToNote(
      GetJSONObject(LEndpoint.Response));
  end;
end;

function TNotesClientModule.FindNote(const ATitle: string; out ANote: TNote): Boolean;
var
  LNotes: TArray<TNote>;
  LParam: TRESTRequestParameter;
  LEndpoint: TBackendEndpoint;
begin
  LEndpoint := BackendEndpointGetNotes;
  LParam := LEndpoint.Params.AddItem;
  try
    // Add a parameter to get note request
    LParam.Kind := TRESTRequestParameterKind.pkGETorPOST;
    LParam.Name := 'title';
    LParam.Value := ATitle;
    BackendEndpointGetNote.Execute;
    LNotes := TNoteJSON.JSONToNotes(
      GetJSONArray(LEndpoint.Response));
    if Length(LNotes) = 0 then
      Result := False
    else
    begin
      Assert(Length(LNotes) = 1);
      ANote := LNotes[0];
      Result := True;
    end;
  finally
    // Delete the parameter
    LEndpoint.Params.Delete(LParam);
  end;
end;

function TNotesClientModule.GetNotes: TArray<TNote>;
var
  LEndpoint: TBackendEndpoint;
begin
  LEndpoint := BackendEndpointGetNotes;
  LEndpoint.Execute;
  Result := TNoteJSON.JSONToNotes(
    GetJSONArray(LEndpoint.Response));
end;

procedure TNotesClientModule.Login(const AUserName, APassword: string);
begin
  BackendAuth1.UserName := AUserName;
  BackendAuth1.Password := APassword;
  BackendAuth1.Login;
end;

procedure TNotesClientModule.Logout;
begin
  BackendAuth1.Logout;
end;

procedure TNotesClientModule.Signup(const AUserName, APassword: string);
begin
  BackendAuth1.UserName := AUserName;
  BackendAuth1.Password := APassword;
  BackendAuth1.Signup;
end;

procedure TNotesClientModule.UpdateNote(const ANote: TNote);
var
  LJSONObject: TJSONObject;
  LEndpoint: TBackendEndpoint;
begin
  LEndpoint := BackendEndpointUpdateNote;
  LEndpoint.Params.Items[0].Value := ANote.ID;
  LJSONObject := TNoteJSON.NoteToJSON(ANote);
  try
    LEndpoint.ClearBody;
    LEndpoint.AddBody(LJSONObject);
    LEndpoint.Execute;
  finally
    LJSONObject.Free;
  end;
end;

end.
