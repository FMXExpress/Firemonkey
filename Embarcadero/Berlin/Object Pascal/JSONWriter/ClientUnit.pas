//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit ClientUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, IPPeerClient,
  REST.Backend.EMSProvider, REST.Backend.ServiceTypes, System.JSON,
  REST.Backend.EMSServices, System.Rtti, System.Bindings.Outputs,
  Fmx.Bind.Editors, Data.Bind.EngExt, Fmx.Bind.DBEngExt, FMX.StdCtrls,
  Data.Bind.Components, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  Data.Bind.ObjectScope, REST.Client, REST.Backend.EndPoint;

type
  TForm3 = class(TForm)
    EMSProvider1: TEMSProvider;
    BackendEndpoint1: TBackendEndpoint;
    MemoResponseJSONText: TMemo;
    BindingsList1: TBindingsList;
    BTGetResourcesAndEndPointNames: TButton;
    BTSendGetObject: TButton;
    ToolBar1: TToolBar;
    BTSendReadJSON: TButton;
    procedure BTGetResourcesAndEndPointNamesClick(Sender: TObject);
    procedure BTSendGetObjectClick(Sender: TObject);
    procedure BTSendReadJSONClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses
  Rest.JSON, Rest.types,
  System.JSON.Writers,
  System.JSON.Readers,
  System.JSON.Builders,
  System.JSON.Types,
  System.TypInfo;

{$R *.fmx}

procedure TForm3.BTGetResourcesAndEndPointNamesClick(Sender: TObject);
begin
  BackendEndpoint1.Method := TRESTRequestMethod.rmGET;
  BackendEndpoint1.Execute;

  if BackendEndpoint1.Response.JSONValue <> nil then
    MemoResponseJSONText.Text := TJson.Format(BackendEndpoint1.Response.JSONValue);
end;

const 
  sUserName = 'my user name';
  sPassword = 'my password';
procedure TForm3.BTSendGetObjectClick(Sender: TObject);
begin
  // Write Request using JSONObjectBuilder
  BackendEndpoint1.Body.ClearBody;
  with TJSONObjectBuilder.Create(BackendEndpoint1.Body.JSONWriter) do
  try
    BeginObject
      .Add('UserName', sUserName)
      .Add('Password', sPassword);
  finally
    Free;
  end;

  BackendEndpoint1.Method := TRESTRequestMethod.rmPOST;
  BackendEndpoint1.Execute;

  // Read response using JSONValue property
  if BackendEndpoint1.Response.JSONValue <> nil then
    MemoResponseJSONText.Text := TJson.Format(BackendEndpoint1.Response.JSONValue);
end;

procedure TForm3.BTSendReadJSONClick(Sender: TObject);

  procedure LogMessage(const AValue: string);
  begin
    MemoResponseJSONText.Lines.Append(AValue);
  end;

  procedure LogString(const AValue: string);
  begin
    LogMessage('"' + AValue + '"');
  end;

  procedure LogTokenType(ATokenType: TJsonToken);
  begin
    LogMessage('TJsonToken.' + GetEnumName(TypeInfo(TJsonToken), Ord(ATokenType)));
  end;
var
  LReader: TJSONTextReader;
begin
  //Write Request using JSONObjectBuilder
  BackendEndpoint1.Body.ClearBody;
  with TJSONObjectBuilder.Create(BackendEndpoint1.Body.JSONWriter) do
  try
    BeginObject
      .Add('UserName', sUserName)
      .Add('Password', sPassword);
  finally
    Free;
  end;

  BackendEndpoint1.Method := TRESTRequestMethod.rmPOST;
  BackendEndpoint1.Execute;

  // Read response using JSONReader property
  // Expecting JSON like this:
  //   { "UserDetails": ["my user name", "my password] }
  MemoResponseJSONText.Lines.Clear;
  LReader := BackendEndpoint1.Response.JSONReader;

  LReader.Read;
  LogTokenType(LReader.TokenType);
  if LReader.TokenType <> TJsonToken.StartObject then
    raise Exception.Create('Unexpected Token, expected object');

  LReader.Read;
  LogTokenType(LReader.TokenType);
  if LReader.TokenType <> TJsonToken.PropertyName then
    raise Exception.Create('Unexpected Token, expected property name');
  LogString(LReader.Value.AsString);

  LReader.Read;
  LogTokenType(LReader.TokenType);
  if LReader.TokenType <> TJsonToken.StartArray then
    raise Exception.Create('Unexpected Token, expected array');

  // Read array of strings
  while LReader.Read do
  begin
    LogTokenType(LReader.TokenType);
    if LReader.TokenType = TJsonToken.EndArray then
      break;
    if LReader.TokenType <> TJsonToken.String then
      raise Exception.Create('Unexpected Token, expected string');
    LogString(LReader.Value.AsString);
  end;
end;

end.
