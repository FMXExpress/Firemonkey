//---------------------------------------------------------------------------
// Copyright (c) 2016 Embarcadero Technologies, Inc. All rights reserved.  
//
// This software is the copyrighted property of Embarcadero Technologies, Inc. 
// ("Embarcadero") and its licensors. You may only use this software if you 
// are an authorized licensee of Delphi, C++Builder or RAD Studio 
// (the "Embarcadero Products").  This software is subject to Embarcadero's 
// standard software license and support agreement that accompanied your 
// purchase of the Embarcadero Products and is considered a Redistributable, 
// as such term is defined thereunder. Your use of this software constitutes 
// your acknowledgement of your agreement to the foregoing software license 
// and support agreement. 
//---------------------------------------------------------------------------
unit ListenerFrameU;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Controls.Presentation, EMSHosting.ExtensionsServices,
  System.JSON, IPPeerClient, REST.Backend.EMSProvider, REST.Backend.Providers,
  EMSHosting.EdgeService, REST.Backend.ServiceTypes, Data.Bind.Components,
  Data.Bind.ObjectScope, REST.Client, REST.Backend.EndPoint, FMX.Layouts,
  System.Actions, FMX.ActnList;

type
  TEMSEdgeModuleListenerFrame = class(TFrame)
    GroupBox2: TGroupBox;
    CheckBoxEdgepointActive: TCheckBox;
    EditListenerHost: TEdit;
    Label1: TLabel;
    EditListenerPort: TEdit;
    Label2: TLabel;
    ButtonTestVersion: TButton;
    BackendEndpointVersion: TBackendEndpoint;
    Layout1: TLayout;
    ActionList1: TActionList;
    ActionActivateEdgeModule: TAction;
    EditModuleName: TEdit;
    Label3: TLabel;
    ButtonGetLocal: TButton;
    procedure ButtonTestVersionClick(Sender: TObject);
    procedure ActionActivateEdgeModuleExecute(Sender: TObject);
    procedure ActionActivateEdgeModuleUpdate(Sender: TObject);
    procedure ButtonGetLocalClick(Sender: TObject);
  private
    FEMSEdgeService: TEMSEdgeService;
    procedure SetEMSEdgeService(const Value: TEMSEdgeService);
    function GetEMSProvider: TEMSProvider;
    procedure EdgeServiceChanged;
    procedure UpdateEdgeService;
    //procedure CheckEdgeService;
    { Private declarations }
    property EMSProvider: TEMSProvider read GetEMSProvider;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    { Public declarations }
    property EMSEdgeService: TEMSEdgeService read FEMSEdgeService write SetEMSEdgeService;
  end;

implementation

{$R *.fmx}

uses REST.Json, EMSHosting.EdgeHttpListener,
REST.Backend.EMSAPI
{$IFDEF MSWINDOWS}
  ,Winapi.WinSock2
{$ENDIF}
{$IFDEF POSIX}
  ,Posix.Unistd
{$ENDIF}
;

function GetLocalHost: string;
var
  LBuffer: TArray<Byte>;
  LLength: Integer;
  LPointer: MarshaledAString;
begin
  SetLength(LBuffer, 256);
  LPointer := @(LBuffer[0]);
  if gethostname(LPointer, 255) = 0 then
  begin
    LLength := System.Length(LPointer);
    Result := TEncoding.Default.GetString(LBuffer, 0, LLength);
  end;
end;

{ TEMSEdgeModuleListenerFrame }

procedure TEMSEdgeModuleListenerFrame.SetEMSEdgeService(
  const Value: TEMSEdgeService);
begin
  if FEMSEdgeService <> Value then
  begin
    if FEMSEdgeService <> nil then
    begin
      FEMSEdgeService.RemoveFreeNotification(Self);
    end;
    FEMSEdgeService := Value;
    if FEMSEdgeService <> nil then
    begin
      FEMSEdgeService.FreeNotification(Self);
    end;
    EdgeServiceChanged;
  end;
end;

procedure TEMSEdgeModuleListenerFrame.ActionActivateEdgeModuleExecute(
  Sender: TObject);
begin
  Assert(FEMSEdgeService <> nil);
  UpdateEdgeService;
  Assert(EMSEdgeService.AutoRegister);
  EMSEdgeservice.Active := not EMSEdgeservice.Active;  // Register or unregister
end;

procedure TEMSEdgeModuleListenerFrame.ActionActivateEdgeModuleUpdate(
  Sender: TObject);
begin
  TAction(Sender).Checked := (FEMSEdgeService <> nil) and  EmsEdgeService.Active;
  TAction(Sender).Enabled := (FEMSEdgeService <> nil) and (EditModuleName.Text <> '');
  EditModuleName.Enabled :=  not ((FEMSEdgeService <> nil) and  EmsEdgeService.Active);
end;

procedure TEMSEdgeModuleListenerFrame.ButtonGetLocalClick(Sender: TObject);
begin
  EditListenerHost.Text := GetLocalHost;
end;

procedure TEMSEdgeModuleListenerFrame.ButtonTestVersionClick(Sender: TObject);
begin
  BackendEndpointVersion.Provider := EMSProvider;
  BackendEndpointVersion.Params[0].Value := EditModuleName.Text;
  BackendEndpointVersion.Execute;
  ShowMessage(TJson.Format(BackendEndpointVersion.Response.JSONValue));
end;

function TEMSEdgeModuleListenerFrame.GetEMSProvider: TEMSProvider;
begin
  if FEMSEdgeService = nil then
    raise Exception.Create('Missing EMSEdgeService');
  if FEMSEdgeService.Provider = nil then
    raise Exception.Create('Missing EMSEdgeService.Provider');
  Result := FEMSEdgeService.Provider as TEMSProvider;
end;

procedure TEMSEdgeModuleListenerFrame.Notification(
  AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  /// clean up component-references
  if (Operation = opRemove) then
  begin
    if FEMSEdgeService = AComponent then
      FEMSEdgeService := nil;
  end;
end;

procedure TEMSEdgeModuleListenerFrame.UpdateEdgeService;
begin
  EMSEdgeService.ModuleName := EditModuleName.Text;
  (EMSEdgeService.ListenerService as TCustomEMSEdgeHTTPListener).Host := EditListenerHost.Text;
  (EMSEdgeService.ListenerService as TCustomEMSEdgeHTTPListener).Port := StrToInt(EditListenerPort.Text);
end;


procedure TEMSEdgeModuleListenerFrame.EdgeServiceChanged;
var
  I: Integer;
begin
  if FEMSEdgeService <> nil then
  begin
    EditListenerPort.Text := (EMSEdgeService.ListenerService as TCustomEMSEdgeHTTPListener).Port.ToString;

    //Application.OnIdle := OnIdle;
    EditListenerHost.Text := (EMSEdgeService.ListenerService as TCustomEMSEdgeHTTPListener).Host;
    EditListenerPort.Text := (EMSEdgeService.ListenerService as TCustomEMSEdgeHTTPListener).Port.ToString;

    // Fill in module parameter for endpoint
    for I := 0 to ComponentCount - 1  do
      if Components[I] is TBackendEndpoint then
        with TBackendEndpoint(Components[I]) do
          if Params.ContainsParameter('module') then
            Params.ParameterByName('module').Value := EMSEdgeService.ModuleName;

    if EditListenerHost.Text = '' then
      // Get host name for this computer
      EditListenerHost.Text := GetLocalHost;
  end;
end;

end.
