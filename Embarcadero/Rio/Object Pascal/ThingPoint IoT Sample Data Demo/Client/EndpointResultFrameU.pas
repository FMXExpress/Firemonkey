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
unit EndpointResultFrameU;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  IPPeerClient, REST.Backend.ServiceTypes, System.JSON,
  REST.Backend.EMSServices, Data.Bind.Components, Data.Bind.ObjectScope,
  REST.Client, REST.Backend.EndPoint, REST.Backend.EMSProvider,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  System.Rtti, Data.Bind.EngExt, Fmx.Bind.DBEngExt, Fmx.Bind.Grid,
  System.Bindings.Outputs, Fmx.Bind.Editors, REST.Response.Adapter,
  Data.Bind.Grid, Data.Bind.DBScope, FMX.Controls.Presentation, FMX.Layouts,
  FMX.Grid, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client, ClientSettingsU,
  System.Actions, FMX.ActnList;

type
  TEMSEndpointResultFrame = class(TFrame)
    FDMemTable1: TFDMemTable;
    StringGrid1: TStringGrid;
    Layout1: TLayout;
    Layout2: TLayout;
    ButtonExecute: TButton;
    ButtonClear: TButton;
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkGridToDataSourceBindSourceDB1: TLinkGridToDataSource;
    RESTResponseDataSetAdapter1: TRESTResponseDataSetAdapter;
    Layout3: TLayout;
    CheckBoxAutoRefresh: TCheckBox;
    Timer1: TTimer;
    ActionList1: TActionList;
    ActionAutoRefresh: TAction;
    procedure ButtonExecuteClick(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
    procedure ActionAutoRefreshExecute(Sender: TObject);
    procedure ActionAutoRefreshUpdate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FBackendEndpoint: TBackendEndpoint;
    FSettingsList: TSettingsList;
    { Private declarations }
    procedure CheckBackendEndpoint;
    procedure SetBackendEndpoint(const Value: TBackendEndpoint);
    procedure BackendEndpointChanged;
    function MakeWidthKey(const AHeader: string): string;
    procedure RestoreGridColumnWidths;
    procedure SaveGridColumnWidths;
    function GetAutoRefresh: Boolean;
    procedure SetAutoRefresh(const Value: Boolean);
    procedure SetSettingsList(const Value: TSettingsList);
    function GetEmpty: Boolean;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Public declarations }
    procedure Execute;
    procedure Clear;
    procedure SaveSettings;
    property BackendEndpoint: TBackendEndpoint read FBackendEndpoint write SetBackendEndpoint;
    property SettingsList: TSettingsList read FSettingsList write SetSettingsList;
    property AutoRefresh: Boolean read GetAutoRefresh write SetAutoRefresh;
    property Empty: Boolean read GetEmpty;
  end;

implementation

{$R *.fmx}

procedure TEMSEndpointResultFrame.ActionAutoRefreshExecute(Sender: TObject);
begin
 AutoRefresh := not AutoRefresh;
end;

procedure TEMSEndpointResultFrame.ActionAutoRefreshUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := AutoRefresh;
end;

procedure TEMSEndpointResultFrame.BackendEndpointChanged;
begin
  if FBackendEndpoint <> nil then
    RESTResponseDataSetAdapter1.ResponseJSON := FBackendEndpoint
  else
    RESTResponseDataSetAdapter1.ResponseJSON := nil;
end;

procedure TEMSEndpointResultFrame.ButtonClearClick(Sender: TObject);
begin
  Clear;
end;

procedure TEMSEndpointResultFrame.ButtonExecuteClick(Sender: TObject);
begin
  Execute;
end;

procedure TEMSEndpointResultFrame.CheckBackendEndpoint;
begin
  if FBackendEndpoint = nil then
    raise Exception.Create('Missing BackendEndpoint');
end;

procedure TEMSEndpointResultFrame.Clear;
begin
  CheckBackendEndpoint;
  AutoRefresh := False;
  SaveGridColumnWidths;
  if BackendEndpoint.Response <> nil then
    BackendEndpoint.Response.ResetToDefaults;
end;

procedure TEMSEndpointResultFrame.Execute;
begin
  CheckBackendEndpoint;
  SaveGridColumnWidths;
  BackendEndpoint.Execute;
  RestoreGridColumnWidths;
end;

function TEMSEndpointResultFrame.GetAutoRefresh: Boolean;
begin
  Result := Timer1.Enabled;
end;

function TEMSEndpointResultFrame.GetEmpty: Boolean;
begin
  Result := StringGrid1.RowCount = 0;
end;

procedure TEMSEndpointResultFrame.SetAutoRefresh(const Value: Boolean);
begin
  if Value then
    CheckBackendEndpoint;
  Timer1.Enabled := Value;
end;

procedure TEMSEndpointResultFrame.SetBackendEndpoint(
  const Value: TBackendEndpoint);
begin
  if FBackendEndpoint <> Value then
  begin
    if FBackendEndpoint <> nil then
    begin
      FBackendEndpoint.RemoveFreeNotification(Self);
    end;
    FBackendEndpoint := Value;
    if FBackendEndpoint <> nil then
    begin
      FBackendEndpoint.FreeNotification(Self);
    end;
    BackendEndpointChanged;
  end;
end;

procedure TEMSEndpointResultFrame.SetSettingsList(const Value: TSettingsList);
begin
  FSettingsList := Value;
  RestoreGridColumnWidths;
end;

procedure TEMSEndpointResultFrame.Timer1Timer(Sender: TObject);
begin
  try
    Execute;
  except
    Timer1.Enabled := False;
    raise;
  end;
end;

procedure TEMSEndpointResultFrame.Notification(
  AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  /// clean up component-references
  if (Operation = opRemove) then
  begin
    if FBackendEndpoint = AComponent then
      FBackendEndpoint := nil;
  end;
end;

function TEMSEndpointResultFrame.MakeWidthKey(const AHeader: string): string;
var
  I, J: Integer;
begin
  Result := Self.Name + '||' + AHeader;
  repeat
    I := Result.IndexOf('[');
    if I >= 0 then
    begin
      J := Result.IndexOf(']', I);
      if J > 0 then
        Result := Result.Substring(0, I) + '||' + Result.Substring(J+1, Length(Result))
      else
        Result := Result.Substring(0, I) + '||';
    end;
  until I < 0;
  if Result.StartsWith('||.') then
    Result := Result.SubString(3);
end;

procedure TEMSEndpointResultFrame.SaveGridColumnWidths;
var
  I: Integer;
  LColumn: TColumn;
  LKey: string;
begin
  for I := 0 to StringGrid1.ColumnCount - 1 do
  begin
    LColumn := StringGrid1.Columns[I];
    LKey := MakeWidthKey(LColumn.Header);
    FSettingsList.AddWidth(LKey, Round(LColumn.Width));
  end;
end;

procedure TEMSEndpointResultFrame.SaveSettings;
begin
  SaveGridColumnWidths;
end;

procedure TEMSEndpointResultFrame.RestoreGridColumnWidths;
var
  I: Integer;
  LColumn: TColumn;
  LKey: string;
  LWidth: Integer;
begin
  for I := 0 to StringGrid1.ColumnCount - 1 do
  begin
    LColumn := StringGrid1.Columns[I];
    LKey := MakeWidthKey(LColumn.Header);
    if FSettingsList.GetWidth(LKey, LWidth) then
      LColumn.Width := LWidth;
  end;
end;


end.
